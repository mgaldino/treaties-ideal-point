import numpy as np

def kalman_smoother_country(y_star_i, item_indices, alpha, beta,
                            bill_session, mu0, Sigma0, Omega,
                            start_t, end_t, T_total):
    """
    Kalman Filter-Smoother for a single country.
    """
    K = len(mu0)
    T_active = end_t - start_t + 1
    I_K = np.eye(K)
    
    # Pre-group items by period
    items_by_period = [[] for _ in range(T_active)]
    if len(item_indices) > 0:
        local_t = bill_session[item_indices] - start_t
        valid = (local_t >= 0) & (local_t < T_active)

        if not np.all(valid):
            item_indices = item_indices[valid]
            y_star_i = y_star_i[valid]
            local_t = local_t[valid]

        for idx, t in enumerate(local_t):
            items_by_period[int(t)].append(idx)
            
    # Storage
    x_filt = np.zeros((K, T_active))
    P_filt = [None] * T_active
    x_pred_store = np.zeros((K, T_active))
    P_pred_store = [None] * T_active
    
    # ---- Forward Pass ----
    for s in range(T_active):
        if s == 0:
            x_hat = mu0.copy()
            P_hat = Sigma0.copy()
        else:
            x_hat = x_filt[:, s-1]
            P_hat = P_filt[s-1] + Omega
            
        x_pred_store[:, s] = x_hat
        P_pred_store[s] = P_hat
        
        # Update
        obs_idx = items_by_period[s]
        if len(obs_idx) > 0:
            if len(obs_idx) == 1:
                # Fast path
                idx = obs_idx[0]
                j = item_indices[idx]
                beta_j = beta[j]
                
                innovation = y_star_i[idx] - alpha[j] - np.dot(beta_j, x_hat)
                S = np.dot(beta_j, np.dot(P_hat, beta_j)) + 1.0
                K_gain = np.dot(P_hat, beta_j) / S
                
                x_hat = x_hat + K_gain * innovation
                
                IKb = I_K - np.outer(K_gain, beta_j)
                P_hat = IKb @ P_hat @ IKb.T + np.outer(K_gain, K_gain)
                P_hat = (P_hat + P_hat.T) / 2
                
            else:
                # Block update
                obs_items = item_indices[np.array(obs_idx, dtype=int)]
                H = beta[obs_items]
                y_t = y_star_i[obs_idx]
                alpha_t = alpha[obs_items]
                y_t = y_t - alpha_t
                
                try:
                    P_inv = np.linalg.solve(P_hat, I_K)
                except np.linalg.LinAlgError:
                    P_inv = np.linalg.inv(P_hat)
                    
                A = P_inv + H.T @ H
                rhs = P_inv @ x_hat + H.T @ y_t
                
                try:
                    chol_A = np.linalg.cholesky(A)
                    x_hat = np.linalg.solve(chol_A, rhs)
                    x_hat = np.linalg.solve(chol_A.T, x_hat)
                    invA = np.linalg.solve(chol_A, I_K)
                    P_hat = np.linalg.solve(chol_A.T, invA)
                except np.linalg.LinAlgError:
                    x_hat = np.linalg.solve(A, rhs)
                    P_hat = np.linalg.solve(A, I_K)
                    
                P_hat = (P_hat + P_hat.T) / 2
        
        x_filt[:, s] = x_hat
        P_filt[s] = P_hat

    # ---- Backward Pass ----
    x_smooth = np.zeros((K, T_active))
    P_smooth = [None] * T_active
    L_store = [None] * T_active
    
    x_smooth[:, T_active-1] = x_filt[:, T_active-1]
    P_smooth[T_active-1] = P_filt[T_active-1]
    
    if T_active > 1:
        for s in range(T_active - 2, -1, -1):
            P_pred_next = P_pred_store[s+1]
            
            try:
                L_t = np.linalg.solve(P_pred_next.T, P_filt[s].T).T
            except np.linalg.LinAlgError:
                L_t = P_filt[s] @ np.linalg.inv(P_pred_next)
                
            x_smooth[:, s] = x_filt[:, s] + L_t @ (x_smooth[:, s+1] - x_pred_store[:, s+1])
            
            P_diff = P_smooth[s+1] - P_pred_next
            P_smooth[s] = P_filt[s] + L_t @ P_diff @ L_t.T
            P_smooth[s] = (P_smooth[s] + P_smooth[s].T) / 2
            
            L_store[s] = L_t
            
    # ---- Lag-one Covariance ----
    P_lag = [None] * max(T_active - 1, 0)
    if T_active > 1:
        for s in range(1, T_active):
            P_lag[s-1] = L_store[s-1] @ P_smooth[s]
            
    return {
        'x_smooth': x_smooth,
        'P_smooth': P_smooth,
        'P_lag': P_lag,
        'x_pred': x_pred_store,
        'P_pred': P_pred_store
    }
