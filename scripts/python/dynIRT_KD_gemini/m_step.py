import numpy as np

def m_step_items(y_star, rc, x_smooth, P_smooth,
                 bill_session, beta_mu, beta_sigma,
                 voters_by_item=None):
    """
    M-Step: Update item parameters.
    
    Args:
        y_star: N x J matrix.
        rc: N x J matrix (original votes).
        x_smooth: N x K x T array.
        P_smooth: List (len N) of List (len T) of KxK matrices.
        bill_session: J-vector.
        beta_mu: (K+1)-vector.
        beta_sigma: (K+1)x(K+1) matrix.
        voters_by_item: Optional precomputed list of voters.
        
    Returns:
        tuple: (alpha, beta)
    """
    N, J = rc.shape
    K = x_smooth.shape[1]
    Kp1 = K + 1
    
    alpha_new = np.zeros(J)
    beta_new = np.zeros((J, K))
    
    # Prior precision
    Sigma_beta_inv = np.linalg.inv(beta_sigma)
    Sigma_beta_inv_mu = Sigma_beta_inv @ beta_mu
    
    if voters_by_item is None:
        voters_by_item = [np.where(rc[:, j] != 0)[0] for j in range(J)]
        
    for j in range(J):
        voters = voters_by_item[j]
        if len(voters) == 0:
            continue
            
        t_j = int(bill_session[j])
        
        # X: (n_voters, K)
        X = x_smooth[voters, :, t_j]
        Y = y_star[voters, j]
        
        # Sum of P_it
        P_sum = np.zeros((K, K))
        for idx in voters:
            # P_smooth[idx][t_j] should be KxK
            P_sum += P_smooth[idx][t_j]
            
        count = len(voters)
        sum_x = np.sum(X, axis=0)
        sum_y = np.sum(Y)
        sum_yx = X.T @ Y
        sum_xx = X.T @ X
        
        Sigma_zz = np.zeros((Kp1, Kp1))
        Sigma_zz[0, 0] = count
        Sigma_zz[0, 1:] = sum_x
        Sigma_zz[1:, 0] = sum_x
        Sigma_zz[1:, 1:] = P_sum + sum_xx
        
        Sigma_zy = np.concatenate(([sum_y], sum_yx))
        
        # MAP estimate
        A = Sigma_beta_inv + Sigma_zz
        b = Sigma_beta_inv_mu + Sigma_zy
        
        gamma_hat = np.linalg.solve(A, b)
            
        alpha_new[j] = gamma_hat[0]
        beta_new[j, :] = gamma_hat[1:]
        
    return alpha_new, beta_new

def m_step_Omega(x_smooth, P_smooth, P_lag,
                 startlegis, endlegis,
                 diagonal_only=False, ridge=1e-6):
    """
    M-Step: Update Omega.
    """
    N, K, T = x_smooth.shape
    
    Omega_sum = np.zeros((K, K))
    total_transitions = 0
    
    for i in range(N):
        si = startlegis[i]
        ei = endlegis[i]
        T_active_i = ei - si + 1
        
        if T_active_i < 2:
            continue
            
        # s ranges from 1 to T_active_i - 1 (local transition index)
        # s=1 corresponds to transition from si to si+1
        for s in range(1, T_active_i):
            t_curr = si + s
            t_prev = si + s - 1
            
            x_curr = x_smooth[i, :, t_curr]
            x_prev = x_smooth[i, :, t_prev]
            dx = x_curr - x_prev
            
            P_curr = P_smooth[i][t_curr]
            P_prev = P_smooth[i][t_prev]
            P_cross = P_lag[i][s-1] # List of transitions
            
            # E_innov = dx dx' + P_curr - P_cross - P_cross' + P_prev
            E_innov = np.outer(dx, dx) + P_curr - P_cross - P_cross.T + P_prev
            
            Omega_sum += E_innov
            total_transitions += 1
            
    if total_transitions == 0:
        return ridge * np.eye(K)
        
    Omega_hat = Omega_sum / total_transitions
    
    if diagonal_only:
        Omega_hat = np.diag(np.diag(Omega_hat))
        
    Omega_hat = Omega_hat + ridge * np.eye(K)
    Omega_hat = (Omega_hat + Omega_hat.T) / 2
    
    return Omega_hat
