import numpy as np
from numpy.linalg import LinAlgError


def kalman_smoother_country(
    y_star_i,
    item_indices,
    alpha,
    beta,
    bill_session,
    mu0,
    Sigma0,
    Omega,
    start_t,
    end_t,
    T_total,
):
    """
    Kalman filter + RTS smoother for a single country.

    Returns x_smooth (K x T_active), P_smooth (list of KxK), P_lag (list of KxK),
    x_pred (K x T_active), P_pred (list of KxK).
    """
    alpha = np.asarray(alpha, dtype=float)
    beta = np.asarray(beta, dtype=float)
    bill_session = np.asarray(bill_session, dtype=int)
    mu0 = np.asarray(mu0, dtype=float)
    Sigma0 = np.asarray(Sigma0, dtype=float)
    Omega = np.asarray(Omega, dtype=float)

    K = mu0.size
    T_active = int(end_t - start_t + 1)
    I_K = np.eye(K)

    items_by_period = [[] for _ in range(T_active)]
    if len(item_indices) > 0:
        item_indices = np.asarray(item_indices, dtype=int)
        y_star_i = np.asarray(y_star_i, dtype=float)
        local_t = bill_session[item_indices] - start_t
        valid = (local_t >= 0) & (local_t < T_active)
        if not np.all(valid):
            item_indices = item_indices[valid]
            y_star_i = y_star_i[valid]
            local_t = local_t[valid]
        if len(item_indices) > 0:
            for pos, t_loc in enumerate(local_t):
                items_by_period[int(t_loc)].append(pos)

    x_filt = np.zeros((K, T_active), dtype=float)
    P_filt = [None] * T_active
    x_pred_store = np.zeros((K, T_active), dtype=float)
    P_pred_store = [None] * T_active

    # ---- Forward pass ----
    for s in range(T_active):
        if s == 0:
            x_hat = mu0.copy()
            P_hat = Sigma0.copy()
        else:
            x_hat = x_filt[:, s - 1].copy()
            P_hat = P_filt[s - 1] + Omega

        x_pred_store[:, s] = x_hat
        P_pred_store[s] = P_hat

        obs_idx = items_by_period[s]
        if len(obs_idx) > 0:
            if len(obs_idx) == 1:
                j = item_indices[obs_idx[0]]
                beta_j = beta[j, :]

                innovation = y_star_i[obs_idx[0]] - alpha[j] - np.dot(beta_j, x_hat)
                S = float(beta_j.T @ (P_hat @ beta_j)) + 1.0
                K_gain = (P_hat @ beta_j) / S

                x_hat = x_hat + K_gain * innovation

                IKb = I_K - np.outer(K_gain, beta_j)
                P_hat = IKb @ P_hat @ IKb.T + np.outer(K_gain, K_gain)
                P_hat = 0.5 * (P_hat + P_hat.T)
            else:
                obs_items = item_indices[np.asarray(obs_idx, dtype=int)]
                H = beta[obs_items, :]
                y_t = y_star_i[np.asarray(obs_idx, dtype=int)]
                alpha_t = alpha[obs_items]
                y_t = y_t - alpha_t

                P_inv = np.linalg.inv(P_hat)
                A = P_inv + H.T @ H
                rhs = P_inv @ x_hat + H.T @ y_t

                try:
                    chol_A = np.linalg.cholesky(A)
                    x_hat = np.linalg.solve(chol_A.T, np.linalg.solve(chol_A, rhs))
                    P_hat = np.linalg.solve(chol_A.T, np.linalg.solve(chol_A, np.eye(K)))
                except LinAlgError:
                    x_hat = np.linalg.solve(A, rhs)
                    P_hat = np.linalg.solve(A, np.eye(K))

                P_hat = 0.5 * (P_hat + P_hat.T)

        x_filt[:, s] = x_hat
        P_filt[s] = P_hat

    # ---- Backward pass (RTS) ----
    x_smooth = np.zeros((K, T_active), dtype=float)
    P_smooth = [None] * T_active
    L_store = [None] * T_active

    x_smooth[:, -1] = x_filt[:, -1]
    P_smooth[-1] = P_filt[-1]

    if T_active > 1:
        for s in range(T_active - 2, -1, -1):
            P_pred_next = P_pred_store[s + 1]
            try:
                P_pred_inv = np.linalg.solve(P_pred_next, np.eye(K))
            except LinAlgError:
                P_pred_inv = np.linalg.pinv(P_pred_next)
            L_t = P_filt[s] @ P_pred_inv

            x_smooth[:, s] = x_filt[:, s] + L_t @ (x_smooth[:, s + 1] - x_pred_store[:, s + 1])
            P_smooth[s] = P_filt[s] + L_t @ (P_smooth[s + 1] - P_pred_next) @ L_t.T
            P_smooth[s] = 0.5 * (P_smooth[s] + P_smooth[s].T)

            L_store[s] = L_t

    # ---- Lag-one cross-covariance ----
    P_lag = []
    if T_active > 1:
        for s in range(1, T_active):
            P_lag.append(L_store[s - 1] @ P_smooth[s])

    return {
        "x_smooth": x_smooth,
        "P_smooth": P_smooth,
        "P_lag": P_lag,
        "x_pred": x_pred_store,
        "P_pred": P_pred_store,
    }
