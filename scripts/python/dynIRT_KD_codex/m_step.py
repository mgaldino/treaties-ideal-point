import numpy as np


def m_step_items(y_star, rc, x_smooth, P_smooth, bill_session, beta_mu, beta_sigma, voters_by_item=None):
    """
    Update item parameters (alpha, beta) via penalized WLS.
    """
    rc = np.asarray(rc)
    y_star = np.asarray(y_star, dtype=float)
    x_smooth = np.asarray(x_smooth, dtype=float)
    bill_session = np.asarray(bill_session, dtype=int)
    beta_mu = np.asarray(beta_mu, dtype=float)
    beta_sigma = np.asarray(beta_sigma, dtype=float)

    N, J = rc.shape
    K = x_smooth.shape[1]
    Kp1 = K + 1

    alpha_new = np.zeros(J, dtype=float)
    beta_new = np.zeros((J, K), dtype=float)

    Sigma_beta_inv = np.linalg.inv(beta_sigma)
    Sigma_beta_inv_mu = Sigma_beta_inv @ beta_mu

    if voters_by_item is None:
        voters_by_item = [np.nonzero(rc[:, j] != 0)[0] for j in range(J)]

    for j in range(J):
        voters = voters_by_item[j]
        if len(voters) == 0:
            continue

        t_j = int(bill_session[j])

        X = x_smooth[voters, :, t_j]
        Y = y_star[voters, j]

        P_sum = np.zeros((K, K), dtype=float)
        for i in voters:
            P_sum += P_smooth[i][t_j]

        count = len(voters)
        sum_x = X.sum(axis=0)
        sum_y = float(np.sum(Y))
        sum_yx = X.T @ Y
        sum_xx = X.T @ X

        Sigma_zz = np.zeros((Kp1, Kp1), dtype=float)
        Sigma_zz[0, 0] = count
        Sigma_zz[0, 1:Kp1] = sum_x
        Sigma_zz[1:Kp1, 0] = sum_x
        Sigma_zz[1:Kp1, 1:Kp1] = P_sum + sum_xx

        Sigma_zy = np.concatenate(([sum_y], sum_yx))

        A = Sigma_beta_inv + Sigma_zz
        b = Sigma_beta_inv_mu + Sigma_zy
        gamma_hat = np.linalg.solve(A, b)

        alpha_new[j] = gamma_hat[0]
        beta_new[j, :] = gamma_hat[1:Kp1]

    return {"alpha": alpha_new, "beta": beta_new}


def m_step_Omega(x_smooth, P_smooth, P_lag, startlegis, endlegis, diagonal_only=False, ridge=1e-6):
    """
    Update evolution covariance Omega from smoothed states.
    """
    x_smooth = np.asarray(x_smooth, dtype=float)
    startlegis = np.asarray(startlegis, dtype=int)
    endlegis = np.asarray(endlegis, dtype=int)

    N = x_smooth.shape[0]
    K = x_smooth.shape[1]

    Omega_sum = np.zeros((K, K), dtype=float)
    total_transitions = 0

    for i in range(N):
        si = int(startlegis[i])
        ei = int(endlegis[i])
        T_active = ei - si + 1
        if T_active < 2:
            continue

        for s in range(1, T_active):
            t_curr = si + s
            t_prev = si + s - 1

            x_curr = x_smooth[i, :, t_curr]
            x_prev = x_smooth[i, :, t_prev]
            dx = x_curr - x_prev

            P_curr = P_smooth[i][t_curr]
            P_prev = P_smooth[i][t_prev]
            P_cross = P_lag[i][s - 1]

            E_innov = np.outer(dx, dx) + P_curr - P_cross - P_cross.T + P_prev

            Omega_sum += E_innov
            total_transitions += 1

    if total_transitions == 0:
        return ridge * np.eye(K)

    Omega_hat = Omega_sum / total_transitions
    if diagonal_only:
        Omega_hat = np.diag(np.diag(Omega_hat))

    Omega_hat = Omega_hat + ridge * np.eye(K)
    Omega_hat = 0.5 * (Omega_hat + Omega_hat.T)

    return Omega_hat
