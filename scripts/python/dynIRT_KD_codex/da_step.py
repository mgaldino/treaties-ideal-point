import numpy as np

from stats_utils import logcdf, logpdf, logsf


def truncnorm_moments(mu, y):
    """
    Truncated normal moments for probit data augmentation.

    For y = +1: y* ~ TN(mu, 1, 0, +Inf)
    For y = -1: y* ~ TN(mu, 1, -Inf, 0)
    """
    mu = np.asarray(mu, dtype=float)
    y = np.asarray(y, dtype=int)

    n = mu.size
    y_star = np.full(n, np.nan, dtype=float)
    y_star_var = np.full(n, np.nan, dtype=float)

    log_phi = logpdf(mu)

    pos = y == 1
    if np.any(pos):
        mu_pos = mu[pos]
        log_phi_pos = log_phi[pos]
        log_Phi_pos = logcdf(mu_pos)
        lambda_pos = np.exp(log_phi_pos - log_Phi_pos)
        y_star[pos] = mu_pos + lambda_pos
        y_star_var[pos] = 1.0 - lambda_pos * (lambda_pos + mu_pos)

    neg = y == -1
    if np.any(neg):
        mu_neg = mu[neg]
        log_phi_neg = log_phi[neg]
        log_1mPhi_neg = logsf(mu_neg)
        lambda_neg = np.exp(log_phi_neg - log_1mPhi_neg)
        y_star[neg] = mu_neg - lambda_neg
        y_star_var[neg] = 1.0 - lambda_neg * (lambda_neg - mu_neg)

    y_star_var = np.clip(y_star_var, 1e-12, 1.0)

    return y_star, y_star_var


def da_step(rc, alpha, beta, x_smooth, bill_session):
    """
    Data augmentation step for all non-missing votes.

    Returns y_star and y_star_var as N x J matrices (NaN for missing).
    """
    rc = np.asarray(rc)
    alpha = np.asarray(alpha, dtype=float)
    beta = np.asarray(beta, dtype=float)
    x_smooth = np.asarray(x_smooth, dtype=float)
    bill_session = np.asarray(bill_session, dtype=int)

    N, J = rc.shape
    K = beta.shape[1]

    y_star = np.full((N, J), np.nan, dtype=float)
    y_star_var = np.full((N, J), np.nan, dtype=float)

    periods = np.unique(bill_session)
    periods.sort()

    for tt in periods:
        j_idx = np.where(bill_session == tt)[0]
        if j_idx.size == 0:
            continue

        x_t = x_smooth[:, :, tt]
        mu_mat = x_t @ beta[j_idx, :].T
        mu_mat += alpha[j_idx]

        rc_sub = rc[:, j_idx]
        nonmiss_mask = rc_sub != 0
        if np.any(nonmiss_mask):
            mu_vec = mu_mat[nonmiss_mask]
            y_vec = rc_sub[nonmiss_mask]
            y_m, y_v = truncnorm_moments(mu_vec, y_vec)

            tmp_ystar = np.full(rc_sub.shape, np.nan, dtype=float)
            tmp_ystar_var = np.full(rc_sub.shape, np.nan, dtype=float)
            tmp_ystar[nonmiss_mask] = y_m
            tmp_ystar_var[nonmiss_mask] = y_v

            y_star[:, j_idx] = tmp_ystar
            y_star_var[:, j_idx] = tmp_ystar_var

    return y_star, y_star_var
