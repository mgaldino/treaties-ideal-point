import numpy as np
import math

try:
    from scipy.stats import norm
    def norm_logpdf(x):
        return norm.logpdf(x)
    def norm_logcdf(x):
        return norm.logcdf(x)
    def norm_logsf(x):
        return norm.logsf(x)
except ImportError:
    _vec_erfc = np.vectorize(math.erfc, otypes=[float])
    def norm_logpdf(x):
        x = np.asarray(x, dtype=float)
        return -0.5 * x**2 - 0.5 * np.log(2 * np.pi)
    def norm_logcdf(x):
        x = np.asarray(x, dtype=float)
        return np.log(0.5 * _vec_erfc(-x / np.sqrt(2)))
    def norm_logsf(x):
        x = np.asarray(x, dtype=float)
        return np.log(0.5 * _vec_erfc(x / np.sqrt(2)))

def truncnorm_moments(mu, y):
    """
    Computes E[y* | y] and Var[y* | y] for truncated normal moments.
    """
    mu = np.asarray(mu, dtype=float)
    y = np.asarray(y)
    y_star = np.zeros_like(mu, dtype=float)
    y_star_var = np.zeros_like(mu, dtype=float)
    
    # Log-space computation
    log_phi = norm_logpdf(mu)
    
    # Positive votes: y* ~ TN(mu, 1, 0, +Inf)
    pos = (y == 1)
    if np.any(pos):
        mu_pos = mu[pos]
        log_phi_pos = log_phi[pos]
        log_Phi_pos = norm_logcdf(mu_pos)
        
        # lambda = phi(mu) / Phi(mu)
        # exp(log_phi - log_Phi)
        lambda_pos = np.exp(log_phi_pos - log_Phi_pos)
        y_star[pos] = mu_pos + lambda_pos
        y_star_var[pos] = 1 - lambda_pos * (lambda_pos + mu_pos)
        
    # Negative votes: y* ~ TN(mu, 1, -Inf, 0)
    neg = (y == -1)
    if np.any(neg):
        mu_neg = mu[neg]
        log_phi_neg = log_phi[neg]
        # log(1 - Phi(mu)) = logsf(mu)
        log_1mPhi_neg = norm_logsf(mu_neg)
        
        # lambda = phi(mu) / (1 - Phi(mu))
        lambda_neg = np.exp(log_phi_neg - log_1mPhi_neg)
        y_star[neg] = mu_neg - lambda_neg
        y_star_var[neg] = 1 - lambda_neg * (lambda_neg - mu_neg)
        
    # Clamp variance
    y_star_var = np.clip(y_star_var, 1e-12, 1.0)
    
    return y_star, y_star_var

def da_step(rc, alpha, beta, x_smooth, bill_session):
    N, J = rc.shape
    K = beta.shape[1]
    
    y_star = np.full((N, J), np.nan)
    y_star_var = np.full((N, J), np.nan)
    
    periods = np.unique(bill_session)
    
    for tt in periods:
        j_idx = np.where(bill_session == tt)[0]
        if len(j_idx) == 0:
            continue
            
        x_t = x_smooth[:, :, tt]
        mu_mat = x_t @ beta[j_idx].T
        mu_mat = mu_mat + alpha[j_idx]
        
        rc_sub = rc[:, j_idx]
        
        nonmiss_mask = (rc_sub != 0)
        if np.any(nonmiss_mask):
            mu_flat = mu_mat[nonmiss_mask]
            rc_flat = rc_sub[nonmiss_mask]
            
            moments_y, moments_var = truncnorm_moments(mu_flat, rc_flat)
            
            tmp_ystar = np.full((N, len(j_idx)), np.nan)
            tmp_ystar_var = np.full((N, len(j_idx)), np.nan)
            
            tmp_ystar[nonmiss_mask] = moments_y
            tmp_ystar_var[nonmiss_mask] = moments_var
            
            y_star[:, j_idx] = tmp_ystar
            y_star_var[:, j_idx] = tmp_ystar_var
            
    return y_star, y_star_var
