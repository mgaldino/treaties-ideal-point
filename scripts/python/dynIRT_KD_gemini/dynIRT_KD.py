import numpy as np
import math
import time
import sys

try:
    from scipy.stats import norm
    def norm_logcdf(x):
        return norm.logcdf(x)
    def norm_logsf(x):
        return norm.logsf(x)
except ImportError:
    _vec_erfc = np.vectorize(math.erfc, otypes=[float])
    def norm_logcdf(x):
        x = np.asarray(x, dtype=float)
        return np.log(0.5 * _vec_erfc(-x / np.sqrt(2)))
    def norm_logsf(x):
        x = np.asarray(x, dtype=float)
        return np.log(0.5 * _vec_erfc(x / np.sqrt(2)))

# Import local modules (assuming sys.path is set correctly by caller)
try:
    from da_step import da_step
    from kalman import kalman_smoother_country
    from m_step import m_step_items, m_step_Omega
except ImportError:
    # Fallback for relative import if run as package
    from .da_step import da_step
    from .kalman import kalman_smoother_country
    from .m_step import m_step_items, m_step_Omega

def compute_loglik(rc, alpha, beta, x_smooth, bill_session):
    """Compute observed-data log-likelihood."""
    N, J = rc.shape
    K = beta.shape[1]
    ll = 0.0
    
    periods = np.unique(bill_session)
    
    for tt in periods:
        j_idx = np.where(bill_session == tt)[0]
        if len(j_idx) == 0:
            continue
            
        x_t = x_smooth[:, :, tt] # N x K
        
        # Linear predictor
        mu_mat = x_t @ beta[j_idx].T + alpha[j_idx]
        
        rc_sub = rc[:, j_idx]
        
        # Positive votes
        pos_mask = (rc_sub == 1)
        if np.any(pos_mask):
            ll += np.sum(norm_logcdf(mu_mat[pos_mask]))
            
        # Negative votes
        neg_mask = (rc_sub == -1)
        if np.any(neg_mask):
            ll += np.sum(norm_logsf(mu_mat[neg_mask]))
            
    return ll

def param_change(alpha_old, alpha_new, beta_old, beta_new, x_old, x_new):
    """Max absolute parameter change."""
    d_alpha = np.max(np.abs(alpha_new - alpha_old))
    d_beta = np.max(np.abs(beta_new - beta_old))
    d_x = np.max(np.abs(x_new - x_old))
    return max(d_alpha, d_beta, d_x)

def _kalman_one_country(i, da_y_star, rc, alpha, beta, bill_session,
                        x_mu0, x_Sigma0, Omega, startlegis, endlegis,
                        T_total, K, x_prev_i):
    """Helper for single country Kalman filter."""
    si = startlegis[i]
    ei = endlegis[i]
    
    obs_items = np.where(rc[i, :] != 0)[0]
    
    # Initialize output structures
    x_smooth_i = x_prev_i.copy() # K x T
    P_smooth_i = [None] * T_total
    
    if len(obs_items) == 0:
        # No observations: random walk from prior
        P_cov = x_Sigma0[i]
        for tt in range(T_total):
            if tt == 0:
                x_smooth_i[:, tt] = x_mu0[i]
                P_smooth_i[tt] = P_cov.copy()
            else:
                x_smooth_i[:, tt] = x_smooth_i[:, tt-1]
                P_cov = P_cov + Omega
                P_smooth_i[tt] = P_cov.copy()
                
        return {
            'x_smooth_i': x_smooth_i,
            'P_smooth_i': P_smooth_i,
            'P_lag_i': [None] * max(T_total - 1, 0)
        }
    
    y_star_i = da_y_star[i, obs_items]
    
    ks = kalman_smoother_country(
        y_star_i=y_star_i,
        item_indices=obs_items,
        alpha=alpha,
        beta=beta,
        bill_session=bill_session,
        mu0=x_mu0[i],
        Sigma0=x_Sigma0[i],
        Omega=Omega,
        start_t=si,
        end_t=ei,
        T_total=T_total
    )
    
    # Map back to global timeline
    T_active_i = ei - si + 1
    for s in range(T_active_i):
        t_global = si + s
        x_smooth_i[:, t_global] = ks['x_smooth'][:, s]
        P_smooth_i[t_global] = ks['P_smooth'][s]
        
    # Fill missing/None periods
    for tt in range(T_total):
        if P_smooth_i[tt] is None:
            P_smooth_i[tt] = np.eye(K) * 100.0

        if np.any(np.isnan(x_smooth_i[:, tt])):
            if tt > 0:
                x_smooth_i[:, tt] = x_smooth_i[:, tt - 1]
            else:
                x_smooth_i[:, tt] = x_mu0[i]

    return {
        'x_smooth_i': x_smooth_i,
        'P_smooth_i': P_smooth_i,
        'P_lag_i': ks['P_lag']
    }

def dynIRT_KD(data, starts, priors, control, K=2):
    """
    Main EM Loop.
    """
    # Unpack inputs
    rc = data['rc']
    startlegis = data['startlegis'].flatten()
    endlegis = data['endlegis'].flatten()
    bill_session = data['bill_session'].flatten()
    T_total = int(data['T'])
    
    N, J = rc.shape
    
    alpha = starts['alpha'].flatten()
    beta = starts['beta'] # J x K
    x_smooth = starts['x'] # N x K x T
    
    x_mu0 = priors['x_mu0'] # N x K
    x_Sigma0 = priors['x_Sigma0'] # List of KxK or N x K diag
    beta_mu = priors['beta_mu'].flatten()
    beta_sigma = priors['beta_sigma']
    Omega = priors['omega']
    
    # Normalize x_Sigma0 to list of matrices
    if isinstance(x_Sigma0, np.ndarray) and x_Sigma0.ndim == 2:
        # It's N x K diagonal variances
        tmp = []
        for i in range(N):
            tmp.append(np.diag(x_Sigma0[i]))
        x_Sigma0 = tmp
        
    # Control
    verbose = bool(control.get('verbose', False))
    thresh = control.get('thresh', 1e-6)
    maxit = int(control.get('maxit', 500))
    checkfreq = int(control.get('checkfreq', 50))
    estimate_omega = bool(control.get('estimate_omega', False))
    diagonal_omega = bool(control.get('diagonal_omega', False))
    thresh_loglik = control.get('thresh_loglik', None)
    loglik_patience = int(control.get('loglik_patience', 3))
    thresh_aitken = control.get('thresh_aitken', None)
    
    loglik_trace = np.zeros(maxit, dtype=float)
    loglik_streak = 0
    aitken_iter = None
    t_start = time.time()
    conv = 0
    ncores_used = 1
    
    # Precompute voters
    voters_by_item = [np.where(rc[:, j] != 0)[0] for j in range(J)]
    
    for m in range(maxit):
        iter_num = m + 1
        alpha_old = alpha.copy()
        beta_old = beta.copy()
        x_old = x_smooth.copy()
        
        # ==== E-STEP ====
        # (a) DA Step
        y_star, y_star_var = da_step(rc, alpha, beta, x_smooth, bill_session)
        
        # (b) Kalman
        P_smooth_all = [None] * N
        P_lag_all = [None] * N
        
        # Sequential processing (no parallel for simplicity/safety in validation)
        for i in range(N):
            res_i = _kalman_one_country(
                i=i,
                da_y_star=y_star,
                rc=rc,
                alpha=alpha,
                beta=beta,
                bill_session=bill_session,
                x_mu0=x_mu0,
                x_Sigma0=x_Sigma0,
                Omega=Omega,
                startlegis=startlegis,
                endlegis=endlegis,
                T_total=T_total,
                K=K,
                x_prev_i=x_smooth[i].copy()
            )
            
            x_smooth[i] = res_i['x_smooth_i']
            P_smooth_all[i] = res_i['P_smooth_i']
            P_lag_all[i] = res_i['P_lag_i']
            
        # ==== M-STEP ====
        # (a) Items
        alpha, beta = m_step_items(
            y_star=y_star,
            rc=rc,
            x_smooth=x_smooth,
            P_smooth=P_smooth_all,
            bill_session=bill_session,
            beta_mu=beta_mu,
            beta_sigma=beta_sigma,
            voters_by_item=voters_by_item
        )
        
        # (b) Omega
        if estimate_omega:
            Omega = m_step_Omega(
                x_smooth=x_smooth,
                P_smooth=P_smooth_all,
                P_lag=P_lag_all,
                startlegis=startlegis,
                endlegis=endlegis,
                diagonal_only=diagonal_omega
            )
            
        # Check convergence
        delta = param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

        need_ll = False
        if iter_num == 1:
            need_ll = True
        if verbose and (iter_num == 1 or iter_num % checkfreq == 0):
            need_ll = True
        if thresh_aitken is not None:
            need_ll = True
        if thresh_loglik is not None:
            need_ll = True
        if delta < thresh:
            need_ll = True

        if need_ll:
            ll = compute_loglik(rc, alpha, beta, x_smooth, bill_session)
            loglik_trace[m] = ll
        else:
            loglik_trace[m] = loglik_trace[m - 1]

        if verbose and (iter_num == 1 or iter_num % checkfreq == 0):
            print(
                f"Iter {iter_num:4d} | loglik = {loglik_trace[m]:.4f} | "
                f"delta = {delta:.2e} | Omega[1,1] = {Omega[0, 0]:.4f}"
            )

        if delta < thresh:
            conv = 1
            if verbose:
                print(f"Converged at iter {iter_num} (delta = {delta:.2e})")
            loglik_trace = loglik_trace[:iter_num]
            break

        if thresh_loglik is not None and iter_num >= 2:
            ll_m = loglik_trace[m]
            ll_m1 = loglik_trace[m - 1]
            rel_change = abs(ll_m - ll_m1) / (1 + abs(ll_m1))
            if np.isfinite(rel_change) and rel_change < thresh_loglik:
                loglik_streak += 1
            else:
                loglik_streak = 0
            if loglik_streak >= loglik_patience:
                conv = 1
                if verbose:
                    print(
                        "Loglik converged at iteration "
                        f"{iter_num} (rel_change = {rel_change:.2e}, "
                        f"patience = {loglik_patience})"
                    )
                loglik_trace = loglik_trace[:iter_num]
                break

        if thresh_aitken is not None and iter_num >= 3:
            ll_m = loglik_trace[m]
            ll_m1 = loglik_trace[m - 1]
            ll_m2 = loglik_trace[m - 2]
            denom = ll_m1 - ll_m2
            if (
                np.isfinite(ll_m)
                and np.isfinite(ll_m1)
                and np.isfinite(ll_m2)
                and abs(denom) > np.finfo(float).eps
            ):
                a_m = (ll_m - ll_m1) / denom
                if np.isfinite(a_m) and 0 < a_m < 1:
                    l_inf = ll_m1 + (ll_m - ll_m1) / (1 - a_m)
                    if np.isfinite(l_inf) and abs(l_inf - ll_m) < thresh_aitken:
                        aitken_iter = iter_num
                        conv = 1
                        if verbose:
                            print(
                                "Aitken converged at iteration "
                                f"{iter_num} (predicted gain = {abs(l_inf - ll_m):.2e})"
                            )
                        loglik_trace = loglik_trace[:iter_num]
                        break
            
    elapsed = time.time() - t_start
    if conv == 0 and verbose:
        print(
            f"WARNING: Did not converge in {maxit} iterations "
            f"(delta = {delta:.2e})"
        )
    
    return {
        'means': {
            'x': x_smooth,
            'alpha': alpha,
            'beta': beta
        },
        'vars': {
            'P_smooth': P_smooth_all
        },
        'omega': Omega,
        'runtime': {
            'iters': len(loglik_trace),
            'conv': conv,
            'seconds': elapsed,
            'ncores_used': ncores_used,
            'loglik_trace': loglik_trace,
            'aitken_iter': aitken_iter
        }
    }
