import time
import numpy as np

from da_step import da_step
from kalman import kalman_smoother_country
from m_step import m_step_items, m_step_Omega
from stats_utils import logcdf, logsf


def compute_loglik(rc, alpha, beta, x_smooth, bill_session):
    rc = np.asarray(rc)
    alpha = np.asarray(alpha, dtype=float)
    beta = np.asarray(beta, dtype=float)
    x_smooth = np.asarray(x_smooth, dtype=float)
    bill_session = np.asarray(bill_session, dtype=int)

    N, J = rc.shape
    K = beta.shape[1]

    ll = 0.0
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

        pos_mask = rc_sub == 1
        if np.any(pos_mask):
            ll += float(np.sum(logcdf(mu_mat[pos_mask])))

        neg_mask = rc_sub == -1
        if np.any(neg_mask):
            ll += float(np.sum(logsf(mu_mat[neg_mask])))

    return ll


def param_change(alpha_old, alpha_new, beta_old, beta_new, x_old, x_new):
    return max(
        float(np.max(np.abs(alpha_new - alpha_old))),
        float(np.max(np.abs(beta_new - beta_old))),
        float(np.max(np.abs(x_new - x_old))),
    )


def _kalman_one_country(
    i,
    da_y_star,
    rc,
    alpha,
    beta,
    bill_session,
    x_mu0,
    x_Sigma0_list,
    Omega,
    startlegis,
    endlegis,
    T_total,
    K,
    x_prev_i,
):
    si = int(startlegis[i])
    ei = int(endlegis[i])

    obs_items = np.nonzero(rc[i, :] != 0)[0]

    if len(obs_items) == 0:
        x_smooth_i = np.full((K, T_total), np.nan, dtype=float)
        P_smooth_i = [None] * T_total
        P_cov = x_Sigma0_list[i].copy()
        for tt in range(T_total):
            if tt == 0:
                x_smooth_i[:, tt] = x_mu0[i, :]
                P_smooth_i[tt] = P_cov
            else:
                x_smooth_i[:, tt] = x_smooth_i[:, tt - 1]
                P_cov = P_cov + Omega
                P_smooth_i[tt] = P_cov
        return {
            "x_smooth_i": x_smooth_i,
            "P_smooth_i": P_smooth_i,
            "P_lag_i": [None] * max(T_total - 1, 0),
        }

    y_star_i = da_y_star[i, obs_items]

    ks = kalman_smoother_country(
        y_star_i=y_star_i,
        item_indices=obs_items,
        alpha=alpha,
        beta=beta,
        bill_session=bill_session,
        mu0=x_mu0[i, :],
        Sigma0=x_Sigma0_list[i],
        Omega=Omega,
        start_t=si,
        end_t=ei,
        T_total=T_total,
    )

    T_active_i = ei - si + 1
    x_smooth_i = x_prev_i.copy()
    P_smooth_i = [None] * T_total

    for s in range(T_active_i):
        t_global = si + s
        x_smooth_i[:, t_global] = ks["x_smooth"][:, s]
        P_smooth_i[t_global] = ks["P_smooth"][s]

    for tt in range(T_total):
        if P_smooth_i[tt] is None:
            P_smooth_i[tt] = np.eye(K) * 100.0
        if np.any(np.isnan(x_smooth_i[:, tt])):
            if tt > 0:
                x_smooth_i[:, tt] = x_smooth_i[:, tt - 1]
            else:
                x_smooth_i[:, tt] = x_mu0[i, :]

    return {
        "x_smooth_i": x_smooth_i,
        "P_smooth_i": P_smooth_i,
        "P_lag_i": ks["P_lag"],
    }


def dynIRT_KD(data, starts, priors, control, K=2):
    # ---- Unpack ----
    rc = np.asarray(data["rc"], dtype=int)
    startlegis = np.asarray(data["startlegis"], dtype=int).ravel()
    endlegis = np.asarray(data["endlegis"], dtype=int).ravel()
    bill_session = np.asarray(data["bill_session"], dtype=int).ravel()
    T_total = int(data["T"])

    N, J = rc.shape

    alpha = np.asarray(starts["alpha"], dtype=float).copy()
    beta = np.asarray(starts["beta"], dtype=float).copy()
    x_smooth = np.asarray(starts["x"], dtype=float).copy()

    x_mu0 = np.asarray(priors["x_mu0"], dtype=float)
    x_Sigma0 = priors["x_Sigma0"]
    if isinstance(x_Sigma0, list):
        x_Sigma0_list = [np.asarray(m, dtype=float) for m in x_Sigma0]
    else:
        x_Sigma0_arr = np.asarray(x_Sigma0, dtype=float)
        if x_Sigma0_arr.ndim != 2:
            raise ValueError("x_Sigma0 must be list of KxK or N x K diagonal matrix")
        x_Sigma0_list = [np.diag(x_Sigma0_arr[i, :]) for i in range(N)]

    beta_mu = np.asarray(priors["beta_mu"], dtype=float)
    beta_sigma = np.asarray(priors["beta_sigma"], dtype=float)
    Omega = np.asarray(priors["omega"], dtype=float)

    # Control
    verbose = bool(control.get("verbose", False))
    thresh = control.get("thresh", 1e-6)
    maxit = int(control.get("maxit", 500))
    checkfreq = int(control.get("checkfreq", 50))
    thresh_loglik = control.get("thresh_loglik", None)
    loglik_patience = int(control.get("loglik_patience", 3))
    estimate_omega = bool(control.get("estimate_omega", False))
    diagonal_omega = bool(control.get("diagonal_omega", False))
    thresh_aitken = control.get("thresh_aitken", None)
    ncores_used = int(control.get("ncores", 1))

    voters_by_item = [np.nonzero(rc[:, j] != 0)[0] for j in range(J)]

    loglik_trace = np.zeros(maxit, dtype=float)
    conv = 0
    aitken_iter = None
    loglik_streak = 0

    t_start = time.time()

    for m in range(maxit):
        alpha_old = alpha.copy()
        beta_old = beta.copy()
        x_old = x_smooth.copy()

        # ==== E-step ====
        da_y_star, _ = da_step(rc, alpha, beta, x_smooth, bill_session)

        P_smooth_all = [None] * N
        P_lag_all = [None] * N
        x_prev = x_smooth.copy()

        for i in range(N):
            res_i = _kalman_one_country(
                i=i,
                da_y_star=da_y_star,
                rc=rc,
                alpha=alpha,
                beta=beta,
                bill_session=bill_session,
                x_mu0=x_mu0,
                x_Sigma0_list=x_Sigma0_list,
                Omega=Omega,
                startlegis=startlegis,
                endlegis=endlegis,
                T_total=T_total,
                K=K,
                x_prev_i=x_prev[i, :, :],
            )
            x_smooth[i, :, :] = res_i["x_smooth_i"]
            P_smooth_all[i] = res_i["P_smooth_i"]
            P_lag_all[i] = res_i["P_lag_i"]

        # ==== M-step ====
        m_items = m_step_items(
            y_star=da_y_star,
            rc=rc,
            x_smooth=x_smooth,
            P_smooth=P_smooth_all,
            bill_session=bill_session,
            beta_mu=beta_mu,
            beta_sigma=beta_sigma,
            voters_by_item=voters_by_item,
        )
        alpha = m_items["alpha"]
        beta = m_items["beta"]

        if estimate_omega:
            Omega = m_step_Omega(
                x_smooth=x_smooth,
                P_smooth=P_smooth_all,
                P_lag=P_lag_all,
                startlegis=startlegis,
                endlegis=endlegis,
                diagonal_only=diagonal_omega,
            )

        # ==== Convergence check ====
        delta = param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

        need_ll = False
        if m == 0:
            need_ll = True
        if verbose and (m == 0 or (m + 1) % checkfreq == 0):
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

        if verbose and (m == 0 or (m + 1) % checkfreq == 0):
            print(
                f"Iter {m+1:4d} | loglik = {loglik_trace[m]:.4f} | "
                f"delta = {delta:.2e} | Omega[1,1] = {Omega[0,0]:.4f}"
            )

        if delta < thresh:
            conv = 1
            loglik_trace = loglik_trace[: m + 1]
            break

        if thresh_loglik is not None and m >= 1:
            ll_m = loglik_trace[m]
            ll_m1 = loglik_trace[m - 1]
            rel_change = abs(ll_m - ll_m1) / (1.0 + abs(ll_m1))
            if np.isfinite(rel_change) and rel_change < thresh_loglik:
                loglik_streak += 1
            else:
                loglik_streak = 0
            if loglik_streak >= loglik_patience:
                conv = 1
                loglik_trace = loglik_trace[: m + 1]
                break

        if thresh_aitken is not None and m >= 2:
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
                        aitken_iter = m + 1
                        conv = 1
                        loglik_trace = loglik_trace[: m + 1]
                        break

    elapsed = time.time() - t_start

    return {
        "means": {"x": x_smooth, "alpha": alpha, "beta": beta},
        "vars": {"P_smooth": P_smooth_all},
        "omega": Omega,
        "runtime": {
            "iters": len(loglik_trace),
            "conv": conv,
            "seconds": elapsed,
            "ncores_used": ncores_used,
            "loglik_trace": loglik_trace,
            "aitken_iter": aitken_iter,
        },
    }
