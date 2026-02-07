#' K-Dimensional Dynamic IRT via EM with SQUAREM + Aitken
#'
#' Alternative entry point that accelerates EM with SQUAREM and adds
#' Aitken early stopping. Interface and return format match dynIRT_KD().
#'
#' Reference: docs/estimation_plan_2d.md (full specification)

# Source companion modules from the same directory (skip if already loaded)
if (!exists("da_step", mode = "function") ||
    !exists("kalman_smoother_country", mode = "function") ||
    !exists("m_step_items", mode = "function")) {
  .KD_DIR_local <- tryCatch(
    dirname(normalizePath(sys.frame(sys.nframe())$ofile)),
    error = function(e) "scripts/R"
  )
  source(file.path(.KD_DIR_local, "da_step.R"))
  source(file.path(.KD_DIR_local, "kalman.R"))
  source(file.path(.KD_DIR_local, "m_step.R"))
}

# Null-coalesce operator (available in base R >= 4.4; define for older versions)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Compute observed-data log-likelihood (if not already defined)
if (!exists("compute_loglik", mode = "function")) {
  compute_loglik <- function(rc, alpha, beta, x_smooth, bill.session) {
    N <- nrow(rc)
    J <- ncol(rc)
    K <- ncol(beta)
    ll <- 0

    periods <- sort(unique(bill.session))

    for (tt in periods) {
      j_idx <- which(bill.session == tt)
      if (length(j_idx) == 0) next

      x_t <- x_smooth[, , tt + 1L, drop = FALSE]
      dim(x_t) <- c(N, K)

      mu_mat <- x_t %*% t(beta[j_idx, , drop = FALSE])
      mu_mat <- sweep(mu_mat, 2, alpha[j_idx], "+")

      rc_sub <- rc[, j_idx, drop = FALSE]

      pos_mask <- rc_sub == 1L
      if (any(pos_mask)) {
        ll <- ll + sum(pnorm(mu_mat[pos_mask], log.p = TRUE))
      }

      neg_mask <- rc_sub == -1L
      if (any(neg_mask)) {
        ll <- ll + sum(pnorm(mu_mat[neg_mask], lower.tail = FALSE, log.p = TRUE))
      }
    }

    ll
  }
}

# Compute max absolute parameter change (if not already defined)
if (!exists("param_change", mode = "function")) {
  param_change <- function(alpha_old, alpha_new, beta_old, beta_new,
                           x_old, x_new) {
    max(
      max(abs(alpha_new - alpha_old)),
      max(abs(beta_new - beta_old)),
      max(abs(x_new - x_old))
    )
  }
}

#' K-Dimensional Dynamic IRT via EM (Fast)
#'
#' @param .data    See dynIRT_KD().
#' @param .starts  See dynIRT_KD().
#' @param .priors  See dynIRT_KD().
#' @param .control See dynIRT_KD(), plus:
#'                 \describe{
#'                   \item{use_squarem}{Logical. Use SQUAREM acceleration? (default TRUE)}
#'                   \item{thresh_aitken}{Numeric. Aitken stopping threshold (default 1e-4)}
#'                   \item{clamp_limit}{Numeric. Clamp bound for params in SQUAREM evals (default 6)}
#'                   \item{ll_drop_tol}{Numeric. Log-lik drop tolerance for fallback (default 1e-6)}
#'                   \item{squarem_control}{List. Control params passed to SQUAREM (default step.max0=4)}
#'                 }
#' @param K        Integer. Number of latent dimensions (default 2).
#'
#' @return Same structure as dynIRT_KD().
#'         runtime includes extra fields: em_evals, events, aitken_iter.
#'
#' Notes:
#' - SQUAREM may extrapolate outside reasonable domains; a conservative clamp
#'   (|param| <= clamp_limit, default 6) is applied inside accelerated evaluations.
#' - If extrapolated updates are numerically unstable or reduce log-likelihood
#'   beyond ll_drop_tol (default 1e-6), the iteration falls back to vanilla EM.
#' - When estimate_omega=TRUE, SQUAREM is disabled (uses vanilla EM) to keep
#'   the fixed-point map well-defined.
#'
#' @export
#'

dynIRT_KD_fast <- function(.data, .starts, .priors, .control, K = 2L) {

  # ---- Unpack inputs ----
  rc           <- .data$rc
  startlegis   <- as.integer(.data$startlegis)
  endlegis     <- as.integer(.data$endlegis)
  bill.session <- as.integer(.data$bill.session)
  T_total      <- as.integer(.data$T)

  N <- nrow(rc)
  J <- ncol(rc)

  alpha <- as.numeric(.starts$alpha)
  beta  <- as.matrix(.starts$beta)
  if (ncol(beta) != K) stop("beta must have K columns")

  x_smooth <- .starts$x
  if (!is.array(x_smooth) || length(dim(x_smooth)) != 3) {
    stop("x must be an N x K x T array")
  }

  # Priors
  x_mu0 <- .priors$x.mu0
  x_Sigma0 <- .priors$x.Sigma0
  beta_mu <- as.numeric(.priors$beta.mu)
  beta_sigma <- as.matrix(.priors$beta.sigma)
  Omega <- as.matrix(.priors$omega)

  # Prepare Sigma0: if N x K matrix (diagonal variances), convert to list of diag matrices
  if (is.matrix(x_Sigma0) && !is.list(x_Sigma0)) {
    Sigma0_list <- vector("list", N)
    for (i in seq_len(N)) {
      Sigma0_list[[i]] <- diag(x_Sigma0[i, ], nrow = K)
    }
    x_Sigma0 <- Sigma0_list
  }

  # Control
  verbose        <- isTRUE(.control$verbose)
  thresh         <- .control$thresh %||% 1e-6
  maxit          <- .control$maxit %||% 500L
  checkfreq      <- .control$checkfreq %||% 50L
  estimate_omega <- isTRUE(.control$estimate_omega)
  diagonal_omega <- isTRUE(.control$diagonal_omega)
  use_squarem    <- .control$use_squarem %||% TRUE
  thresh_aitken  <- .control$thresh_aitken %||% 1e-4

  clamp_limit <- .control$clamp_limit %||% 6
  ll_drop_tol <- .control$ll_drop_tol %||% 1e-6
  squarem_control <- .control$squarem_control %||% list()

  # SQUAREM control (allow extrapolation by default)
  sq_ctrl <- modifyList(
    list(step.min0 = 1, step.max0 = 4, mstep = 4, method = 3, trace = FALSE),
    squarem_control
  )
  sq_ctrl$maxiter <- 1
  sq_ctrl$tol <- 0

  # Event log for diagnostics
  events <- list()
  add_event <- function(type, iter, detail) {
    events[[length(events) + 1L]] <<- list(iter = iter, type = type, detail = detail)
  }

  if (use_squarem && estimate_omega) {
    use_squarem <- FALSE
    add_event("squarem_disabled", 0L, "estimate_omega=TRUE; using vanilla EM")
    if (verbose) {
      message("SQUAREM disabled because estimate_omega=TRUE")
    }
  }

  if (use_squarem && !requireNamespace("SQUAREM", quietly = TRUE)) {
    stop("Package 'SQUAREM' is required for use_squarem=TRUE. Install it first.")
  }

  # ---- Helpers ----
  pack_theta <- function(alpha, beta, x) {
    c(alpha, as.vector(beta), as.vector(x))
  }

  unpack_theta <- function(theta) {
    idx1 <- J
    idx2 <- J + J * K
    alpha_u <- theta[1:idx1]
    beta_u <- matrix(theta[(idx1 + 1L):idx2], nrow = J, ncol = K)
    x_vec <- theta[(idx2 + 1L):length(theta)]
    x_u <- array(x_vec, dim = c(N, K, T_total))
    list(alpha = alpha_u, beta = beta_u, x = x_u)
  }

  clamp_params <- function(alpha, beta, x, limit) {
    alpha_c <- pmax(pmin(alpha, limit), -limit)
    beta_c  <- pmax(pmin(beta,  limit), -limit)
    x_c     <- pmax(pmin(x,     limit), -limit)
    clamped <- any(alpha_c != alpha) || any(beta_c != beta) || any(x_c != x)
    list(alpha = alpha_c, beta = beta_c, x = x_c, clamped = clamped)
  }

  # ---- Single EM update (E-step + M-step) ----
  em_update_internal <- function(alpha_in, beta_in, x_in, Omega_in) {

    # (a) Data augmentation
    da <- da_step(rc, alpha_in, beta_in, x_in, bill.session)

    # (b) Kalman filter-smoother for each country
    P_smooth_all <- vector("list", N)
    P_lag_all    <- vector("list", N)
    x_out <- x_in

    for (i in seq_len(N)) {
      si <- startlegis[i]
      ei <- endlegis[i]
      T_active_i <- ei - si + 1L

      obs_items <- which(rc[i, ] != 0L)
      if (length(obs_items) == 0) {
        P_smooth_i <- vector("list", T_total)
        P_cov <- x_Sigma0[[i]]
        for (tt in seq_len(T_total)) {
          if (tt == 1L) {
            x_out[i, , tt] <- x_mu0[i, ]
            P_smooth_i[[tt]] <- P_cov
          } else {
            x_out[i, , tt] <- x_out[i, , tt - 1L]
            P_cov <- P_cov + Omega_in
            P_smooth_i[[tt]] <- P_cov
          }
        }
        P_smooth_all[[i]] <- P_smooth_i
        P_lag_all[[i]] <- vector("list", max(T_total - 1L, 0L))
        next
      }

      y_star_i <- da$y_star[i, obs_items]

      ks <- kalman_smoother_country(
        y_star_i     = y_star_i,
        item_indices = obs_items,
        alpha        = alpha_in,
        beta         = beta_in,
        bill.session = bill.session,
        mu0          = x_mu0[i, ],
        Sigma0       = x_Sigma0[[i]],
        Omega        = Omega_in,
        start_t      = si,
        end_t        = ei,
        T_total      = T_total
      )

      for (s in seq_len(T_active_i)) {
        t_global <- si + s
        x_out[i, , t_global] <- ks$x_smooth[, s]
      }

      P_smooth_i <- vector("list", T_total)
      for (s in seq_len(T_active_i)) {
        t_global <- si + s
        P_smooth_i[[t_global]] <- ks$P_smooth[[s]]
      }
      for (tt in seq_len(T_total)) {
        if (is.null(P_smooth_i[[tt]])) {
          P_smooth_i[[tt]] <- diag(K) * 100
        }
      }
      P_smooth_all[[i]] <- P_smooth_i
      P_lag_all[[i]] <- ks$P_lag
    }

    # (c) M-step: item parameters
    m_items <- m_step_items(
      y_star       = da$y_star,
      rc           = rc,
      x_smooth     = x_out,
      P_smooth     = P_smooth_all,
      bill.session = bill.session,
      beta_mu      = beta_mu,
      beta_sigma   = beta_sigma
    )
    alpha_out <- m_items$alpha
    beta_out  <- m_items$beta

    # (d) Optional Omega update
    Omega_out <- Omega_in
    if (estimate_omega) {
      Omega_out <- m_step_Omega(
        x_smooth     = x_out,
        P_smooth     = P_smooth_all,
        P_lag        = P_lag_all,
        startlegis   = startlegis,
        endlegis     = endlegis,
        diagonal_only = diagonal_omega
      )
    }

    list(
      alpha    = alpha_out,
      beta     = beta_out,
      x        = x_out,
      P_smooth = P_smooth_all,
      P_lag    = P_lag_all,
      Omega    = Omega_out
    )
  }

  # ---- Fixed-point evaluation wrapper ----
  em_evals <- 0L

  em_eval_from_parts <- function(alpha_in, beta_in, x_in, Omega_in, do_clamp = FALSE) {
    em_evals <<- em_evals + 1L

    clamped_flag <- FALSE
    if (do_clamp) {
      cl <- clamp_params(alpha_in, beta_in, x_in, clamp_limit)
      clamped_flag <- clamped_flag || cl$clamped
      alpha_in <- cl$alpha
      beta_in  <- cl$beta
      x_in     <- cl$x
    }

    res <- em_update_internal(alpha_in, beta_in, x_in, Omega_in)

    if (do_clamp) {
      cl2 <- clamp_params(res$alpha, res$beta, res$x, clamp_limit)
      clamped_flag <- clamped_flag || cl2$clamped
      res$alpha <- cl2$alpha
      res$beta  <- cl2$beta
      res$x     <- cl2$x
    }

    res$clamped <- clamped_flag
    res
  }

  # ---- EM Loop ----
  loglik_trace <- numeric(maxit)
  aitken_est   <- rep(NA_real_, maxit)
  conv <- 0L
  aitken_iter <- NA_integer_

  t_start <- proc.time()

  theta <- pack_theta(alpha, beta, x_smooth)
  P_smooth_all <- vector("list", N)
  P_lag_all <- vector("list", N)

  for (m in seq_len(maxit)) {

    alpha_old <- alpha
    beta_old  <- beta
    x_old     <- x_smooth
    theta_old <- theta

    ll_old <- if (m > 1L) loglik_trace[m - 1L] else {
      compute_loglik(rc, alpha, beta, x_smooth, bill.session)
    }

    if (use_squarem) {
      # Cache for outputs from squarem fixpt evaluations (for P_smooth alignment)
      cache <- new.env(parent = emptyenv())
      cache$thetas <- list()
      cache$results <- list()

      fixpt_fn <- function(th) {
        pars <- unpack_theta(th)
        res <- em_eval_from_parts(pars$alpha, pars$beta, pars$x, Omega, do_clamp = TRUE)
        th_new <- pack_theta(res$alpha, res$beta, res$x)
        cache$thetas[[length(cache$thetas) + 1L]] <- th_new
        cache$results[[length(cache$results) + 1L]] <- res
        th_new
      }

      sq <- SQUAREM::squarem(
        par = theta_old,
        fixptfn = fixpt_fn,
        control = sq_ctrl
      )

      theta_cand <- as.numeric(sq$par)

      if (length(cache$thetas) == 0L) {
        add_event("squarem_fallback", m, "no fixpt evals returned; using vanilla EM")
        res_cand <- em_eval_from_parts(alpha_old, beta_old, x_old, Omega, do_clamp = FALSE)
      } else {
        dists <- vapply(cache$thetas, function(th) sum((th - theta_cand)^2), numeric(1))
        idx <- which.min(dists)
        res_cand <- cache$results[[idx]]
      }

      # Apply candidate update
      alpha <- res_cand$alpha
      beta  <- res_cand$beta
      x_smooth <- res_cand$x
      P_smooth_all <- res_cand$P_smooth
      P_lag_all <- res_cand$P_lag
      theta <- pack_theta(alpha, beta, x_smooth)

      ll_new <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)

      invalid <- any(!is.finite(theta)) || !is.finite(ll_new)
      if (is.finite(ll_old) && ll_new < (ll_old - ll_drop_tol)) {
        invalid <- TRUE
      }

      if (invalid) {
        res_fb <- em_eval_from_parts(alpha_old, beta_old, x_old, Omega, do_clamp = FALSE)
        alpha <- res_fb$alpha
        beta  <- res_fb$beta
        x_smooth <- res_fb$x
        P_smooth_all <- res_fb$P_smooth
        P_lag_all <- res_fb$P_lag
        if (estimate_omega) Omega <- res_fb$Omega
        theta <- pack_theta(alpha, beta, x_smooth)
        ll_new <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)
        add_event("squarem_fallback", m, "invalid step or loglik drop")
      }

      if (isTRUE(res_cand$clamped)) {
        add_event("squarem_clamp", m, "parameters clamped during acceleration")
      }

    } else {
      res <- em_eval_from_parts(alpha, beta, x_smooth, Omega, do_clamp = FALSE)
      alpha <- res$alpha
      beta  <- res$beta
      x_smooth <- res$x
      P_smooth_all <- res$P_smooth
      P_lag_all <- res$P_lag
      if (estimate_omega) Omega <- res$Omega
      theta <- pack_theta(alpha, beta, x_smooth)
      ll_new <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)
    }

    loglik_trace[m] <- ll_new

    delta <- param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

    if (verbose && (m == 1L || m %% checkfreq == 0L)) {
      message(sprintf(
        "Iter %4d | loglik = %.4f | delta = %.2e | Omega[1,1] = %.4f",
        m, ll_new, delta, Omega[1, 1]
      ))
    }

    # Aitken acceleration stopping rule
    if (m >= 3L) {
      ll_m   <- loglik_trace[m]
      ll_m1  <- loglik_trace[m - 1L]
      ll_m2  <- loglik_trace[m - 2L]
      denom <- ll_m1 - ll_m2
      if (is.finite(ll_m) && is.finite(ll_m1) && is.finite(ll_m2) && denom != 0) {
        a_m <- (ll_m - ll_m1) / denom
        if (is.finite(a_m) && a_m > 0 && a_m < 1) {
          l_inf <- ll_m1 + (ll_m - ll_m1) / (1 - a_m)
          aitken_est[m] <- l_inf
          if (is.finite(l_inf) && abs(l_inf - ll_m) < thresh_aitken) {
            conv <- 1L
            aitken_iter <- m
            if (verbose) {
              message(sprintf(
                "Aitken stop at iter %d | l_inf = %.4f | ll = %.4f",
                m, l_inf, ll_m
              ))
            }
            loglik_trace <- loglik_trace[seq_len(m)]
            aitken_est <- aitken_est[seq_len(m)]
            break
          }
        }
      }
    }

    # Parameter-based convergence
    if (delta < thresh) {
      conv <- 1L
      if (verbose) {
        message(sprintf("Converged at iteration %d (delta = %.2e)", m, delta))
      }
      loglik_trace <- loglik_trace[seq_len(m)]
      aitken_est <- aitken_est[seq_len(m)]
      break
    }
  }

  elapsed <- as.numeric((proc.time() - t_start)["elapsed"])

  if (conv == 0L && verbose) {
    message(sprintf(
      "WARNING: Did not converge in %d iterations", maxit
    ))
  }

  list(
    means = list(
      x     = x_smooth,
      alpha = alpha,
      beta  = beta
    ),
    vars = list(
      P_smooth = P_smooth_all
    ),
    omega = Omega,
    runtime = list(
      iters        = length(loglik_trace),
      conv         = conv,
      seconds      = elapsed,
      loglik_trace = loglik_trace,
      aitken_est   = aitken_est,
      aitken_iter  = aitken_iter,
      em_evals     = em_evals,
      events       = events
    )
  )
}
