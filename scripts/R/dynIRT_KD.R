#' K-Dimensional Dynamic IRT via EM with Data Augmentation
#'
#' Main entry point for the K-dimensional dynamic probit IRT model.
#' Orchestrates the EM algorithm: E-step (data augmentation + Kalman
#' filter-smoother) and M-step (item parameters + optional Omega).
#'
#' Reference: docs/estimation_plan_2d.md (full specification)
#'            Imai, Lo & Olmsted (2016), APSR 110(4).

# Source companion modules from the same directory (skip if already loaded)
# Usage: source("scripts/R/dynIRT_KD.R") from the project root,
#        or source da_step.R, kalman.R, m_step.R individually before this file.
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


#' Compute observed-data log-likelihood (Section 10.2)
#'
#' @param rc           N x J integer matrix.
#' @param alpha        Numeric vector length J.
#' @param beta         J x K numeric matrix.
#' @param x_smooth     N x K x T numeric array.
#' @param bill.session Integer vector length J. 0-indexed.
#' @return Scalar log-likelihood.
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

    # Linear predictor: N x |j_idx|
    mu_mat <- x_t %*% t(beta[j_idx, , drop = FALSE])
    mu_mat <- sweep(mu_mat, 2, alpha[j_idx], "+")

    rc_sub <- rc[, j_idx, drop = FALSE]

    # Positive votes: log Phi(mu)
    pos_mask <- rc_sub == 1L
    if (any(pos_mask)) {
      ll <- ll + sum(pnorm(mu_mat[pos_mask], log.p = TRUE))
    }

    # Negative votes: log(1 - Phi(mu)) = log Phi(-mu)
    neg_mask <- rc_sub == -1L
    if (any(neg_mask)) {
      ll <- ll + sum(pnorm(mu_mat[neg_mask], lower.tail = FALSE, log.p = TRUE))
    }
  }

  ll
}


#' Compute max absolute parameter change (Section 10.1)
#'
#' @param alpha_old    Numeric vector.
#' @param alpha_new    Numeric vector.
#' @param beta_old     J x K matrix.
#' @param beta_new     J x K matrix.
#' @param x_old        N x K x T array.
#' @param x_new        N x K x T array.
#' @return Scalar. Max absolute change.
param_change <- function(alpha_old, alpha_new, beta_old, beta_new,
                         x_old, x_new) {
  max(
    max(abs(alpha_new - alpha_old)),
    max(abs(beta_new - beta_old)),
    max(abs(x_new - x_old))
  )
}

#' Kalman filter-smoother for one country (helper for parallelization)
#'
#' @param i Integer. Country index.
#' @param da_y_star N x J matrix of pseudo-observations.
#' @param rc N x J integer matrix.
#' @param alpha Numeric vector length J.
#' @param beta J x K numeric matrix.
#' @param bill.session Integer vector length J (0-indexed).
#' @param x_mu0 N x K numeric matrix (prior means).
#' @param x_Sigma0 List of N K x K matrices (prior covariances).
#' @param Omega K x K numeric matrix (evolution covariance).
#' @param startlegis Integer vector length N (0-indexed start period).
#' @param endlegis Integer vector length N (0-indexed end period).
#' @param T_total Integer. Total number of periods.
#' @param K Integer. Number of latent dimensions.
#' @param x_prev_i K x T_total numeric matrix (previous x for this country).
#'
#' @return List with components:
#'   \item{x_smooth_i}{K x T_total matrix}
#'   \item{P_smooth_i}{List of length T_total of K x K matrices}
#'   \item{P_lag_i}{List of length (T_total - 1)}
.kalman_one_country <- function(i, da_y_star, rc, alpha, beta, bill.session,
                                x_mu0, x_Sigma0, Omega, startlegis, endlegis,
                                T_total, K, x_prev_i) {
  si <- startlegis[i]
  ei <- endlegis[i]

  obs_items <- which(rc[i, ] != 0L)

  if (length(obs_items) == 0) {
    # No observations: smoothed = prior, propagated via random walk
    x_smooth_i <- matrix(NA_real_, nrow = K, ncol = T_total)
    P_smooth_i <- vector("list", T_total)
    P_cov <- x_Sigma0[[i]]
    for (tt in seq_len(T_total)) {
      if (tt == 1L) {
        x_smooth_i[, tt] <- x_mu0[i, ]
        P_smooth_i[[tt]] <- P_cov
      } else {
        x_smooth_i[, tt] <- x_smooth_i[, tt - 1L]
        P_cov <- P_cov + Omega
        P_smooth_i[[tt]] <- P_cov
      }
    }
    return(list(
      x_smooth_i = x_smooth_i,
      P_smooth_i = P_smooth_i,
      P_lag_i    = vector("list", max(T_total - 1L, 0L))
    ))
  }

  y_star_i <- da_y_star[i, obs_items]

  ks <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = obs_items,
    alpha        = alpha,
    beta         = beta,
    bill.session = bill.session,
    mu0          = x_mu0[i, ],
    Sigma0       = x_Sigma0[[i]],
    Omega        = Omega,
    start_t      = si,
    end_t        = ei,
    T_total      = T_total
  )

  T_active_i <- ei - si + 1L
  x_smooth_i <- x_prev_i
  P_smooth_i <- vector("list", T_total)

  for (s in seq_len(T_active_i)) {
    t_global <- si + s
    x_smooth_i[, t_global] <- ks$x_smooth[, s]
    P_smooth_i[[t_global]] <- ks$P_smooth[[s]]
  }

  for (tt in seq_len(T_total)) {
    if (is.null(P_smooth_i[[tt]])) {
      P_smooth_i[[tt]] <- diag(K) * 100
    }
    if (any(is.na(x_smooth_i[, tt]))) {
      x_smooth_i[, tt] <- if (tt > 1L) x_smooth_i[, tt - 1L] else x_mu0[i, ]
    }
  }

  list(
    x_smooth_i = x_smooth_i,
    P_smooth_i = P_smooth_i,
    P_lag_i    = ks$P_lag
  )
}


#' K-Dimensional Dynamic IRT via EM
#'
#' @param .data    List with components:
#'                 \describe{
#'                   \item{rc}{N x J integer matrix. Votes: +1, -1, 0.}
#'                   \item{startlegis}{N x 1 integer matrix. 0-indexed first period.}
#'                   \item{endlegis}{N x 1 integer matrix. 0-indexed last period.}
#'                   \item{bill.session}{J x 1 integer matrix. 0-indexed period per item.}
#'                   \item{T}{Integer. Number of periods.}
#'                 }
#' @param .starts  List with components:
#'                 \describe{
#'                   \item{alpha}{Numeric vector length J. Starting intercepts.}
#'                   \item{beta}{J x K numeric matrix. Starting discriminations.}
#'                   \item{x}{N x K x T numeric array. Starting ideal points.}
#'                 }
#' @param .priors  List with components:
#'                 \describe{
#'                   \item{x.mu0}{N x K numeric matrix. Prior means for initial state.}
#'                   \item{x.Sigma0}{List of N K x K matrices. Prior covariances.
#'                         Alternatively, N x K matrix of diagonal variances.}
#'                   \item{beta.mu}{Numeric vector length (K+1). Prior mean for item params.}
#'                   \item{beta.sigma}{(K+1) x (K+1) numeric matrix. Prior covariance.}
#'                   \item{omega}{K x K numeric matrix. Evolution covariance (fixed or initial).}
#'                 }
#' @param .control List with components:
#'                 \describe{
#'                   \item{threads}{Integer. Number of threads (deprecated; unused).}
#'                   \item{ncores}{Integer. Number of cores for Kalman parallelism.
#'                         Default: half of detected cores (min 1). On Windows,
#'                         forced to 1.}
#'                   \item{verbose}{Logical. Print progress?}
#'                   \item{thresh}{Numeric. Convergence threshold (default 1e-6).}
#'                   \item{maxit}{Integer. Max EM iterations (default 500).}
#'                   \item{checkfreq}{Integer. Print every N iterations (default 50).}
#'                   \item{thresh_loglik}{Numeric or NULL. Relative log-likelihood
#'                         change threshold for early stopping (default NULL).
#'                         Uses |ll_t - ll_{t-1}| / (1 + |ll_{t-1}|).}
#'                   \item{loglik_patience}{Integer. Number of consecutive iterations
#'                         below thresh_loglik required to stop (default 3).}
#'                   \item{estimate_omega}{Logical. Estimate Omega? (default FALSE).}
#'                   \item{diagonal_omega}{Logical. Diagonal Omega only? (default FALSE).}
#'                   \item{thresh_aitken}{Numeric or NULL. Aitken acceleration early
#'                         stopping threshold. When non-NULL, the EM loop monitors the
#'                         Aitken-extrapolated log-likelihood limit and stops when the
#'                         predicted remaining gain falls below this threshold.
#'                         Recommended value: 1e-4. Default NULL (disabled).}
#'                 }
#' @param K        Integer. Number of latent dimensions (default 2).
#'
#' @return List with components:
#'   \item{means}{List: x (N x K x T array), alpha (J-vector), beta (J x K matrix).}
#'   \item{vars}{List: P_smooth (list of N, each list of T K x K matrices).}
#'   \item{omega}{K x K matrix (final).}
#'   \item{runtime}{List: iters, conv (0/1), seconds, ncores_used,
#'         loglik_trace (vector), aitken_iter (integer or NA — iteration at which
#'         Aitken stopped).}
dynIRT_KD <- function(.data, .starts, .priors, .control, K = 2L) {

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
  x_mu0 <- .priors$x.mu0  # N x K matrix
  x_Sigma0 <- .priors$x.Sigma0  # list of N K x K matrices, or N x K diagonal
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
  thresh_loglik  <- .control$thresh_loglik  # NULL = disabled
  loglik_patience <- .control$loglik_patience %||% 3L
  estimate_omega <- isTRUE(.control$estimate_omega)
  diagonal_omega <- isTRUE(.control$diagonal_omega)
  thresh_aitken  <- .control$thresh_aitken  # NULL = disabled

  # Parallelism control (Kalman by country)
  ncores <- .control$ncores
  if (is.null(ncores)) {
    detected <- parallel::detectCores()
    if (is.na(detected) || detected < 1L) detected <- 1L
    ncores <- max(1L, floor(detected / 2))
  }
  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) ncores <- 1L
  if (.Platform$OS.type == "windows") {
    if (ncores > 1L && verbose) {
      message("Windows detected: disabling mclapply; using ncores = 1")
    }
    ncores <- 1L
  }
  ncores_used <- ncores
  if (verbose && ncores_used > 1L) {
    message(sprintf("Using %d cores for Kalman filtering", ncores_used))
  }

  # Precompute voters per item (static across EM iterations)
  voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

  # ---- EM Loop ----
  loglik_trace <- numeric(maxit)
  conv <- 0L
  aitken_iter <- NA_integer_
  loglik_streak <- 0L
  t_start <- proc.time()

  for (m in seq_len(maxit)) {

    # Save old parameters for convergence check
    alpha_old <- alpha
    beta_old  <- beta
    x_old     <- x_smooth

    # ==== E-STEP ====

    # (a) Data Augmentation (Section 5)
    da <- da_step(rc, alpha, beta, x_smooth, bill.session)

    # (b) Kalman Filter-Smoother for each country (Section 6)
    P_smooth_all <- vector("list", N)
    P_lag_all    <- vector("list", N)
    x_prev <- x_smooth

    if (ncores_used > 1L) {
      ks_results <- parallel::mclapply(seq_len(N), function(i) {
        .kalman_one_country(
          i = i,
          da_y_star = da$y_star,
          rc = rc,
          alpha = alpha,
          beta = beta,
          bill.session = bill.session,
          x_mu0 = x_mu0,
          x_Sigma0 = x_Sigma0,
          Omega = Omega,
          startlegis = startlegis,
          endlegis = endlegis,
          T_total = T_total,
          K = K,
          x_prev_i = matrix(x_prev[i, , ], nrow = K, ncol = T_total)
        )
      }, mc.cores = ncores_used)
    } else {
      ks_results <- lapply(seq_len(N), function(i) {
        .kalman_one_country(
          i = i,
          da_y_star = da$y_star,
          rc = rc,
          alpha = alpha,
          beta = beta,
          bill.session = bill.session,
          x_mu0 = x_mu0,
          x_Sigma0 = x_Sigma0,
          Omega = Omega,
          startlegis = startlegis,
          endlegis = endlegis,
          T_total = T_total,
          K = K,
          x_prev_i = matrix(x_prev[i, , ], nrow = K, ncol = T_total)
        )
      })
    }

    for (i in seq_len(N)) {
      res_i <- ks_results[[i]]
      x_smooth[i, , ] <- res_i$x_smooth_i
      P_smooth_all[[i]] <- res_i$P_smooth_i
      P_lag_all[[i]] <- res_i$P_lag_i
    }

    # ==== M-STEP ====

    # (a) Update item parameters (Section 7)
    m_items <- m_step_items(
      y_star       = da$y_star,
      rc           = rc,
      x_smooth     = x_smooth,
      P_smooth     = P_smooth_all,
      bill.session = bill.session,
      beta_mu      = beta_mu,
      beta_sigma   = beta_sigma,
      voters_by_item = voters_by_item
    )
    alpha <- m_items$alpha
    beta  <- m_items$beta

    # (b) Optionally update Omega (Section 8)
    if (estimate_omega) {
      Omega <- m_step_Omega(
        x_smooth   = x_smooth,
        P_smooth   = P_smooth_all,
        P_lag      = P_lag_all,
        startlegis = startlegis,
        endlegis   = endlegis,
        diagonal_only = diagonal_omega
      )
    }

    # ==== Convergence check ====
    delta <- param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

    need_ll <- FALSE
    if (m == 1L) need_ll <- TRUE
    if (verbose && (m == 1L || m %% checkfreq == 0L)) need_ll <- TRUE
    if (!is.null(thresh_aitken)) need_ll <- TRUE
    if (!is.null(thresh_loglik)) need_ll <- TRUE
    if (delta < thresh) need_ll <- TRUE

    if (need_ll) {
      ll <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)
      loglik_trace[m] <- ll
    } else {
      loglik_trace[m] <- loglik_trace[m - 1L]
    }

    if (verbose && (m == 1L || m %% checkfreq == 0L)) {
      message(sprintf(
        "Iter %4d | loglik = %.4f | delta = %.2e | Omega[1,1] = %.4f",
        m, loglik_trace[m], delta, Omega[1, 1]
      ))
    }

    if (delta < thresh) {
      conv <- 1L
      if (verbose) {
        message(sprintf("Converged at iteration %d (delta = %.2e)", m, delta))
      }
      loglik_trace <- loglik_trace[seq_len(m)]
      break
    }

    # Log-likelihood early stopping (optional)
    if (!is.null(thresh_loglik) && m >= 2L) {
      ll_m <- loglik_trace[m]
      ll_m1 <- loglik_trace[m - 1L]
      rel_change <- abs(ll_m - ll_m1) / (1 + abs(ll_m1))
      if (is.finite(rel_change) && rel_change < thresh_loglik) {
        loglik_streak <- loglik_streak + 1L
      } else {
        loglik_streak <- 0L
      }
      if (loglik_streak >= loglik_patience) {
        conv <- 1L
        if (verbose) {
          message(sprintf(
            "Loglik converged at iteration %d (rel_change = %.2e, patience = %d)",
            m, rel_change, loglik_patience
          ))
        }
        loglik_trace <- loglik_trace[seq_len(m)]
        break
      }
    }

    # Aitken early stopping (optional)
    if (!is.null(thresh_aitken) && m >= 3L) {
      ll_m   <- loglik_trace[m]
      ll_m1  <- loglik_trace[m - 1L]
      ll_m2  <- loglik_trace[m - 2L]
      denom  <- ll_m1 - ll_m2
      if (is.finite(ll_m) && is.finite(ll_m1) && is.finite(ll_m2) &&
          abs(denom) > .Machine$double.eps) {
        a_m <- (ll_m - ll_m1) / denom
        if (is.finite(a_m) && a_m > 0 && a_m < 1) {
          l_inf <- ll_m1 + (ll_m - ll_m1) / (1 - a_m)
          if (is.finite(l_inf) && abs(l_inf - ll_m) < thresh_aitken) {
            aitken_iter <- m
            conv <- 1L
            if (verbose) {
              message(sprintf(
                "Aitken converged at iteration %d (predicted gain = %.2e)",
                m, abs(l_inf - ll_m)
              ))
            }
            loglik_trace <- loglik_trace[seq_len(m)]
            break
          }
        }
      }
    }
  }

  elapsed <- as.numeric((proc.time() - t_start)["elapsed"])

  if (conv == 0L && verbose) {
    message(sprintf(
      "WARNING: Did not converge in %d iterations (delta = %.2e)", maxit, delta
    ))
  }

  # ---- Return ----
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
      ncores_used  = ncores_used,
      loglik_trace = loglik_trace,
      aitken_iter  = aitken_iter
    )
  )
}
