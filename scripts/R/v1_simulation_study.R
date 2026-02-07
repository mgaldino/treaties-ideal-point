#' Phase V1: Simulation Study (Internal Validation)
#'
#' Verifies that dynIRT_KD recovers known parameters from simulated data.
#' Reference: Section 15.1 of docs/estimation_plan_2d.md
#'
#' Usage: Rscript scripts/R/v1_simulation_study.R
#'        or source("scripts/R/v1_simulation_study.R") from project root

library(MASS)

# Source the estimation code
source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD.R")

# ============================================================
# V1.1 Data-Generating Process
# ============================================================

simulate_data <- function(seed, N = 100, J = 200, T_sim = 6, K = 2) {
  set.seed(seed)

  # True evolution covariance
  Omega_true <- 0.1 * diag(K)

  # True ideal points: random walk
  x_true <- array(NA, dim = c(N, K, T_sim))
  x_true[, , 1] <- mvrnorm(N, mu = rep(0, K), Sigma = diag(K))
  for (t in 2:T_sim) {
    x_true[, , t] <- x_true[, , t - 1] +
      mvrnorm(N, mu = rep(0, K), Sigma = Omega_true)
  }

  # True item parameters
  alpha_true <- rnorm(J, 0, 1)
  beta_true  <- matrix(rnorm(J * K, 0, 0.7), nrow = J, ncol = K)

  # Assign items to periods (roughly uniform)
  bill_session <- sample(0:(T_sim - 1), J, replace = TRUE)

  # Generate votes (probit)
  rc <- matrix(0L, nrow = N, ncol = J)
  for (j in 1:J) {
    t_j <- bill_session[j] + 1L
    mu_star <- alpha_true[j] + x_true[, , t_j] %*% beta_true[j, ]
    rc[, j] <- ifelse(rnorm(N) < mu_star, 1L, -1L)
  }

  # Introduce ~30% missing data (random)
  missing_mask <- matrix(rbinom(N * J, 1, 0.3), nrow = N, ncol = J)
  rc[missing_mask == 1] <- 0L

  list(
    rc = rc,
    bill_session = bill_session,
    x_true = x_true,
    alpha_true = alpha_true,
    beta_true = beta_true,
    Omega_true = Omega_true,
    N = N, J = J, T_sim = T_sim, K = K
  )
}

# ============================================================
# V1.2 Select anchors and run estimation
# ============================================================

select_anchors <- function(x_true, K = 2) {
  # Target anchor positions
  targets <- rbind(
    c(+2, +2),   # a0: high on both dims
    c(-2, -2),   # a1: low on both dims
    c(+1, -1)    # a2: off-diagonal
  )
  if (K != 2) stop("Anchor selection only implemented for K=2")

  N <- nrow(x_true)
  x_t1 <- matrix(x_true[, , 1], nrow = N, ncol = K)

  # Find unit closest to each target
  anchor_idx <- integer(3)
  for (a in 1:3) {
    dists <- rowSums(sweep(x_t1, 2, targets[a, ])^2)
    # Avoid selecting the same unit twice
    dists[anchor_idx[anchor_idx > 0]] <- Inf
    anchor_idx[a] <- which.min(dists)
  }

  list(
    idx = anchor_idx,
    positions = targets
  )
}


run_single_replication <- function(seed, N = 100, J = 200, T_sim = 6, K = 2,
                                    maxit = 500, thresh = 1e-6) {
  cat(sprintf("\n===== Replication seed=%d =====\n", seed))

  # Generate data
  sim <- simulate_data(seed, N, J, T_sim, K)

  # Select anchors
  anchors <- select_anchors(sim$x_true, K)
  cat(sprintf("Anchor units: %s\n", paste(anchors$idx, collapse = ", ")))
  cat(sprintf("Anchor t=1 true positions:\n"))
  for (a in seq_along(anchors$idx)) {
    i <- anchors$idx[a]
    cat(sprintf("  Unit %d: (%.2f, %.2f) -> target (%.1f, %.1f)\n",
                i, sim$x_true[i, 1, 1], sim$x_true[i, 2, 1],
                anchors$positions[a, 1], anchors$positions[a, 2]))
  }

  # Build priors
  x_mu0 <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)  # diagonal variances

  for (a in seq_along(anchors$idx)) {
    i <- anchors$idx[a]
    x_mu0[i, ] <- anchors$positions[a, ]
    x_Sigma0[i, ] <- 0.01  # tight prior
  }

  # Starting values: noisy version of truth (to test convergence, not just recovery)
  x_start <- sim$x_true + array(rnorm(N * K * T_sim, 0, 0.5),
                                  dim = c(N, K, T_sim))
  # Overwrite anchor starts with target positions
  for (a in seq_along(anchors$idx)) {
    i <- anchors$idx[a]
    for (t in 1:T_sim) {
      x_start[i, , t] <- anchors$positions[a, ]
    }
  }

  alpha_start <- rnorm(J, 0, 0.5)
  beta_start  <- matrix(rnorm(J * K, 0, 0.3), nrow = J, ncol = K)

  # Build data list
  .data <- list(
    rc           = sim$rc,
    startlegis   = matrix(rep(0L, N), ncol = 1),
    endlegis     = matrix(rep(T_sim - 1L, N), ncol = 1),
    bill.session = matrix(sim$bill_session, ncol = 1),
    T            = T_sim
  )

  .starts <- list(
    alpha = alpha_start,
    beta  = beta_start,
    x     = x_start
  )

  .priors <- list(
    x.mu0      = x_mu0,
    x.Sigma0   = x_Sigma0,
    beta.mu    = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega      = 0.1 * diag(K)
  )

  .control <- list(
    threads        = 1L,
    verbose        = TRUE,
    thresh         = thresh,
    maxit          = maxit,
    checkfreq      = 50L,
    estimate_omega = FALSE
  )

  # Run estimation
  t0 <- proc.time()
  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)
  elapsed <- as.numeric((proc.time() - t0)["elapsed"])

  cat(sprintf("\nFinished in %.1f seconds, %d iterations, conv=%d\n",
              elapsed, result$runtime$iters, result$runtime$conv))

  # ============================================================
  # V1.3 Evaluation
  # ============================================================

  eval_results <- evaluate_recovery(sim, result, anchors)
  eval_results$seed <- seed
  eval_results$elapsed <- elapsed
  eval_results$iters <- result$runtime$iters
  eval_results$conv <- result$runtime$conv
  eval_results$loglik_trace <- result$runtime$loglik_trace

  eval_results
}


evaluate_recovery <- function(sim, result, anchors) {
  N <- sim$N; J <- sim$J; K <- sim$K; T_sim <- sim$T_sim

  # --- Ideal point recovery per dimension per period ---
  x_cor <- matrix(NA, nrow = T_sim, ncol = K)
  for (t in 1:T_sim) {
    for (k in 1:K) {
      # Exclude anchor countries from correlation (they're fixed)
      non_anchor <- setdiff(1:N, anchors$idx)
      x_cor[t, k] <- cor(sim$x_true[non_anchor, k, t],
                          result$means$x[non_anchor, k, t])
    }
  }
  cat("\nIdeal point correlations (non-anchor) per period x dimension:\n")
  colnames(x_cor) <- paste0("dim", 1:K)
  rownames(x_cor) <- paste0("t", 1:T_sim)
  print(round(x_cor, 4))

  mean_x_cor <- mean(x_cor)
  cat(sprintf("Mean ideal point correlation: %.4f (target > 0.90)\n", mean_x_cor))

  # --- Item discrimination recovery per dimension ---
  beta_cor <- numeric(K)
  for (k in 1:K) {
    beta_cor[k] <- cor(sim$beta_true[, k], result$means$beta[, k])
  }
  cat(sprintf("\nBeta correlations: %s (target > 0.85)\n",
              paste(sprintf("dim%d=%.4f", 1:K, beta_cor), collapse = ", ")))

  # --- Item intercept recovery ---
  alpha_cor <- cor(sim$alpha_true, result$means$alpha)
  cat(sprintf("Alpha correlation: %.4f (target > 0.85)\n", alpha_cor))

  # --- Log-likelihood monotonicity ---
  ll <- result$runtime$loglik_trace
  ll_diffs <- diff(ll)
  ll_monotone <- all(ll_diffs >= -1e-6)
  n_decreases <- sum(ll_diffs < -1e-6)
  cat(sprintf("Log-likelihood monotonic: %s", ifelse(ll_monotone, "YES", "NO")))
  if (!ll_monotone) {
    cat(sprintf(" (%d decreases, max decrease = %.2e)", n_decreases, min(ll_diffs)))
  }
  cat("\n")

  list(
    x_cor = x_cor,
    mean_x_cor = mean_x_cor,
    beta_cor = beta_cor,
    alpha_cor = alpha_cor,
    ll_monotone = ll_monotone,
    n_ll_decreases = n_decreases
  )
}

# ============================================================
# V1.4 Run replications
# ============================================================

run_all_replications <- function(seeds = 1:10, ...) {
  results <- vector("list", length(seeds))

  for (i in seq_along(seeds)) {
    results[[i]] <- run_single_replication(seed = seeds[i], ...)
  }

  # Summary across replications
  cat("\n\n========== SUMMARY ACROSS REPLICATIONS ==========\n\n")

  mean_x_cors  <- sapply(results, `[[`, "mean_x_cor")
  alpha_cors   <- sapply(results, `[[`, "alpha_cor")
  beta_cors_d1 <- sapply(results, function(r) r$beta_cor[1])
  beta_cors_d2 <- sapply(results, function(r) r$beta_cor[2])
  converged    <- sapply(results, `[[`, "conv")
  monotone     <- sapply(results, `[[`, "ll_monotone")
  elapsed      <- sapply(results, `[[`, "elapsed")

  cat(sprintf("Ideal point r:  mean=%.4f, sd=%.4f, min=%.4f\n",
              mean(mean_x_cors), sd(mean_x_cors), min(mean_x_cors)))
  cat(sprintf("Beta dim1 r:    mean=%.4f, sd=%.4f, min=%.4f\n",
              mean(beta_cors_d1), sd(beta_cors_d1), min(beta_cors_d1)))
  cat(sprintf("Beta dim2 r:    mean=%.4f, sd=%.4f, min=%.4f\n",
              mean(beta_cors_d2), sd(beta_cors_d2), min(beta_cors_d2)))
  cat(sprintf("Alpha r:        mean=%.4f, sd=%.4f, min=%.4f\n",
              mean(alpha_cors), sd(alpha_cors), min(alpha_cors)))
  cat(sprintf("Converged:      %d / %d\n", sum(converged), length(converged)))
  cat(sprintf("LL monotonic:   %d / %d\n", sum(monotone), length(monotone)))
  cat(sprintf("Runtime:        mean=%.1fs, total=%.1fs\n",
              mean(elapsed), sum(elapsed)))

  # Pass/fail assessment
  cat("\n--- PASS/FAIL ---\n")
  pass <- TRUE
  if (mean(mean_x_cors) < 0.90) {
    cat("FAIL: Mean ideal point correlation < 0.90\n"); pass <- FALSE
  } else {
    cat(sprintf("PASS: Mean ideal point correlation = %.4f >= 0.90\n",
                mean(mean_x_cors)))
  }
  if (mean(beta_cors_d1) < 0.85 || mean(beta_cors_d2) < 0.85) {
    cat("FAIL: Mean beta correlation < 0.85\n"); pass <- FALSE
  } else {
    cat(sprintf("PASS: Mean beta correlations = (%.4f, %.4f) >= 0.85\n",
                mean(beta_cors_d1), mean(beta_cors_d2)))
  }
  if (mean(alpha_cors) < 0.85) {
    cat("FAIL: Mean alpha correlation < 0.85\n"); pass <- FALSE
  } else {
    cat(sprintf("PASS: Mean alpha correlation = %.4f >= 0.85\n",
                mean(alpha_cors)))
  }
  if (!all(monotone)) {
    cat(sprintf("WARN: %d / %d replications had LL decreases\n",
                sum(!monotone), length(monotone)))
  } else {
    cat("PASS: All replications have monotone log-likelihood\n")
  }
  if (!all(converged)) {
    cat(sprintf("WARN: %d / %d replications did not converge\n",
                sum(!converged), length(converged)))
  }

  cat(sprintf("\nOverall: %s\n", ifelse(pass, "V1 PASSED", "V1 FAILED")))

  invisible(results)
}

# ============================================================
# Main execution
# ============================================================

if (!interactive() || exists(".run_v1")) {
  # First: single replication to verify correctness
  cat("Running single replication (seed=42) as smoke test...\n")
  res1 <- run_single_replication(seed = 42, N = 100, J = 200, T_sim = 6, K = 2,
                                  maxit = 500, thresh = 1e-6)

  cat("\n\nSmoke test complete. Run run_all_replications() for full study.\n")
}
