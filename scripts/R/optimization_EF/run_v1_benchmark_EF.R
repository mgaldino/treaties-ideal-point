# End-to-end benchmark: dynIRT_KD slow vs fast M-step (V1 simulation)
# Outputs: outputs/optimization_EF/v1_benchmark_summary.{rds,txt}

suppressPackageStartupMessages({
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required. Install it with install.packages('MASS').")
  }
})

source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD.R")

# ---- Helper functions (from v1_simulation_study.R) ----

simulate_data <- function(seed, N = 100, J = 200, T_sim = 6, K = 2) {
  set.seed(seed)

  Omega_true <- 0.1 * diag(K)
  x_true <- array(NA, dim = c(N, K, T_sim))
  x_true[, , 1] <- MASS::mvrnorm(N, mu = rep(0, K), Sigma = diag(K))
  for (t in 2:T_sim) {
    x_true[, , t] <- x_true[, , t - 1] +
      MASS::mvrnorm(N, mu = rep(0, K), Sigma = Omega_true)
  }

  alpha_true <- rnorm(J, 0, 1)
  beta_true  <- matrix(rnorm(J * K, 0, 0.7), nrow = J, ncol = K)

  bill_session <- sample(0:(T_sim - 1), J, replace = TRUE)

  rc <- matrix(0L, nrow = N, ncol = J)
  for (j in 1:J) {
    t_j <- bill_session[j] + 1L
    mu_star <- alpha_true[j] + x_true[, , t_j] %*% beta_true[j, ]
    rc[, j] <- ifelse(rnorm(N) < mu_star, 1L, -1L)
  }

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

select_anchors <- function(x_true, K = 2) {
  targets <- rbind(
    c(+2, +2),
    c(-2, -2),
    c(+1, -1)
  )
  if (K != 2) stop("Anchor selection only implemented for K=2")

  N <- nrow(x_true)
  x_t1 <- matrix(x_true[, , 1], nrow = N, ncol = K)

  anchor_idx <- integer(3)
  for (a in 1:3) {
    dists <- rowSums(sweep(x_t1, 2, targets[a, ])^2)
    dists[anchor_idx[anchor_idx > 0]] <- Inf
    anchor_idx[a] <- which.min(dists)
  }

  list(idx = anchor_idx, positions = targets)
}

# ---- Slow reference implementation (pre-optimization) ----

m_step_items_slow <- function(y_star, rc, x_smooth, P_smooth,
                              bill.session, beta_mu, beta_sigma,
                              voters_by_item = NULL) {
  N <- nrow(rc)
  J <- ncol(rc)
  K <- dim(x_smooth)[2]
  Kp1 <- K + 1L

  alpha_new <- numeric(J)
  beta_new  <- matrix(0, nrow = J, ncol = K)

  Sigma_beta_inv <- solve(beta_sigma)
  Sigma_beta_inv_mu <- Sigma_beta_inv %*% beta_mu

  for (j in seq_len(J)) {
    voters <- which(rc[, j] != 0L)
    if (length(voters) == 0) next

    t_j <- bill.session[j] + 1L

    Sigma_zz <- matrix(0, nrow = Kp1, ncol = Kp1)
    Sigma_zy <- numeric(Kp1)

    for (i in voters) {
      x_it <- x_smooth[i, , t_j]
      P_it <- P_smooth[[i]][[t_j]]

      Ez_outer <- matrix(0, nrow = Kp1, ncol = Kp1)
      Ez_outer[1, 1] <- 1
      Ez_outer[1, 2:Kp1] <- x_it
      Ez_outer[2:Kp1, 1] <- x_it
      Ez_outer[2:Kp1, 2:Kp1] <- P_it + tcrossprod(x_it)

      Sigma_zz <- Sigma_zz + Ez_outer

      Ez <- c(1, x_it)
      Sigma_zy <- Sigma_zy + y_star[i, j] * Ez
    }

    A <- Sigma_beta_inv + Sigma_zz
    b <- Sigma_beta_inv_mu + Sigma_zy
    gamma_hat <- solve(A, b)

    alpha_new[j]  <- gamma_hat[1]
    beta_new[j, ] <- gamma_hat[2:Kp1]
  }

  list(alpha = alpha_new, beta = beta_new)
}

# ---- Setup ----

seed <- 42
N <- 100; J <- 200; T_sim <- 6; K <- 2
maxit <- 500; thresh <- 1e-6

sim <- simulate_data(seed, N, J, T_sim, K)
anchors <- select_anchors(sim$x_true, K)

x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchors$idx)) {
  i <- anchors$idx[a]
  x_mu0[i, ] <- anchors$positions[a, ]
  x_Sigma0[i, ] <- 0.01
}

x_start <- sim$x_true + array(rnorm(N * K * T_sim, 0, 0.5),
                              dim = c(N, K, T_sim))
for (a in seq_along(anchors$idx)) {
  i <- anchors$idx[a]
  for (t in 1:T_sim) {
    x_start[i, , t] <- anchors$positions[a, ]
  }
}

alpha_start <- rnorm(J, 0, 0.5)
beta_start  <- matrix(rnorm(J * K, 0, 0.3), nrow = J, ncol = K)

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

# Detect cores and use up to 8
ncores_detected <- parallel::detectCores()
if (is.na(ncores_detected) || ncores_detected < 1L) ncores_detected <- 1L
ncores_target <- min(8L, ncores_detected)
if (ncores_target < 1L) ncores_target <- 1L

.control_common <- list(
  threads        = 1L,
  verbose        = FALSE,
  thresh         = thresh,
  maxit          = maxit,
  checkfreq      = 50L,
  estimate_omega = FALSE,
  thresh_aitken  = NULL,
  ncores         = ncores_target
)

cat(sprintf("Detected cores: %d | Using ncores_target: %d\n",
            ncores_detected, ncores_target))

# ---- Run slow (baseline) ----

m_step_items_fast <- m_step_items
assign("m_step_items", m_step_items_slow, envir = .GlobalEnv)

cat("Running baseline (slow m_step_items)...\n")
res_slow <- dynIRT_KD(.data, .starts, .priors, .control_common, K = K)

# ---- Run fast ----

assign("m_step_items", m_step_items_fast, envir = .GlobalEnv)
cat("Running optimized (fast m_step_items)...\n")
res_fast <- dynIRT_KD(.data, .starts, .priors, .control_common, K = K)

# ---- Equivalence checks ----

x_equal <- isTRUE(all.equal(res_slow$means$x, res_fast$means$x, tolerance = 1e-10))
alpha_equal <- isTRUE(all.equal(res_slow$means$alpha, res_fast$means$alpha, tolerance = 1e-10))
beta_equal <- isTRUE(all.equal(res_slow$means$beta, res_fast$means$beta, tolerance = 1e-10))

speedup <- res_slow$runtime$seconds / res_fast$runtime$seconds

summary <- list(
  seed = seed,
  N = N, J = J, T_sim = T_sim, K = K,
  ncores_detected = ncores_detected,
  ncores_target = ncores_target,
  runtime_slow = res_slow$runtime,
  runtime_fast = res_fast$runtime,
  equivalence = list(
    x_equal = x_equal,
    alpha_equal = alpha_equal,
    beta_equal = beta_equal
  ),
  speedup = speedup
)

output_dir <- "outputs/optimization_EF"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(summary, file.path(output_dir, "v1_benchmark_summary.rds"))

summary_path <- file.path(output_dir, "v1_benchmark_summary.txt")
cat(sprintf("Seed: %d\n", seed), file = summary_path)
cat(sprintf("Detected cores: %d\n", ncores_detected), file = summary_path, append = TRUE)
cat(sprintf("ncores_target: %d\n", ncores_target), file = summary_path, append = TRUE)
cat(sprintf("Equivalence: x=%s, alpha=%s, beta=%s\n",
            x_equal, alpha_equal, beta_equal), file = summary_path, append = TRUE)
cat(sprintf("Slow runtime: iters=%d, conv=%d, seconds=%.2f, ncores_used=%d\n",
            res_slow$runtime$iters, res_slow$runtime$conv, res_slow$runtime$seconds,
            res_slow$runtime$ncores_used), file = summary_path, append = TRUE)
cat(sprintf("Fast runtime: iters=%d, conv=%d, seconds=%.2f, ncores_used=%d\n",
            res_fast$runtime$iters, res_fast$runtime$conv, res_fast$runtime$seconds,
            res_fast$runtime$ncores_used), file = summary_path, append = TRUE)
cat(sprintf("Speedup: %.2fx\n", speedup), file = summary_path, append = TRUE)

cat("Benchmark complete. Summary saved to outputs/optimization_EF.\n")
