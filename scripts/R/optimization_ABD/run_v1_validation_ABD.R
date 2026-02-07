# V1 validation for optimization ABD (sequential vs parallel)

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

evaluate_recovery <- function(sim, result, anchors) {
  N <- sim$N; J <- sim$J; K <- sim$K; T_sim <- sim$T_sim

  x_cor <- matrix(NA, nrow = T_sim, ncol = K)
  for (t in 1:T_sim) {
    for (k in 1:K) {
      non_anchor <- setdiff(1:N, anchors$idx)
      x_cor[t, k] <- cor(sim$x_true[non_anchor, k, t],
                         result$means$x[non_anchor, k, t])
    }
  }

  beta_cor <- numeric(K)
  for (k in 1:K) {
    beta_cor[k] <- cor(sim$beta_true[, k], result$means$beta[, k])
  }

  alpha_cor <- cor(sim$alpha_true, result$means$alpha)

  list(
    x_cor = x_cor,
    mean_x_cor = mean(x_cor),
    beta_cor = beta_cor,
    alpha_cor = alpha_cor
  )
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

# Detect cores and use half
ncores_detected <- parallel::detectCores()
if (is.na(ncores_detected) || ncores_detected < 1L) ncores_detected <- 1L
ncores_half <- max(1L, floor(ncores_detected / 2))

# ---- Run sequential ----

.control_seq <- list(
  threads        = 1L,
  verbose        = FALSE,
  thresh         = thresh,
  maxit          = maxit,
  checkfreq      = 50L,
  estimate_omega = FALSE,
  thresh_aitken  = NULL,
  ncores         = 1L
)

cat(sprintf("Detected cores: %d | Using ncores_half: %d\n", ncores_detected, ncores_half))

cat("Running sequential (ncores=1)...\n")
res_seq <- dynIRT_KD(.data, .starts, .priors, .control_seq, K = K)

# ---- Run parallel ----

.control_par <- .control_seq
.control_par$ncores <- ncores_half

cat(sprintf("Running parallel (ncores=%d)...\n", ncores_half))
res_par <- dynIRT_KD(.data, .starts, .priors, .control_par, K = K)

# ---- Equivalence check ----

x_equal <- isTRUE(all.equal(res_seq$means$x, res_par$means$x, tolerance = 1e-12))
alpha_equal <- isTRUE(all.equal(res_seq$means$alpha, res_par$means$alpha, tolerance = 1e-12))
beta_equal <- isTRUE(all.equal(res_seq$means$beta, res_par$means$beta, tolerance = 1e-12))

# ---- Recovery metrics (sequential) ----

rec_seq <- evaluate_recovery(sim, res_seq, anchors)
rec_par <- evaluate_recovery(sim, res_par, anchors)

summary <- list(
  seed = seed,
  N = N, J = J, T_sim = T_sim, K = K,
  ncores_detected = ncores_detected,
  ncores_half = ncores_half,
  runtime_seq = res_seq$runtime,
  runtime_par = res_par$runtime,
  equivalence = list(
    x_equal = x_equal,
    alpha_equal = alpha_equal,
    beta_equal = beta_equal
  ),
  recovery_seq = rec_seq,
  recovery_par = rec_par
)

output_dir <- "outputs/optimization_ABD"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(summary, file.path(output_dir, "v1_validation_summary.rds"))

# Write a small text summary
summary_path <- file.path(output_dir, "v1_validation_summary.txt")
cat(sprintf("Seed: %d\n", seed), file = summary_path)
cat(sprintf("Detected cores: %d\n", ncores_detected), file = summary_path, append = TRUE)
cat(sprintf("ncores_half: %d\n", ncores_half), file = summary_path, append = TRUE)
cat(sprintf("Equivalence: x=%s, alpha=%s, beta=%s\n",
            x_equal, alpha_equal, beta_equal), file = summary_path, append = TRUE)
cat(sprintf("Seq runtime: iters=%d, conv=%d, seconds=%.2f, ncores_used=%d\n",
            res_seq$runtime$iters, res_seq$runtime$conv, res_seq$runtime$seconds,
            res_seq$runtime$ncores_used), file = summary_path, append = TRUE)
cat(sprintf("Par runtime: iters=%d, conv=%d, seconds=%.2f, ncores_used=%d\n",
            res_par$runtime$iters, res_par$runtime$conv, res_par$runtime$seconds,
            res_par$runtime$ncores_used), file = summary_path, append = TRUE)
cat(sprintf("Mean x corr (seq): %.4f\n", rec_seq$mean_x_cor), file = summary_path, append = TRUE)
cat(sprintf("Mean x corr (par): %.4f\n", rec_par$mean_x_cor), file = summary_path, append = TRUE)

cat("Validation complete. Summary saved to outputs/optimization_ABD.\n")
