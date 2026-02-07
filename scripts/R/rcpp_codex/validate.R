#!/usr/bin/env Rscript

# Validation: R vs Rcpp equivalence + run tests against Rcpp

set.seed(2026)

# ---- Helper: small simulated dataset (adapted from tests) ----
simulate_small_data <- function(N = 20, J = 30, T_total = 3, K = 2, seed = 42) {
  set.seed(seed)
  Omega_true <- 0.1 * diag(K)
  x_true <- array(NA_real_, dim = c(N, K, T_total))
  x_true[, , 1] <- MASS::mvrnorm(N, mu = rep(0, K), Sigma = diag(K))
  for (t in 2:T_total) {
    x_true[, , t] <- x_true[, , t - 1] +
      MASS::mvrnorm(N, mu = rep(0, K), Sigma = Omega_true)
  }

  alpha_true <- rnorm(J, 0, 0.5)
  beta_true  <- matrix(rnorm(J * K, 0, 0.7), nrow = J, ncol = K)
  bill.session <- sample(0:(T_total - 1), J, replace = TRUE)

  rc <- matrix(0L, nrow = N, ncol = J)
  for (j in seq_len(J)) {
    t_j <- bill.session[j] + 1L
    x_tj <- matrix(x_true[, , t_j], nrow = N, ncol = K)
    mu_star <- alpha_true[j] + x_tj %*% beta_true[j, ]
    rc[, j] <- ifelse(rnorm(N) < mu_star, 1L, -1L)
  }

  missing_mask <- matrix(rbinom(N * J, 1, 0.2), nrow = N, ncol = J)
  rc[missing_mask == 1] <- 0L

  list(
    rc = rc,
    startlegis = matrix(rep(0L, N), ncol = 1),
    endlegis   = matrix(rep(T_total - 1L, N), ncol = 1),
    bill.session = matrix(bill.session, ncol = 1),
    T = T_total,
    K = K,
    N = N,
    J = J
  )
}

# ---- Run R version ----
source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD.R")

sim <- simulate_small_data(N = 15, J = 20, T_total = 3, K = 2, seed = 123)
K <- sim$K

x_start <- array(0, dim = c(sim$N, K, sim$T))
alpha_start <- rep(0, sim$J)
beta_start  <- matrix(rnorm(sim$J * K, 0, 0.1), nrow = sim$J, ncol = K)

.data <- list(
  rc = sim$rc,
  startlegis = sim$startlegis,
  endlegis   = sim$endlegis,
  bill.session = sim$bill.session,
  T = sim$T
)

.priors <- list(
  x.mu0    = matrix(0, nrow = sim$N, ncol = K),
  x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),
  beta.mu    = rep(0, K + 1),
  beta.sigma = 25 * diag(K + 1),
  omega      = 0.1 * diag(K)
)

.control <- list(
  verbose = FALSE,
  thresh  = 1e-6,
  maxit   = 10L,
  estimate_omega = FALSE
)

res_r <- dynIRT_KD(.data, list(alpha = alpha_start, beta = beta_start, x = x_start),
                   .priors, .control, K = K)

# ---- Run Rcpp version ----
source("scripts/R/rcpp_codex/dynIRT_KD_rcpp.R")
res_rcpp <- dynIRT_KD(.data, list(alpha = alpha_start, beta = beta_start, x = x_start),
                      .priors, .control, K = K)

# ---- Compare ----
max_diff_alpha <- max(abs(res_r$means$alpha - res_rcpp$means$alpha))
max_diff_beta  <- max(abs(res_r$means$beta - res_rcpp$means$beta))
max_diff_x     <- max(abs(res_r$means$x - res_rcpp$means$x))
max_diff <- max(max_diff_alpha, max_diff_beta, max_diff_x)

cat(sprintf("Max abs diff (alpha): %.3e\n", max_diff_alpha))
cat(sprintf("Max abs diff (beta) : %.3e\n", max_diff_beta))
cat(sprintf("Max abs diff (x)    : %.3e\n", max_diff_x))
cat(sprintf("Max abs diff overall: %.3e\n", max_diff))

if (!is.finite(max_diff) || max_diff > 1e-10) {
  stop(sprintf("R vs Rcpp mismatch: max diff %.3e exceeds tolerance", max_diff))
}

# ---- Run test suite against Rcpp ----
run_test_file_rcpp <- function(path) {
  lines <- readLines(path)
  # remove source() lines to avoid overriding Rcpp functions
  lines <- lines[!grepl("^source\\(", lines)]
  lines <- lines[!grepl("source\\(file.path", lines)]
  lines <- lines[!grepl("source\\(file.path\\(\"..\", \\\"scripts\\\"", lines)]
  env <- new.env(parent = globalenv())
  eval(parse(text = lines), envir = env)
}

cat("\nRunning tests against Rcpp versions...\n")
run_test_file_rcpp("tests/test_da_step.R")
run_test_file_rcpp("tests/test_kalman.R")
run_test_file_rcpp("tests/test_m_step.R")
run_test_file_rcpp("tests/test_dynIRT_KD.R")

cat("All Rcpp tests passed.\n")
