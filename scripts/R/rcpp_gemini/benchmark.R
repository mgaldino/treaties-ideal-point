#!/usr/bin/env Rscript
# Benchmark script: Pure R vs Rcpp
# Usage: Rscript scripts/R/rcpp_gemini/benchmark.R

# Ensure we are in the project root
if (!dir.exists("scripts/R")) {
  stop("Please run from the project root directory")
}

# Settings
domain <- "investment"
K <- 2L
maxit <- 50L
verbose <- TRUE

cat(sprintf("Benchmarking %s with K=%d, maxit=%d\n", domain, K, maxit))

# Load data
flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
if (!file.exists(flow_path)) stop("Missing: ", flow_path)
flow <- readRDS(flow_path)

N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
cat(sprintf("Data: N=%d, J=%d, T=%d\n", N, J, T_periods))

# Prepare inputs
sl <- as.integer(flow$startlegis)
el <- as.integer(flow$endlegis)
bs <- as.integer(flow$bill.session)

# Anchors (Investment: DNK, IRN, CHN)
anchor_iso <- c("DNK", "IRN", "CHN")
anchor_idx <- match(anchor_iso, flow$country_codes)
anchor_positions <- rbind(
  c(+2, +2),
  c(-2, -2),
  c(+1, -1)
)

# Priors
x_mu0    <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ]    <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

# Starts
set.seed(2026)
build_pca_starts_2d <- function(rc, T_periods, K = 2L) {
  N <- nrow(rc); J <- ncol(rc)
  rc_num <- matrix(as.numeric(rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }
  pca <- prcomp(rc_num, center = TRUE, scale. = FALSE)
  npc <- min(K, ncol(pca$x))
  x_pca <- pca$x[, seq_len(npc), drop = FALSE]
  if (npc < K) {
    x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
  }
  for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_start <- array(NA_real_, dim = c(N, K, T_periods))
  for (t in seq_len(T_periods)) x_start[, , t] <- x_pca
  list(
    x     = x_start,
    alpha = numeric(J),
    beta  = matrix(rnorm(J * K, 0, 0.1), nrow = J, ncol = K)
  )
}
starts <- build_pca_starts_2d(flow$rc, T_periods, K)

data_list <- list(
  rc           = flow$rc,
  startlegis   = matrix(sl, ncol = 1),
  endlegis     = matrix(el, ncol = 1),
  bill.session = matrix(bs, ncol = 1),
  T            = T_periods
)
priors_list <- list(
  x.mu0      = x_mu0,
  x.Sigma0   = x_Sigma0,
  beta.mu    = rep(0, K + 1),
  beta.sigma = 25 * diag(K + 1),
  omega      = 0.1 * diag(K)
)
control_list <- list(
  verbose        = FALSE, # Reduce noise
  thresh         = 1e-6,
  maxit          = maxit,
  checkfreq      = 50L,
  estimate_omega = FALSE,
  ncores         = 4L
)

# --- RUN 1: Pure R ---
cat("\n--- Running Pure R ---\n")
# Source originals to be sure
env_r <- new.env()
sys.source("scripts/R/da_step.R", envir = env_r)
sys.source("scripts/R/m_step.R", envir = env_r)
sys.source("scripts/R/kalman.R", envir = env_r)
sys.source("scripts/R/dynIRT_KD.R", envir = env_r)

# We need to execute dynIRT_KD in an environment where da_step is the R one.
# dynIRT_KD calls da_step directly.
# We can temporarily assign them to globalenv or modify dynIRT_KD's environment.
# Easiest: assign to globalenv
da_step <<- env_r$da_step
m_step_items <<- env_r$m_step_items
kalman_smoother_country <<- env_r$kalman_smoother_country
m_step_Omega <<- env_r$m_step_Omega
truncnorm_moments <<- env_r$truncnorm_moments
dynIRT_KD <<- env_r$dynIRT_KD

t0_r <- proc.time()
res_r <- dynIRT_KD(
  .data    = data_list,
  .starts  = list(alpha = starts$alpha, beta = starts$beta, x = starts$x),
  .priors  = priors_list,
  .control = control_list,
  K        = K
)
elapsed_r <- (proc.time() - t0_r)["elapsed"]
cat(sprintf("Pure R time: %.2f s\n", elapsed_r))


# --- RUN 2: Rcpp ---
cat("\n--- Running Rcpp ---\n")
source("scripts/R/rcpp_gemini/dynIRT_KD_rcpp.R")
# This sources the file which overwrites da_step and m_step_items in global env

t0_cpp <- proc.time()
res_cpp <- dynIRT_KD(
  .data    = data_list,
  .starts  = list(alpha = starts$alpha, beta = starts$beta, x = starts$x),
  .priors  = priors_list,
  .control = control_list,
  K        = K
)
elapsed_cpp <- (proc.time() - t0_cpp)["elapsed"]
cat(sprintf("Rcpp time:   %.2f s\n", elapsed_cpp))

# --- Summary ---
speedup <- elapsed_r / elapsed_cpp
cat(sprintf("\nSpeedup: %.2fx\n", speedup))

# Save results
results <- list(
  time_r = elapsed_r,
  time_rcpp = elapsed_cpp,
  speedup = speedup,
  res_r_loglik = res_r$runtime$loglik_trace[maxit],
  res_cpp_loglik = res_cpp$runtime$loglik_trace[maxit]
)
saveRDS(results, "outputs/rcpp_gemini_benchmark.rds")
cat("Saved to outputs/rcpp_gemini_benchmark.rds\n")