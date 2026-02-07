# Benchmark: m_step_items fast vs slow (vectorized sufficient stats)
# Output: outputs/optimization_EF/benchmark_m_step_summary.{rds,txt}

suppressPackageStartupMessages({
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required. Install it with install.packages('MASS').")
  }
})

source("scripts/R/m_step.R")

# ---- Slow reference implementation (pre-optimization) ----
m_step_items_slow <- function(y_star, rc, x_smooth, P_smooth,
                              bill.session, beta_mu, beta_sigma) {
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

# ---- Simulated benchmark data ----
set.seed(101)
N <- 120
J <- 300
T_total <- 4
K <- 2

x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))

# P_smooth: list of N, each list of T_total K x K matrices
P_smooth <- lapply(1:N, function(i) {
  lapply(1:T_total, function(t) 0.1 * diag(K))
})

bill.session <- sample(0:(T_total - 1), J, replace = TRUE)

# Votes and pseudo-observations
rc <- matrix(sample(c(-1L, 0L, 1L), N * J, replace = TRUE,
                    prob = c(0.4, 0.2, 0.4)), nrow = N)

y_star <- matrix(NA_real_, nrow = N, ncol = J)
nonmiss <- rc != 0L
y_star[nonmiss] <- rnorm(sum(nonmiss))

beta_mu <- rep(0, K + 1)
beta_sigma <- 25 * diag(K + 1)

# ---- Benchmark ----

voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

# Slow
pt_slow <- proc.time()
res_slow <- m_step_items_slow(y_star, rc, x_smooth, P_smooth,
                              bill.session, beta_mu, beta_sigma)
pt_slow <- proc.time() - pt_slow

# Fast (current)
pt_fast <- proc.time()
res_fast <- m_step_items(y_star, rc, x_smooth, P_smooth,
                         bill.session, beta_mu, beta_sigma,
                         voters_by_item = voters_by_item)
pt_fast <- proc.time() - pt_fast

max_alpha_diff <- max(abs(res_slow$alpha - res_fast$alpha))
max_beta_diff  <- max(abs(res_slow$beta - res_fast$beta))

speedup <- as.numeric(pt_slow["elapsed"]) / as.numeric(pt_fast["elapsed"])

summary <- list(
  seed = 101,
  N = N, J = J, T_total = T_total, K = K,
  time_slow = pt_slow,
  time_fast = pt_fast,
  speedup = speedup,
  max_alpha_diff = max_alpha_diff,
  max_beta_diff = max_beta_diff
)

output_dir <- "outputs/optimization_EF"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(summary, file.path(output_dir, "benchmark_m_step_summary.rds"))

summary_path <- file.path(output_dir, "benchmark_m_step_summary.txt")
cat(sprintf("Seed: %d\n", summary$seed), file = summary_path)
cat(sprintf("N=%d J=%d T=%d K=%d\n", N, J, T_total, K), file = summary_path, append = TRUE)
cat(sprintf("Slow elapsed: %.3f s\n", summary$time_slow[["elapsed"]]), file = summary_path, append = TRUE)
cat(sprintf("Fast elapsed: %.3f s\n", summary$time_fast[["elapsed"]]), file = summary_path, append = TRUE)
cat(sprintf("Speedup: %.2fx\n", speedup), file = summary_path, append = TRUE)
cat(sprintf("Max |alpha diff|: %.3e\n", max_alpha_diff), file = summary_path, append = TRUE)
cat(sprintf("Max |beta diff|: %.3e\n", max_beta_diff), file = summary_path, append = TRUE)

cat("Benchmark complete. Summary saved to outputs/optimization_EF.\n")
