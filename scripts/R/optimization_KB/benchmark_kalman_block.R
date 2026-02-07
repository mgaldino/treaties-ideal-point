# Benchmark: kalman_smoother_country block vs sequential (reference)
# Outputs: outputs/optimization_KB/benchmark_kalman_summary.{rds,txt}

source("scripts/R/kalman.R")

# Reference sequential implementation (pre-block refactor)
kalman_smoother_country_seq <- function(y_star_i, item_indices, alpha, beta,
                                        bill.session, mu0, Sigma0, Omega,
                                        start_t, end_t, T_total) {
  K <- length(mu0)
  T_active <- end_t - start_t + 1L
  I_K <- diag(K)

  items_by_period <- vector("list", T_active)
  for (idx in seq_along(item_indices)) {
    j <- item_indices[idx]
    t_item <- bill.session[j]
    local_t <- t_item - start_t + 1L
    if (local_t >= 1L && local_t <= T_active) {
      items_by_period[[local_t]] <- c(items_by_period[[local_t]], idx)
    }
  }

  x_filt <- matrix(0, nrow = K, ncol = T_active)
  P_filt <- vector("list", T_active)
  x_pred_store <- matrix(0, nrow = K, ncol = T_active)
  P_pred_store <- vector("list", T_active)

  for (s in seq_len(T_active)) {
    if (s == 1L) {
      x_hat <- mu0
      P_hat <- Sigma0
    } else {
      x_hat <- x_filt[, s - 1L]
      P_hat <- P_filt[[s - 1L]] + Omega
    }

    x_pred_store[, s] <- x_hat
    P_pred_store[[s]] <- P_hat

    obs_idx <- items_by_period[[s]]
    if (length(obs_idx) > 0) {
      for (idx in obs_idx) {
        j <- item_indices[idx]
        beta_j <- beta[j, ]

        innovation <- y_star_i[idx] - alpha[j] - sum(beta_j * x_hat)
        S <- as.numeric(crossprod(beta_j, P_hat %*% beta_j)) + 1.0
        K_gain <- (P_hat %*% beta_j) / S

        x_hat <- x_hat + as.numeric(K_gain) * innovation

        IKb <- I_K - K_gain %*% t(beta_j)
        P_hat <- IKb %*% P_hat %*% t(IKb) + K_gain %*% t(K_gain)
        P_hat <- (P_hat + t(P_hat)) / 2
      }
    }

    x_filt[, s] <- x_hat
    P_filt[[s]] <- P_hat
  }

  x_smooth <- matrix(0, nrow = K, ncol = T_active)
  P_smooth <- vector("list", T_active)
  L_store  <- vector("list", T_active)

  x_smooth[, T_active] <- x_filt[, T_active]
  P_smooth[[T_active]] <- P_filt[[T_active]]

  if (T_active > 1L) {
    for (s in (T_active - 1L):1L) {
      P_pred_next <- P_pred_store[[s + 1L]]
      L_t <- P_filt[[s]] %*% solve(P_pred_next)
      x_smooth[, s] <- x_filt[, s] +
        L_t %*% (x_smooth[, s + 1L] - x_pred_store[, s + 1L])
      P_smooth[[s]] <- P_filt[[s]] +
        L_t %*% (P_smooth[[s + 1L]] - P_pred_next) %*% t(L_t)
      P_smooth[[s]] <- (P_smooth[[s]] + t(P_smooth[[s]])) / 2
      L_store[[s]] <- L_t
    }
  }

  P_lag <- vector("list", max(T_active - 1L, 0L))
  if (T_active > 1L) {
    for (s in 2L:T_active) {
      P_lag[[s - 1L]] <- L_store[[s - 1L]] %*% P_smooth[[s]]
    }
  }

  list(
    x_smooth = x_smooth,
    P_smooth = P_smooth,
    P_lag    = P_lag
  )
}

# ---- Simulated benchmark data ----
set.seed(303)
K <- 2
T_total <- 6
n_items <- 300

alpha <- rnorm(n_items)
beta <- matrix(rnorm(n_items * K), nrow = n_items, ncol = K)
bill.session <- rep(0:(T_total - 1), length.out = n_items)

mu0 <- rep(0, K)
Sigma0 <- diag(K)
Omega <- 0.1 * diag(K)

y_star_i <- rnorm(n_items)
item_indices <- seq_len(n_items)

# ---- Benchmark ----
pt_seq <- proc.time()
res_seq <- kalman_smoother_country_seq(
  y_star_i     = y_star_i,
  item_indices = item_indices,
  alpha        = alpha,
  beta         = beta,
  bill.session = bill.session,
  mu0          = mu0,
  Sigma0       = Sigma0,
  Omega        = Omega,
  start_t      = 0L,
  end_t        = T_total - 1L,
  T_total      = T_total
)
pt_seq <- proc.time() - pt_seq

pt_blk <- proc.time()
res_blk <- kalman_smoother_country(
  y_star_i     = y_star_i,
  item_indices = item_indices,
  alpha        = alpha,
  beta         = beta,
  bill.session = bill.session,
  mu0          = mu0,
  Sigma0       = Sigma0,
  Omega        = Omega,
  start_t      = 0L,
  end_t        = T_total - 1L,
  T_total      = T_total
)
pt_blk <- proc.time() - pt_blk

speedup <- as.numeric(pt_seq["elapsed"]) / as.numeric(pt_blk["elapsed"])

max_x_diff <- max(abs(res_seq$x_smooth - res_blk$x_smooth))
max_P_diff <- max(sapply(seq_along(res_seq$P_smooth), function(i) {
  max(abs(res_seq$P_smooth[[i]] - res_blk$P_smooth[[i]]))
}))

summary <- list(
  seed = 303,
  K = K, T_total = T_total, n_items = n_items,
  time_seq = pt_seq,
  time_blk = pt_blk,
  speedup = speedup,
  max_x_diff = max_x_diff,
  max_P_diff = max_P_diff
)

output_dir <- "outputs/optimization_KB"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(summary, file.path(output_dir, "benchmark_kalman_summary.rds"))

summary_path <- file.path(output_dir, "benchmark_kalman_summary.txt")
cat(sprintf("Seed: %d\n", summary$seed), file = summary_path)
cat(sprintf("K=%d T=%d n_items=%d\n", K, T_total, n_items), file = summary_path, append = TRUE)
cat(sprintf("Sequential elapsed: %.3f s\n", summary$time_seq[["elapsed"]]), file = summary_path, append = TRUE)
cat(sprintf("Block elapsed: %.3f s\n", summary$time_blk[["elapsed"]]), file = summary_path, append = TRUE)
cat(sprintf("Speedup: %.2fx\n", speedup), file = summary_path, append = TRUE)
cat(sprintf("Max |x diff|: %.3e\n", max_x_diff), file = summary_path, append = TRUE)
cat(sprintf("Max |P diff|: %.3e\n", max_P_diff), file = summary_path, append = TRUE)

cat("Benchmark complete. Summary saved to outputs/optimization_KB.\n")
