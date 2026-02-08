#!/usr/bin/env Rscript
set.seed(2026)

domain <- "environment"
K <- 2L

# Source components: prefer Rcpp wrapper; otherwise source pure R modules.
rcpp_ok <- tryCatch({
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
  TRUE
}, error = function(e) {
  cat("Rcpp failed, falling back to pure R\n")
  source("scripts/R/da_step.R")
  source("scripts/R/kalman.R")
  source("scripts/R/m_step.R")
  source("scripts/R/dynIRT_KD.R")
  FALSE
})

if (!exists("compute_loglik", mode = "function")) {
  source("scripts/R/dynIRT_KD.R")
}
if (!exists("param_change", mode = "function")) {
  param_change <- function(alpha_old, alpha_new, beta_old, beta_new, x_old, x_new) {
    max(max(abs(alpha_new - alpha_old)),
        max(abs(beta_new - beta_old)),
        max(abs(x_new - x_old)))
  }
}

flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

# Load Part A results
res_A <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
beta_A <- res_A$beta  # J x K
alpha_A <- res_A$alpha

# Select 3 anchor items from estimated betas
ratio1 <- abs(beta_A[, 1]) / (abs(beta_A[, 2]) + 0.01)
ratio2 <- abs(beta_A[, 2]) / (abs(beta_A[, 1]) + 0.01)
product <- abs(beta_A[, 1]) * abs(beta_A[, 2])

item_A <- which.max(ratio1)
item_B <- which.max(ratio2)

product2 <- product
product2[c(item_A, item_B)] <- -Inf
item_C <- which.max(product2)

anchor_items <- c(item_A, item_B, item_C)

fixed_betas <- beta_A[anchor_items, , drop = FALSE]
fixed_alpha <- alpha_A[anchor_items]

# PCA starts
rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA
rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) {
  m <- mean(rc_num[, j], na.rm = TRUE)
  if (is.nan(m)) m <- 0.5
  rc_num[is.na(rc_num[, j]), j] <- m
}
col_var <- apply(rc_num, 2, var)
rc_pca <- if (any(col_var == 0, na.rm = TRUE)) rc_num[, col_var > 0, drop = FALSE] else rc_num
pca <- prcomp(rc_pca, center = TRUE, scale. = FALSE)
npc <- min(K, ncol(pca$x))
x_pca <- pca$x[, seq_len(npc), drop = FALSE]
if (npc < K) x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
x_start <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) x_start[, , t] <- x_pca

# Diffuse priors for ALL countries (no country anchors)
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
x_Sigma0_list <- vector("list", N)
for (i in seq_len(N)) x_Sigma0_list[[i]] <- diag(x_Sigma0[i, ], nrow = K)

# Warm start from Part A
alpha <- as.numeric(alpha_A)
beta <- as.matrix(beta_A)
x_smooth <- x_start
Omega <- 0.1 * diag(K)

# Data setup
rc <- flow$rc
startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)
bill_session <- as.integer(flow$bill.session)

voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

dir.create("logs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/v7_item_anchors", recursive = TRUE, showWarnings = FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v7_%s_item_%s.log", domain, ts)
sink(log_file, split = TRUE)

cat(sprintf("V7 Item-Anchor: %s | N=%d, J=%d, T=%d, K=%d | rcpp_ok=%d\n",
            domain, N, J, T_periods, K, as.integer(rcpp_ok)))
cat(sprintf("Loaded Part A: outputs/v7_country_anchors/%s_results.rds\n", domain))
cat(sprintf("Anchor items: %s\n", paste(anchor_items, collapse = ", ")))
cat(sprintf("Anchor item labels: %s\n", paste(flow$item_labels[anchor_items], collapse = " | ")))
cat("Fixed alpha:\n"); print(fixed_alpha)
cat("Fixed betas:\n"); print(fixed_betas)
cat("\n")

maxit <- 5000L
checkfreq <- 50L
thresh <- 1e-4
thresh_loglik <- 0.01
loglik_patience <- 5L
ncores <- 4L

loglik_trace <- rep(NA_real_, maxit)
prev_ll <- -Inf
patience <- 0L
converged <- FALSE
final_iter <- maxit

t0 <- proc.time()

for (iter in seq_len(maxit)) {
  alpha_old <- alpha
  beta_old <- beta
  x_old <- x_smooth

  # ==== E-step ====
  da <- da_step(rc, alpha, beta, x_smooth, bill_session)

  P_smooth_all <- vector("list", N)
  x_prev <- x_smooth

  if (exists(".kalman_one_country", mode = "function")) {
    ks_results <- parallel::mclapply(seq_len(N), function(i) {
      .kalman_one_country(
        i = i,
        da_y_star = da$y_star,
        rc = rc,
        alpha = alpha,
        beta = beta,
        bill.session = bill_session,
        x_mu0 = x_mu0,
        x_Sigma0 = x_Sigma0_list,
        Omega = Omega,
        startlegis = startlegis,
        endlegis = endlegis,
        T_total = T_periods,
        K = K,
        x_prev_i = matrix(x_prev[i, , ], nrow = K, ncol = T_periods)
      )
    }, mc.cores = ncores)

    for (i in seq_len(N)) {
      res_i <- ks_results[[i]]
      x_smooth[i, , ] <- res_i$x_smooth_i
      P_smooth_all[[i]] <- res_i$P_smooth_i
    }
  } else {
    # Manual fallback
    for (i in seq_len(N)) {
      si <- startlegis[i]
      ei <- endlegis[i]
      obs_items <- which(rc[i, ] != 0L)
      x_smooth_i <- matrix(x_prev[i, , ], nrow = K, ncol = T_periods)
      P_smooth_i <- vector("list", T_periods)

      if (length(obs_items) > 0) {
        ks <- kalman_smoother_country(
          y_star_i = da$y_star[i, obs_items],
          item_indices = obs_items,
          alpha = alpha,
          beta = beta,
          bill.session = bill_session,
          mu0 = x_mu0[i, ],
          Sigma0 = x_Sigma0_list[[i]],
          Omega = Omega,
          start_t = si,
          end_t = ei,
          T_total = T_periods
        )
        T_active_i <- ei - si + 1L
        for (s in seq_len(T_active_i)) {
          t_global <- si + s
          x_smooth_i[, t_global] <- ks$x_smooth[, s]
          P_smooth_i[[t_global]] <- ks$P_smooth[[s]]
        }
      }

      for (tt in seq_len(T_periods)) {
        if (is.null(P_smooth_i[[tt]])) P_smooth_i[[tt]] <- diag(K) * 100
        if (any(is.na(x_smooth_i[, tt]))) {
          x_smooth_i[, tt] <- if (tt > 1L) x_smooth_i[, tt - 1L] else x_mu0[i, ]
        }
      }

      x_smooth[i, , ] <- x_smooth_i
      P_smooth_all[[i]] <- P_smooth_i
    }
  }

  # ==== M-step ====
  m_items <- m_step_items(
    y_star = da$y_star,
    rc = rc,
    x_smooth = x_smooth,
    P_smooth = P_smooth_all,
    bill.session = bill_session,
    beta_mu = rep(0, K + 1),
    beta_sigma = 25 * diag(K + 1),
    voters_by_item = voters_by_item
  )
  alpha <- m_items$alpha
  beta <- m_items$beta

  # Enforce item anchors: overwrite anchor params back to their fixed Part A values
  alpha[anchor_items] <- fixed_alpha
  beta[anchor_items, ] <- fixed_betas

  # ==== Convergence checks ====
  delta <- param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

  if (iter == 1L || iter %% checkfreq == 0L || delta < thresh || iter == maxit) {
    ll <- compute_loglik(rc, alpha, beta, x_smooth, bill_session)
    loglik_trace[iter] <- ll
    cat(sprintf("Iter %4d | LL = %.4f | delta = %.2e\n", iter, ll, delta))

    if (iter > 1L) {
      rel_change <- abs(ll - prev_ll) / (1 + abs(prev_ll))
      if (is.finite(rel_change) && rel_change < thresh_loglik) {
        patience <- patience + 1L
        if (patience >= loglik_patience) {
          cat(sprintf("Converged at iter %d (LL rel_change < %.3f for %d checks)\n",
                      iter, thresh_loglik, loglik_patience))
          converged <- TRUE
          final_iter <- iter
          break
        }
      } else {
        patience <- 0L
      }
    }
    prev_ll <- ll
  }

  if (delta < thresh) {
    cat(sprintf("Converged at iter %d (delta < %.2e)\n", iter, thresh))
    converged <- TRUE
    final_iter <- iter
    break
  }
}

elapsed_B <- (proc.time() - t0)["elapsed"]
loglik_trace <- loglik_trace[seq_len(final_iter)]
cat(sprintf("\nPart B done: %.1fs | Iters: %d | Conv: %d\n",
            elapsed_B, final_iter, as.integer(converged)))

# Mean ideal points over active periods
sl <- startlegis
el <- endlegis
x_mean <- matrix(NA_real_, N, K)
for (i in seq_len(N)) {
  s <- sl[i] + 1L
  e <- el[i] + 1L
  if (e >= s && s >= 1 && e <= T_periods) {
    x_mean[i, ] <- if (s == e) x_smooth[i, , s] else rowMeans(x_smooth[i, , s:e, drop = FALSE])
  }
}
rownames(x_mean) <- flow$country_codes
colnames(x_mean) <- paste0("dim", seq_len(K))

# Aggregate statistics per period (for paper)
agg <- data.frame(
  period = seq_len(T_periods),
  mean_dim1 = sapply(seq_len(T_periods), function(t) mean(x_smooth[, , t][, 1], na.rm = TRUE)),
  sd_dim1   = sapply(seq_len(T_periods), function(t) sd(x_smooth[, , t][, 1], na.rm = TRUE)),
  mean_dim2 = sapply(seq_len(T_periods), function(t) mean(x_smooth[, , t][, 2], na.rm = TRUE)),
  sd_dim2   = sapply(seq_len(T_periods), function(t) sd(x_smooth[, , t][, 2], na.rm = TRUE))
)
if (!is.null(flow$period_labels)) agg$period_label <- flow$period_labels
cat("\nAggregate trends:\n")
print(agg)

out_B <- list(
  domain = domain,
  strategy = "item_anchors_v7",
  ideal_points = x_smooth,
  ideal_points_mean = x_mean,
  alpha = alpha,
  beta = beta,
  country_codes = flow$country_codes,
  item_labels = flow$item_labels,
  period_labels = flow$period_labels,
  anchor_items = anchor_items,
  anchor_item_labels = flow$item_labels[anchor_items],
  fixed_betas = fixed_betas,
  fixed_alpha = fixed_alpha,
  aggregate = agg,
  runtime = list(
    seconds = as.numeric(elapsed_B),
    iters = final_iter,
    conv = as.integer(converged),
    loglik_trace = loglik_trace
  )
)
saveRDS(out_B, sprintf("outputs/v7_item_anchors/%s_results.rds", domain))
cat(sprintf("Saved: outputs/v7_item_anchors/%s_results.rds\n", domain))
sink()

