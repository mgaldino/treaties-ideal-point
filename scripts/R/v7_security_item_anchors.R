#!/usr/bin/env Rscript
set.seed(2026)

K <- 2L
domain <- "security"

# ---- Sources (as required) ----
using_rcpp <- TRUE
ok <- try(source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R"), silent = TRUE)
if (inherits(ok, "try-error")) {
  using_rcpp <- FALSE
  message("WARNING: Rcpp compilation failed; will run pure R steps.")
}

source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")

if (!exists("compute_loglik", mode = "function")) {
  source("scripts/R/dynIRT_KD.R")
}

# Re-enable Rcpp-optimized da_step/m_step_items if available (after sourcing pure-R modules above)
if (using_rcpp) {
  ok2 <- try(source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R"), silent = TRUE)
  if (inherits(ok2, "try-error")) using_rcpp <- FALSE
}

flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

# Load Part A (V7) results to select anchor items and warm-start parameters
res_A <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
beta_A <- res_A$beta  # J x K

# Select 3 anchor items from estimated betas:
# A: highest |beta_1| / (|beta_2| + 0.01) -> dim1 specialist
# B: highest |beta_2| / (|beta_1| + 0.01) -> dim2 specialist
# C: highest |beta_1| * |beta_2| (excluding A and B) -> both-loader
ratio1 <- abs(beta_A[, 1]) / (abs(beta_A[, 2]) + 0.01)
idx_A <- which.max(ratio1)

ratio2 <- abs(beta_A[, 2]) / (abs(beta_A[, 1]) + 0.01)
ratio2[idx_A] <- -Inf
idx_B <- which.max(ratio2)

product <- abs(beta_A[, 1]) * abs(beta_A[, 2])
product[c(idx_A, idx_B)] <- -Inf
idx_C <- which.max(product)

anchor_items <- c(idx_A, idx_B, idx_C)
if (length(unique(anchor_items)) < 3) {
  # Fallback (should be rare): choose top-3 by total |beta|
  total_beta <- rowSums(abs(beta_A))
  anchor_items <- order(total_beta, decreasing = TRUE)[1:3]
}

fixed_betas <- beta_A[anchor_items, , drop = FALSE]
fixed_alpha <- res_A$alpha[anchor_items]

# ---- Initialization ----
# Diffuse priors for ALL countries (no country anchors)
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0_list <- vector("list", N)
for (i in seq_len(N)) x_Sigma0_list[[i]] <- diag(rep(1, K), nrow = K)

# Warm start alpha/beta from Part A; start x from Part A trajectory if present.
alpha <- as.numeric(res_A$alpha)
beta <- as.matrix(res_A$beta)
if (!is.null(res_A$ideal_points) && is.array(res_A$ideal_points)) {
  x_smooth <- res_A$ideal_points
} else {
  # PCA fallback (should not happen)
  rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }
  pca <- prcomp(rc_num, center = TRUE, scale. = FALSE)
  x_pca <- pca$x[, 1:K, drop = FALSE]
  for (k in 1:K) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_smooth <- array(NA_real_, dim = c(N, K, T_periods))
  for (t in 1:T_periods) x_smooth[, , t] <- x_pca
}

Omega <- 0.1 * diag(K)

# Data
rc <- flow$rc
startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)
bill_session <- as.integer(flow$bill.session)

# Precompute voters per item (static)
voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

# Kalman helper selection
kalman_one <- NULL
if (exists(".kalman_one_country", mode = "function")) {
  kalman_one <- .kalman_one_country
} else {
  # Manual loop: call kalman_smoother_country per i
  kalman_one <- function(i, da_y_star, rc, alpha, beta, bill.session,
                         x_mu0, x_Sigma0, Omega, startlegis, endlegis,
                         T_total, K, x_prev_i) {
    si <- startlegis[i]
    ei <- endlegis[i]
    obs_items <- which(rc[i, ] != 0L)

    if (length(obs_items) == 0) {
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
      y_star_i = y_star_i,
      item_indices = obs_items,
      alpha = alpha,
      beta = beta,
      bill.session = bill.session,
      mu0 = x_mu0[i, ],
      Sigma0 = x_Sigma0[[i]],
      Omega = Omega,
      start_t = si,
      end_t = ei,
      T_total = T_total
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
      if (is.null(P_smooth_i[[tt]])) P_smooth_i[[tt]] <- diag(K) * 100
      if (any(is.na(x_smooth_i[, tt]))) x_smooth_i[, tt] <- if (tt > 1L) x_smooth_i[, tt - 1L] else x_mu0[i, ]
    }
    list(
      x_smooth_i = x_smooth_i,
      P_smooth_i = P_smooth_i,
      P_lag_i    = ks$P_lag
    )
  }
}

dir.create("outputs/v7_item_anchors", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v7_%s_item_%s.log", domain, ts)
sink(log_file, split = TRUE)

cat(sprintf("V7 Item-Anchor: %s | N=%d, J=%d, T=%d, K=%d\n", domain, N, J, T_periods, K))
cat(sprintf("Part A warm-start: alpha/beta from outputs/v7_country_anchors/%s_results.rds\n", domain))
cat(sprintf("Selected anchor items (idx): %s\n", paste(anchor_items, collapse = ", ")))
cat(sprintf("Selected anchor items (labels): %s\n", paste(flow$item_labels[anchor_items], collapse = ", ")))
cat("Fixed anchor (alpha, beta):\n")
print(data.frame(
  item_idx = anchor_items,
  item_label = flow$item_labels[anchor_items],
  alpha = fixed_alpha,
  beta1 = fixed_betas[, 1],
  beta2 = fixed_betas[, 2]
))
cat("\nEstimation: diffuse country priors; item anchors fixed to Part A after each M-step\n\n")

maxit <- 5000L
checkfreq <- 50L
thresh_loglik <- 0.01
patience_target <- 5L
ncores <- 4L
if (.Platform$OS.type == "windows") ncores <- 1L

loglik_trace <- numeric(maxit)
prev_ll <- -Inf
patience <- 0L
converged <- FALSE
final_iter <- maxit

t0 <- proc.time()

for (iter in 1:maxit) {
  alpha_old <- alpha
  beta_old <- beta
  x_old <- x_smooth

  # ==== E-STEP ====
  da <- da_step(rc, alpha, beta, x_smooth, bill_session)

  P_smooth_all <- vector("list", N)
  x_prev <- x_smooth

  if (ncores > 1L) {
    ks_results <- parallel::mclapply(seq_len(N), function(i) {
      kalman_one(
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
  } else {
    ks_results <- lapply(seq_len(N), function(i) {
      kalman_one(
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
    })
  }

  for (i in seq_len(N)) {
    res_i <- ks_results[[i]]
    x_smooth[i, , ] <- res_i$x_smooth_i
    P_smooth_all[[i]] <- res_i$P_smooth_i
  }

  # ==== M-STEP ====
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

  # Enforce item anchors (fixed to Part A values)
  alpha[anchor_items] <- fixed_alpha
  beta[anchor_items, ] <- fixed_betas

  # ==== Convergence checks ====
  delta <- if (exists("param_change", mode = "function")) {
    param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)
  } else {
    max(
      max(abs(alpha - alpha_old)),
      max(abs(beta - beta_old)),
      max(abs(x_smooth - x_old))
    )
  }

  need_ll <- (iter == 1L) || (iter %% checkfreq == 0L) || (iter == maxit)
  if (need_ll) {
    ll <- compute_loglik(rc, alpha, beta, x_smooth, bill_session)
    loglik_trace[iter] <- ll
    cat(sprintf("Iter %4d | LL = %.4f | delta = %.2e\n", iter, ll, delta))

    if (iter > 1L && is.finite(prev_ll)) {
      rel_change <- abs(ll - prev_ll) / (1 + abs(prev_ll))
      if (is.finite(rel_change) && rel_change < thresh_loglik) {
        patience <- patience + 1L
      } else {
        patience <- 0L
      }
      if (patience >= patience_target) {
        converged <- TRUE
        final_iter <- iter
        cat(sprintf("Converged at iter %d (LL rel_change < %.3f for %d checks)\n",
                    iter, thresh_loglik, patience_target))
        break
      }
    }
    prev_ll <- ll
  } else {
    loglik_trace[iter] <- prev_ll
  }

  if (delta < 1e-4) {
    converged <- TRUE
    final_iter <- iter
    cat(sprintf("Converged at iter %d (delta < 1e-4)\n", iter))
    break
  }
}

elapsed_B <- (proc.time() - t0)["elapsed"]
loglik_trace <- loglik_trace[1:final_iter]
cat(sprintf("\nPart B done: %.1fs | Iters: %d | Conv: %d\n",
            as.numeric(elapsed_B), final_iter, as.integer(converged)))

# Mean ideal points across active periods
sl <- startlegis
el <- endlegis
x_mean <- matrix(NA_real_, N, K)
for (i in 1:N) {
  s <- sl[i] + 1L
  e <- el[i] + 1L
  if (e >= s && s >= 1L && e <= T_periods) {
    x_mean[i, ] <- if (s == e) x_smooth[i, , s] else rowMeans(x_smooth[i, , s:e, drop = FALSE])
  }
}
rownames(x_mean) <- flow$country_codes

# Aggregate statistics per period (for paper)
agg <- data.frame(
  period = 1:T_periods,
  mean_dim1 = sapply(1:T_periods, function(t) mean(x_smooth[, , t][, 1], na.rm = TRUE)),
  sd_dim1 = sapply(1:T_periods, function(t) sd(x_smooth[, , t][, 1], na.rm = TRUE)),
  mean_dim2 = sapply(1:T_periods, function(t) mean(x_smooth[, , t][, 2], na.rm = TRUE)),
  sd_dim2 = sapply(1:T_periods, function(t) sd(x_smooth[, , t][, 2], na.rm = TRUE))
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

