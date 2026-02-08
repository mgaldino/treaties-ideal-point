#!/usr/bin/env Rscript
set.seed(2026)

K <- 2L
domain <- "intellectual_property"

# Source baseline components first (requirement), then attempt Rcpp-merged overrides.
source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
ok_rcpp <- TRUE
tryCatch({
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
}, error = function(e) {
  ok_rcpp <<- FALSE
  message(sprintf("Rcpp failed: %s", e$message))
  source("scripts/R/dynIRT_KD.R")
})
if (!exists("compute_loglik", mode = "function")) {
  source("scripts/R/dynIRT_KD.R")
}

# ---- Load data and Part A results ----
flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

res_A <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
beta_A <- res_A$beta  # J x K
alpha_A <- res_A$alpha

# ---- Select anchor items from Part A betas ----
ratio1 <- abs(beta_A[, 1]) / (abs(beta_A[, 2]) + 0.01)
ratio2 <- abs(beta_A[, 2]) / (abs(beta_A[, 1]) + 0.01)
product <- abs(beta_A[, 1]) * abs(beta_A[, 2])

anchor_A <- which.max(ratio1)  # dim1 specialist
anchor_B <- which.max(ratio2)  # dim2 specialist
product2 <- product
product2[c(anchor_A, anchor_B)] <- -Inf
anchor_C <- which.max(product2) # both-loader (excluding A and B)

anchor_items <- c(anchor_A, anchor_B, anchor_C)
if (length(unique(anchor_items)) < 3L || any(!is.finite(product2[anchor_C]))) {
  # Fallback: top 3 by total |beta|, unique
  total_beta <- rowSums(abs(beta_A))
  anchor_items <- unique(order(total_beta, decreasing = TRUE))[1:3]
}
fixed_betas <- beta_A[anchor_items, , drop = FALSE]
fixed_alpha <- alpha_A[anchor_items]

cat(sprintf("Anchor items (indices): %s\n", paste(anchor_items, collapse = ", ")))
if (!is.null(flow$item_labels)) {
  cat(sprintf("Anchor items (labels): %s\n", paste(flow$item_labels[anchor_items], collapse = " | ")))
}
cat("Fixed betas (from Part A):\n")
print(fixed_betas)

# ---- Initialization (PCA for x; warm-start alpha/beta from Part A) ----
rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA
rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) {
  m <- mean(rc_num[, j], na.rm = TRUE)
  if (is.nan(m)) m <- 0.5
  rc_num[is.na(rc_num[, j]), j] <- m
}
col_var <- apply(rc_num, 2, var)
rc_pca <- if (any(col_var == 0)) rc_num[, col_var > 0, drop = FALSE] else rc_num
pca <- prcomp(rc_pca, center = TRUE, scale. = FALSE)
x_pca <- pca$x[, 1:K, drop = FALSE]
for (k in 1:K) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
x_smooth <- array(NA_real_, dim = c(N, K, T_periods))
for (t in 1:T_periods) x_smooth[, , t] <- x_pca

alpha <- as.numeric(alpha_A)
beta <- beta_A

# Diffuse priors for ALL countries (no country anchors)
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
x_Sigma0_list <- vector("list", N)
for (i in seq_len(N)) x_Sigma0_list[[i]] <- diag(x_Sigma0[i, ], nrow = K)

Omega <- 0.1 * diag(K)

# Data setup
rc <- flow$rc
startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)
bill_session <- as.integer(flow$bill.session)

# Precompute voters per item (static; helps Rcpp m_step_items)
voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

# ---- Logging/output ----
dir.create("outputs/v7_item_anchors", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v7_%s_item_%s.log", domain, ts)
sink(log_file, split = TRUE)
cat(sprintf("V7 Item-Anchor: %s | N=%d, J=%d, T=%d, K=%d\n", domain, N, J, T_periods, K))
cat(sprintf("Using Rcpp: %s\n", if (ok_rcpp) "yes" else "no"))
cat(sprintf("Anchor items: %d, %d, %d\n", anchor_items[1], anchor_items[2], anchor_items[3]))
if (!is.null(flow$item_labels)) {
  cat(sprintf("Anchor item labels: %s\n", paste(flow$item_labels[anchor_items], collapse = " | ")))
}
cat("\nFixed anchor alphas:\n")
print(fixed_alpha)
cat("\nFixed anchor betas:\n")
print(fixed_betas)
cat("\n")

# ---- Custom EM loop ----
maxit <- 5000L
checkfreq <- 50L
thresh_loglik <- 0.01
loglik_patience <- 5L
ncores <- 4L

loglik_trace <- numeric(0)
prev_ll <- -Inf
patience <- 0L
converged <- FALSE
final_iter <- maxit

t0 <- proc.time()

for (iter in 1:maxit) {
  # ==== E-STEP ====
  da <- da_step(rc, alpha, beta, x_smooth, bill_session)

  # Kalman smoother per country
  P_smooth_all <- vector("list", N)
  x_prev <- x_smooth

  if (!exists(".kalman_one_country", mode = "function")) {
    stop(".kalman_one_country not found. Ensure scripts/R/dynIRT_KD.R is sourced.")
  }

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

  # Enforce item anchors: reset fixed items after M-step
  alpha[anchor_items] <- fixed_alpha
  beta[anchor_items, ] <- fixed_betas

  # ==== Convergence via observed-data loglik (per prompt) ====
  if (iter == 1L || iter %% checkfreq == 0L || iter == maxit) {
    ll <- compute_loglik(rc, alpha, beta, x_smooth, bill_session)
    loglik_trace <- c(loglik_trace, ll)
    cat(sprintf("Iter %4d | LL = %.6f\n", iter, ll))

    if (is.finite(prev_ll)) {
      rel_change <- abs(ll - prev_ll) / (1 + abs(prev_ll))
      if (rel_change < thresh_loglik) {
        patience <- patience + 1L
      } else {
        patience <- 0L
      }
      cat(sprintf("  rel_change = %.6f | patience = %d/%d\n", rel_change, patience, loglik_patience))
      if (patience >= loglik_patience) {
        cat(sprintf("Converged at iter %d (LL rel_change < %.2f for %d checks)\n",
                    iter, thresh_loglik, loglik_patience))
        converged <- TRUE
        final_iter <- iter
        break
      }
    }
    prev_ll <- ll
  }
}

elapsed_B <- (proc.time() - t0)["elapsed"]
cat(sprintf("\nPart B done: %.1fs | Iters: %d | Conv: %d\n",
            elapsed_B, final_iter, as.integer(converged)))

# ---- Mean ideal points + aggregate trends ----
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
  anchor_item_labels = if (!is.null(flow$item_labels)) flow$item_labels[anchor_items] else NULL,
  fixed_betas = fixed_betas,
  fixed_alpha = fixed_alpha,
  aggregate = agg,
  runtime = list(
    seconds = elapsed_B,
    iters = final_iter,
    conv = as.integer(converged),
    checkfreq = checkfreq,
    thresh_loglik = thresh_loglik,
    loglik_patience = loglik_patience,
    loglik_trace = loglik_trace
  )
)
saveRDS(out_B, sprintf("outputs/v7_item_anchors/%s_results.rds", domain))
cat(sprintf("Saved: outputs/v7_item_anchors/%s_results.rds\n", domain))
sink()

