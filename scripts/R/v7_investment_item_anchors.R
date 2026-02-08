#!/usr/bin/env Rscript
set.seed(2026)

K <- 2L
domain <- "investment"
flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

# Load Part A results to select anchor items and warm-start alpha/beta.
res_A <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
beta_A <- as.matrix(res_A$beta)   # J x K
alpha_A <- as.numeric(res_A$alpha)

# Select 3 anchor items from estimated betas:
# A: highest |beta_1| / (|beta_2| + 0.01) -> dim1 specialist
# B: highest |beta_2| / (|beta_1| + 0.01) -> dim2 specialist
# C: highest |beta_1| * |beta_2| (excluding A and B) -> both-loader
ratio1 <- abs(beta_A[, 1]) / (abs(beta_A[, 2]) + 0.01)
ratio2 <- abs(beta_A[, 2]) / (abs(beta_A[, 1]) + 0.01)
product <- abs(beta_A[, 1]) * abs(beta_A[, 2])

pick_argmax_excluding <- function(x, exclude = integer()) {
  x2 <- x
  if (length(exclude) > 0) x2[exclude] <- -Inf
  which.max(x2)
}

item_A <- pick_argmax_excluding(ratio1)
item_B <- pick_argmax_excluding(ratio2, exclude = item_A)
item_C <- pick_argmax_excluding(product, exclude = c(item_A, item_B))
anchor_items <- c(item_A, item_B, item_C)

fixed_betas <- beta_A[anchor_items, , drop = FALSE]
fixed_alpha <- alpha_A[anchor_items]

# Source components (order chosen to keep Rcpp overrides if available).
source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
if (!exists("compute_loglik", mode = "function")) {
  source("scripts/R/dynIRT_KD.R")
}

ok_rcpp <- TRUE
tryCatch(
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R"),
  error = function(e) {
    message("Rcpp compilation/load failed; continuing with pure R implementation.")
    ok_rcpp <<- FALSE
  }
)

# Diffuse priors for ALL countries (no country anchors)
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
x_Sigma0_list <- vector("list", N)
for (i in seq_len(N)) x_Sigma0_list[[i]] <- diag(x_Sigma0[i, ], nrow = K)

# Warm-start alpha/beta from Part A; x from Part A if present else PCA.
alpha <- alpha_A
beta <- beta_A
x_smooth <- res_A$ideal_points
if (!is.array(x_smooth) || any(dim(x_smooth) != c(N, K, T_periods))) {
  # PCA fallback
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
}
Omega <- 0.1 * diag(K)

# Data setup
rc <- flow$rc
startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)
bill_session <- as.integer(flow$bill.session)

voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

dir.create("outputs/v7_item_anchors", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v7_%s_item_%s.log", domain, ts)
sink(log_file, split = TRUE)
cat(sprintf("V7 Item-Anchor: %s | N=%d, J=%d, T=%d, K=%d\n", domain, N, J, T_periods, K))
cat(sprintf("Impl: %s\n", if (ok_rcpp) "rcpp_merged" else "pure_R"))
cat(sprintf("Anchor items (A,B,C): %d, %d, %d\n", anchor_items[1], anchor_items[2], anchor_items[3]))
cat(sprintf("Anchor item labels: %s\n", paste(flow$item_labels[anchor_items], collapse = " | ")))
cat("Fixed (alpha, beta) for anchors:\n")
print(data.frame(
  item = anchor_items,
  label = flow$item_labels[anchor_items],
  alpha = fixed_alpha,
  beta1 = fixed_betas[, 1],
  beta2 = fixed_betas[, 2]
))
cat("\n")

maxit <- 5000L
checkfreq <- 50L
thresh_loglik <- 0.01
patience_need <- 5L
ncores <- 4L

loglik_trace <- numeric(maxit)
prev_ll <- -Inf
patience <- 0L
converged <- FALSE
final_iter <- maxit

t0 <- proc.time()

for (iter in seq_len(maxit)) {
  alpha_old <- alpha
  beta_old <- beta
  x_old <- x_smooth

  # ==== E-STEP ====
  da <- da_step(rc, alpha, beta, x_smooth, bill_session)

  P_smooth_all <- vector("list", N)
  x_prev <- x_smooth

  if (exists(".kalman_one_country", mode = "function")) {
    ks_results <- parallel::mclapply(
      seq_len(N),
      function(i) {
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
      },
      mc.cores = ncores
    )

    for (i in seq_len(N)) {
      res_i <- ks_results[[i]]
      x_smooth[i, , ] <- res_i$x_smooth_i
      P_smooth_all[[i]] <- res_i$P_smooth_i
    }
  } else {
    # Manual Kalman loop (slower; no parallel helper available).
    for (i in seq_len(N)) {
      obs_items <- which(rc[i, ] != 0L)
      si <- startlegis[i]
      ei <- endlegis[i]

      x_smooth_i <- matrix(x_prev[i, , ], nrow = K, ncol = T_periods)
      P_smooth_i <- vector("list", T_periods)

      if (length(obs_items) == 0) {
        P_cov <- x_Sigma0_list[[i]]
        for (tt in seq_len(T_periods)) {
          if (tt == 1L) {
            x_smooth_i[, tt] <- x_mu0[i, ]
            P_smooth_i[[tt]] <- P_cov
          } else {
            x_smooth_i[, tt] <- x_smooth_i[, tt - 1L]
            P_cov <- P_cov + Omega
            P_smooth_i[[tt]] <- P_cov
          }
        }
      } else {
        y_star_i <- da$y_star[i, obs_items]
        ks <- kalman_smoother_country(
          y_star_i = y_star_i,
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
        for (tt in seq_len(T_periods)) {
          if (is.null(P_smooth_i[[tt]])) P_smooth_i[[tt]] <- diag(K) * 100
          if (any(is.na(x_smooth_i[, tt]))) {
            x_smooth_i[, tt] <- if (tt > 1L) x_smooth_i[, tt - 1L] else x_mu0[i, ]
          }
        }
      }

      x_smooth[i, , ] <- x_smooth_i
      P_smooth_all[[i]] <- P_smooth_i
    }
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

  # Enforce item anchors: overwrite fixed items after M-step
  alpha[anchor_items] <- fixed_alpha
  beta[anchor_items, ] <- fixed_betas

  # ==== Convergence check ====
  delta <- param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

  if (iter == 1L || iter %% checkfreq == 0L || iter == maxit || delta < 1e-4) {
    ll <- compute_loglik(rc, alpha, beta, x_smooth, bill_session)
    loglik_trace[iter] <- ll
    cat(sprintf("Iter %4d | LL = %.4f | delta = %.2e\n", iter, ll, delta))

    if (iter > 1L && abs(ll - prev_ll) / (1 + abs(prev_ll)) < thresh_loglik) {
      patience <- patience + 1L
      if (patience >= patience_need) {
        cat(sprintf("Converged at iter %d (LL rel_change < %.3f for %d checks)\n",
                    iter, thresh_loglik, patience_need))
        converged <- TRUE
        final_iter <- iter
        break
      }
    } else {
      patience <- 0L
    }
    prev_ll <- ll
  } else {
    loglik_trace[iter] <- prev_ll
  }

  if (delta < 1e-4) {
    cat(sprintf("Converged at iter %d (delta = %.2e)\n", iter, delta))
    converged <- TRUE
    final_iter <- iter
    break
  }
}

elapsed_B <- (proc.time() - t0)["elapsed"]
loglik_trace <- loglik_trace[1:final_iter]
cat(sprintf("\nPart B done: %.1fs | Iters: %d | Conv: %d\n",
            elapsed_B, final_iter, as.integer(converged)))

# Compute mean ideal points across active periods
sl <- startlegis
el <- endlegis
x_mean <- matrix(NA_real_, N, K)
for (i in seq_len(N)) {
  s <- sl[i] + 1L
  e <- el[i] + 1L
  if (e >= s && s >= 1L && e <= T_periods) {
    if (s == e) {
      x_mean[i, ] <- x_smooth[i, , s]
    } else {
      x_mean[i, ] <- rowMeans(x_smooth[i, , s:e, drop = FALSE])
    }
  }
}
rownames(x_mean) <- flow$country_codes

# Aggregate statistics per period
agg <- data.frame(
  period = 1:T_periods,
  mean_dim1 = sapply(1:T_periods, function(t) mean(x_smooth[, , t][, 1], na.rm = TRUE)),
  sd_dim1   = sapply(1:T_periods, function(t) sd(x_smooth[, , t][, 1], na.rm = TRUE)),
  mean_dim2 = sapply(1:T_periods, function(t) mean(x_smooth[, , t][, 2], na.rm = TRUE)),
  sd_dim2   = sapply(1:T_periods, function(t) sd(x_smooth[, , t][, 2], na.rm = TRUE))
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

