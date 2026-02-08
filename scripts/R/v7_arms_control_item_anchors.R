#!/usr/bin/env Rscript
set.seed(2026)

domain <- "arms_control"
K <- 2L

dir.create("logs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/v7_item_anchors", recursive = TRUE, showWarnings = FALSE)

ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v7_%s_item_%s.log", domain, ts)
sink(log_file, split = TRUE)

cat(sprintf("V7 Item-Anchor: %s | K=%d | %s\n\n", domain, K, ts))

# Load Part A results + flow matrix
partA_path <- sprintf("outputs/v7_country_anchors/%s_results.rds", domain)
if (!file.exists(partA_path)) stop(sprintf("Part A results not found: %s", partA_path))
partA <- readRDS(partA_path)

flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
rc <- flow$rc
N <- nrow(rc); J <- ncol(rc); T_total <- flow$T

if (is.null(partA$beta) || is.null(partA$alpha)) stop("Part A results missing alpha/beta.")
beta_A <- as.matrix(partA$beta)
alpha_A <- as.numeric(partA$alpha)
if (nrow(beta_A) != J || ncol(beta_A) != K) stop("Part A beta dimensions do not match flow matrix.")
if (length(alpha_A) != J) stop("Part A alpha length does not match flow matrix.")

# ---- Source required components (keep Rcpp fast path if available) ----
rcpp_ok <- TRUE
tryCatch(
  {
    source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
  },
  error = function(e) {
    rcpp_ok <<- FALSE
    cat("WARNING: Rcpp wrapper failed; falling back to pure R dynIRT_KD.\n")
    cat(sprintf("Rcpp error: %s\n\n", conditionMessage(e)))
  }
)
source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
if (!exists("compute_loglik", mode = "function") || !exists(".kalman_one_country", mode = "function")) {
  source("scripts/R/dynIRT_KD.R")
}

if (!exists("da_step", mode = "function")) stop("da_step() not available after sourcing.")
if (!exists("kalman_smoother_country", mode = "function")) stop("kalman_smoother_country() not available.")
if (!exists("m_step_items", mode = "function")) stop("m_step_items() not available.")
if (!exists("compute_loglik", mode = "function")) stop("compute_loglik() not available.")

cat(sprintf("Implementation: %s\n\n", if (rcpp_ok && exists("da_step_rcpp", mode = "function")) "Rcpp (merged)" else "Pure R"))

# ---- Select 3 anchor items from Part A betas ----
score_dim1 <- abs(beta_A[, 1]) / (abs(beta_A[, 2]) + 0.01)
score_dim2 <- abs(beta_A[, 2]) / (abs(beta_A[, 1]) + 0.01)
score_both <- abs(beta_A[, 1]) * abs(beta_A[, 2])

idx_A <- which.max(score_dim1)
idx_B <- which.max(score_dim2)
score_both[c(idx_A, idx_B)] <- -Inf
idx_C <- which.max(score_both)

anchor_items <- c(idx_A, idx_B, idx_C)
anchor_names <- if (!is.null(flow$item_labels)) flow$item_labels[anchor_items] else as.character(anchor_items)

cat("Selected anchor items (from Part A betas):\n")
for (ii in seq_along(anchor_items)) {
  j <- anchor_items[ii]
  nm <- anchor_names[ii]
  cat(sprintf(
    "  %s: j=%d | label=%s | alpha=%.4f | beta=(%.4f, %.4f)\n",
    c("A(dim1 specialist)", "B(dim2 specialist)", "C(both-loader)")[ii],
    j, nm, alpha_A[j], beta_A[j, 1], beta_A[j, 2]
  ))
}
cat("\n")

anchor_fixed <- list(
  j = anchor_items,
  labels = anchor_names,
  alpha = alpha_A[anchor_items],
  beta = beta_A[anchor_items, , drop = FALSE],
  selection = list(
    A = list(j = idx_A, metric = "max |b1|/(|b2|+0.01)", value = score_dim1[idx_A]),
    B = list(j = idx_B, metric = "max |b2|/(|b1|+0.01)", value = score_dim2[idx_B]),
    C = list(j = idx_C, metric = "max |b1|*|b2| (excluding A,B)", value = score_both[idx_C])
  )
)

# ---- Initialize parameters (warm-start alpha/beta from Part A; diffuse x priors) ----
alpha <- alpha_A
beta <- beta_A

x_smooth <- partA$ideal_points
if (!is.array(x_smooth) || length(dim(x_smooth)) != 3L ||
    dim(x_smooth)[1] != N || dim(x_smooth)[2] != K || dim(x_smooth)[3] != T_total) {
  cat("WARNING: Part A ideal_points unavailable or mismatched; initializing x via PCA on rc.\n")
  rc_num <- matrix(as.numeric(rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }
  col_var <- apply(rc_num, 2, var)
  rc_pca <- if (any(col_var == 0)) rc_num[, col_var > 0] else rc_num
  pca <- prcomp(rc_pca, center = TRUE, scale. = FALSE)
  x_pca <- pca$x[, 1:K]
  for (k in 1:K) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_smooth <- array(NA_real_, dim = c(N, K, T_total))
  for (t in 1:T_total) x_smooth[, , t] <- x_pca
}

# Priors (diffuse country priors, no country anchors)
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K) # diagonal variances; dyn code converts to list
beta_mu <- rep(0, K + 1L)
beta_sigma <- 25 * diag(K + 1L)
Omega <- 0.1 * diag(K) # fixed (estimate_omega=FALSE)

startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)
bill.session <- as.integer(flow$bill.session)

# voters_by_item for m_step speed
voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

# Convert Sigma0 to list-of-matrices (matches dynIRT_KD internals)
Sigma0_list <- vector("list", N)
for (i in seq_len(N)) Sigma0_list[[i]] <- diag(x_Sigma0[i, ], nrow = K)

maxit <- 5000L
checkfreq <- 50L
thresh_loglik <- 0.01
patience <- 5L
ncores <- 4L
if (.Platform$OS.type == "windows") ncores <- 1L

cat(sprintf("EM control: maxit=%d, checkfreq=%d, thresh_loglik=%.3g, patience=%d, ncores=%d\n\n",
            maxit, checkfreq, thresh_loglik, patience, ncores))

loglik_trace <- numeric(maxit)
conv <- 0L
loglik_streak <- 0L

t0 <- proc.time()

for (m in seq_len(maxit)) {

  # ==== E-STEP ====
  da <- da_step(rc, alpha, beta, x_smooth, bill.session)

  # Kalman smoother per country
  P_smooth_all <- vector("list", N)
  P_lag_all <- vector("list", N)
  x_prev <- x_smooth

  if (ncores > 1L && exists(".kalman_one_country", mode = "function")) {
    ks_results <- parallel::mclapply(seq_len(N), function(i) {
      .kalman_one_country(
        i = i,
        da_y_star = da$y_star,
        rc = rc,
        alpha = alpha,
        beta = beta,
        bill.session = bill.session,
        x_mu0 = x_mu0,
        x_Sigma0 = Sigma0_list,
        Omega = Omega,
        startlegis = startlegis,
        endlegis = endlegis,
        T_total = T_total,
        K = K,
        x_prev_i = matrix(x_prev[i, , ], nrow = K, ncol = T_total)
      )
    }, mc.cores = ncores)
  } else if (exists(".kalman_one_country", mode = "function")) {
    ks_results <- lapply(seq_len(N), function(i) {
      .kalman_one_country(
        i = i,
        da_y_star = da$y_star,
        rc = rc,
        alpha = alpha,
        beta = beta,
        bill.session = bill.session,
        x_mu0 = x_mu0,
        x_Sigma0 = Sigma0_list,
        Omega = Omega,
        startlegis = startlegis,
        endlegis = endlegis,
        T_total = T_total,
        K = K,
        x_prev_i = matrix(x_prev[i, , ], nrow = K, ncol = T_total)
      )
    })
  } else {
    # Manual loop if helper is unavailable
    ks_results <- vector("list", N)
    for (i in seq_len(N)) {
      si <- startlegis[i]
      ei <- endlegis[i]
      if (is.na(si) || is.na(ei) || ei < si) {
        x_smooth[i, , ] <- 0
        P_smooth_all[[i]] <- replicate(T_total, diag(K), simplify = FALSE)
        P_lag_all[[i]] <- replicate(max(0L, T_total - 1L), diag(K), simplify = FALSE)
        ks_results[[i]] <- list(
          x_smooth_i = matrix(0, nrow = K, ncol = T_total),
          P_smooth_i = P_smooth_all[[i]],
          P_lag_i = P_lag_all[[i]]
        )
        next
      }
      item_idx <- which(rc[i, ] != 0L)
      y_star_i <- da$y_star[i, item_idx]
      ks <- kalman_smoother_country(
        y_star_i = as.numeric(y_star_i),
        item_indices = as.integer(item_idx),
        alpha = alpha,
        beta = beta,
        bill.session = bill.session,
        mu0 = x_mu0[i, ],
        Sigma0 = Sigma0_list[[i]],
        Omega = Omega,
        start_t = si,
        end_t = ei,
        T_total = T_total
      )

      x_smooth_i <- matrix(0, nrow = K, ncol = T_total)
      P_smooth_i <- vector("list", T_total)
      for (t in seq_len(T_total)) P_smooth_i[[t]] <- diag(K)
      P_lag_i <- vector("list", max(0L, T_total - 1L))
      for (t in seq_len(max(0L, T_total - 1L))) P_lag_i[[t]] <- diag(K)

      active_idx <- (si + 1L):(ei + 1L)
      x_smooth_i[, active_idx] <- ks$x_smooth
      P_smooth_i[active_idx] <- ks$P_smooth
      if (length(ks$P_lag) > 0) {
        P_lag_i[(si + 1L):(ei)] <- ks$P_lag
      }

      ks_results[[i]] <- list(x_smooth_i = x_smooth_i, P_smooth_i = P_smooth_i, P_lag_i = P_lag_i)
    }
  }

  for (i in seq_len(N)) {
    res_i <- ks_results[[i]]
    x_smooth[i, , ] <- res_i$x_smooth_i
    P_smooth_all[[i]] <- res_i$P_smooth_i
    P_lag_all[[i]] <- res_i$P_lag_i
  }

  # ==== M-STEP ====
  m_items <- m_step_items(
    y_star = da$y_star,
    rc = rc,
    x_smooth = x_smooth,
    P_smooth = P_smooth_all,
    bill.session = bill.session,
    beta_mu = beta_mu,
    beta_sigma = beta_sigma,
    voters_by_item = voters_by_item
  )
  alpha <- m_items$alpha
  beta <- m_items$beta

  # Enforce fixed anchor item parameters (identification)
  alpha[anchor_fixed$j] <- anchor_fixed$alpha
  beta[anchor_fixed$j, ] <- anchor_fixed$beta

  # ==== Convergence: loglik early stopping ====
  need_ll <- (m == 1L) || (m %% checkfreq == 0L)
  if (need_ll) {
    ll <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)
    loglik_trace[m] <- ll
    cat(sprintf("Iter %4d | loglik = %.4f\n", m, ll))
  } else {
    loglik_trace[m] <- loglik_trace[m - 1L]
  }

  if (m >= 2L) {
    ll_m <- loglik_trace[m]
    ll_m1 <- loglik_trace[m - 1L]
    rel_change <- abs(ll_m - ll_m1) / (1 + abs(ll_m1))
    if (is.finite(rel_change) && rel_change < thresh_loglik) {
      loglik_streak <- loglik_streak + 1L
    } else {
      loglik_streak <- 0L
    }
    if (loglik_streak >= patience) {
      conv <- 1L
      loglik_trace <- loglik_trace[seq_len(m)]
      cat(sprintf("Loglik converged at iteration %d (rel_change=%.2e, patience=%d)\n", m, rel_change, patience))
      break
    }
  }
}

elapsed_B <- (proc.time() - t0)["elapsed"]
iters_B <- length(loglik_trace)
cat(sprintf("\nPart B done: %.1fs | Iters: %d | Conv: %d\n", elapsed_B, iters_B, conv))

# Compute mean ideal points across active periods
sl <- as.integer(flow$startlegis)
el <- as.integer(flow$endlegis)
x_mean <- matrix(NA_real_, N, K)
for (i in 1:N) {
  s <- sl[i] + 1L
  e <- el[i] + 1L
  if (e >= s && s >= 1 && e <= T_total) {
    x_mean[i, ] <- if (s == e) x_smooth[i, , s] else rowMeans(x_smooth[i, , s:e])
  }
}
rownames(x_mean) <- flow$country_codes

agg <- data.frame(
  period = 1:T_total,
  mean_dim1 = sapply(1:T_total, function(t) mean(x_smooth[, , t][, 1], na.rm = TRUE)),
  sd_dim1 = sapply(1:T_total, function(t) sd(x_smooth[, , t][, 1], na.rm = TRUE)),
  mean_dim2 = sapply(1:T_total, function(t) mean(x_smooth[, , t][, 2], na.rm = TRUE)),
  sd_dim2 = sapply(1:T_total, function(t) sd(x_smooth[, , t][, 2], na.rm = TRUE))
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
  anchors = list(
    type = "items",
    fixed_from = "Part A (country anchors)",
    items = anchor_fixed
  ),
  aggregate = agg,
  runtime = list(
    seconds = as.numeric(elapsed_B),
    iters = iters_B,
    conv = conv,
    loglik_trace = loglik_trace
  )
)

saveRDS(out_B, sprintf("outputs/v7_item_anchors/%s_results.rds", domain))
cat(sprintf("\nSaved: outputs/v7_item_anchors/%s_results.rds\n", domain))
sink()

