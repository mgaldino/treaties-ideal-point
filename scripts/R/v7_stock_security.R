#!/usr/bin/env Rscript
set.seed(2026)

# Prefer Rcpp-merged version; fall back to pure R if compilation fails.
ok <- try(source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R"), silent = TRUE)
if (inherits(ok, "try-error")) {
  message("WARNING: Rcpp compilation failed; falling back to scripts/R/dynIRT_KD.R")
  source("scripts/R/dynIRT_KD.R")
}

K <- 2L
domain <- "security"
flow <- readRDS(file.path("data/processed", paste0(domain, "_stock_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

# V7 substantive anchors
anchor_iso <- c("DNK", "IRN", "UKR")
anchor_idx <- match(anchor_iso, flow$country_codes)
if (any(is.na(anchor_idx))) {
  missing <- anchor_iso[is.na(anchor_idx)]
  stop(sprintf("Anchor countries not found in flow matrix: %s", paste(missing, collapse = ", ")))
}
anchor_positions <- rbind(
  c(+2,  0),  # DNK: dim1 positive (pro-ILO)
  c(-2,  0),  # IRN: dim1 negative (anti-ILO)
  c( 0, -2)   # UKR: dim2 anchor (confounder axis)
)

x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

# PCA initialization
rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA
rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) {
  m <- mean(rc_num[, j], na.rm = TRUE)
  if (is.nan(m)) m <- 0.5
  rc_num[is.na(rc_num[, j]), j] <- m
}

# Drop zero-variance columns before PCA (prevents prcomp errors)
col_var <- apply(rc_num, 2, var)
rc_pca <- if (any(col_var == 0)) rc_num[, col_var > 0, drop = FALSE] else rc_num
if (ncol(rc_pca) < K) {
  warning(sprintf("PCA init: only %d non-constant columns available; adding jitter.", ncol(rc_pca)))
  rc_pca <- rc_num + matrix(rnorm(N * J, 0, 1e-6), nrow = N, ncol = J)
}

pca <- prcomp(rc_pca, center = TRUE, scale. = FALSE)
x_pca <- pca$x[, 1:K, drop = FALSE]
for (k in 1:K) x_pca[, k] <- as.numeric(scale(x_pca[, k]))

x_start <- array(NA_real_, dim = c(N, K, T_periods))
for (t in 1:T_periods) x_start[, , t] <- x_pca

dir.create("outputs/v7_stock_country_anchors", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v7_stock_%s_%s.log", domain, ts)
sink(log_file, split = TRUE)

cat(sprintf("V7 Stock Country-Anchor: %s | N=%d, J=%d, T=%d, K=%d\n", domain, N, J, T_periods, K))
cat(sprintf(
  "Anchors: %s (%s)\n",
  paste(anchor_iso, collapse = ", "),
  paste(apply(anchor_positions, 1, function(r) sprintf("(%+.0f,%+.0f)", r[1], r[2])), collapse = ", ")
))
cat("Rationale: dim1=ILO support, dim2=confounder separation\n\n")

t0 <- proc.time()
res <- dynIRT_KD(
  .data = list(
    rc = flow$rc,
    startlegis = matrix(as.integer(flow$startlegis), ncol = 1),
    endlegis = matrix(as.integer(flow$endlegis), ncol = 1),
    bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
    T = T_periods
  ),
  .starts = list(
    alpha = numeric(J),
    beta = matrix(rnorm(J * K, 0, 0.1), J, K),
    x = x_start
  ),
  .priors = list(
    x.mu0 = x_mu0,
    x.Sigma0 = x_Sigma0,
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  ),
  .control = list(
    verbose = TRUE,
    thresh = 1e-4,
    maxit = 5000L,
    checkfreq = 50L,
    estimate_omega = FALSE,
    thresh_loglik = 0.01,
    loglik_patience = 5L,
    ncores = 4L
  ),
  K = K
)
elapsed_A <- (proc.time() - t0)["elapsed"]
cat(sprintf("\nPart A done: %.1fs | Iters: %d | Conv: %d\n", elapsed_A, res$runtime$iters, res$runtime$conv))

# Compute mean ideal points across active periods
sl <- as.integer(flow$startlegis)
el <- as.integer(flow$endlegis)
x_mean <- matrix(NA_real_, N, K)
for (i in 1:N) {
  s <- sl[i] + 1L
  e <- el[i] + 1L
  if (e >= s && s >= 1L && e <= T_periods) {
    x_mean[i, ] <- if (s == e) res$means$x[i, , s] else rowMeans(res$means$x[i, , s:e, drop = FALSE])
  }
}
rownames(x_mean) <- flow$country_codes

# Aggregate statistics per period (for paper)
agg <- data.frame(
  period = 1:T_periods,
  mean_dim1 = sapply(1:T_periods, function(t) mean(res$means$x[, , t][, 1], na.rm = TRUE)),
  sd_dim1 = sapply(1:T_periods, function(t) sd(res$means$x[, , t][, 1], na.rm = TRUE)),
  mean_dim2 = sapply(1:T_periods, function(t) mean(res$means$x[, , t][, 2], na.rm = TRUE)),
  sd_dim2 = sapply(1:T_periods, function(t) sd(res$means$x[, , t][, 2], na.rm = TRUE))
)
if (!is.null(flow$period_labels)) agg$period_label <- flow$period_labels
cat("\nAggregate trends:\n")
print(agg)

out_A <- list(
  domain = domain,
  strategy = "stock_country_anchors_v7",
  ideal_points = res$means$x,
  ideal_points_mean = x_mean,
  alpha = res$means$alpha,
  beta = res$means$beta,
  country_codes = flow$country_codes,
  item_labels = flow$item_labels,
  period_labels = flow$period_labels,
  anchors = list(
    iso = anchor_iso,
    positions = anchor_positions,
    rationale = "dim1=ILO, dim2=confounder"
  ),
  aggregate = agg,
  runtime = list(
    seconds = as.numeric(elapsed_A),
    iters = res$runtime$iters,
    conv = res$runtime$conv,
    loglik_trace = res$runtime$loglik_trace
  )
)

out_path <- file.path("outputs/v7_stock_country_anchors", sprintf("%s_results.rds", domain))
saveRDS(out_A, out_path)
cat(sprintf("Saved: %s\n", out_path))
sink()
