# Calibration diagnostics + Brier decomposition for V2 evaluation

suppressPackageStartupMessages({
  library(emIRT)
})

source("scripts/R/v2_eval/run_models.R")
source("scripts/R/v2_eval/score_metrics.R")

DATA_PATH <- "data/processed/environment_flow_matrix.rds"
SPLITS_PATH <- "outputs/v2_eval/splits.rds"

OUT_DIR <- "outputs/v2_eval"
OUT_BINS <- file.path(OUT_DIR, "calibration_bins.csv")
OUT_HIST <- file.path(OUT_DIR, "p_hist.csv")
OUT_DECOMP <- file.path(OUT_DIR, "brier_decomp.csv")
OUT_SUMMARY <- file.path(OUT_DIR, "diagnostics_summary.csv")

# Control parameters (match run_all)
MAXIT <- 1500L
THRESH <- 1e-6
CHECKFREQ <- 50L
NCORES <- 1L

flow <- readRDS(DATA_PATH)

# Priors (anchors DNK/SAU)
N <- nrow(flow$rc)
K <- 1L
x_mu0 <- matrix(0, nrow = N, ncol = 1)
x_sigma0 <- matrix(1, nrow = N, ncol = 1)

pos_idx <- which(flow$country_codes == "DNK")
neg_idx <- which(flow$country_codes == "SAU")
stopifnot(length(pos_idx) == 1, length(neg_idx) == 1)

x_mu0[pos_idx, 1]    <-  2.0
x_sigma0[pos_idx, 1] <-  0.01
x_mu0[neg_idx, 1]    <- -2.0
x_sigma0[neg_idx, 1] <-  0.01

priors <- list(
  x_mu0 = x_mu0,
  x_sigma0 = x_sigma0,
  beta_mu = matrix(c(0, 0), nrow = 2, ncol = 1),
  beta_sigma = matrix(c(25, 0, 0, 25), nrow = 2, ncol = 2),
  omega2 = matrix(0.1, nrow = N, ncol = 1),
  omega2_val = 0.1
)

control_emirt <- list(
  threads = 1L,
  verbose = FALSE,
  thresh = THRESH,
  maxit = MAXIT,
  checkfreq = CHECKFREQ
)

control_kd <- list(
  threads = 1L,
  verbose = FALSE,
  thresh = THRESH,
  maxit = MAXIT,
  checkfreq = CHECKFREQ,
  thresh_loglik = 1e-6,
  loglik_patience = 3L,
  thresh_aitken = 1e-4,
  estimate_omega = FALSE,
  ncores = NCORES
)

if (!file.exists(SPLITS_PATH)) {
  source("scripts/R/v2_eval/prepare_splits.R")
}

splits <- readRDS(SPLITS_PATH)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers ----

make_bins <- function(p, y, n_bins = 10L) {
  p <- pmin(pmax(p, 0), 1)
  bin_id <- floor(p * n_bins) + 1L
  bin_id[bin_id > n_bins] <- n_bins
  bin_low <- (bin_id - 1L) / n_bins
  bin_high <- bin_id / n_bins

  bins <- data.frame(bin = bin_id, p = p, y = y, bin_low = bin_low, bin_high = bin_high)
  agg <- aggregate(cbind(p, y) ~ bin + bin_low + bin_high, data = bins, FUN = mean)
  counts <- aggregate(y ~ bin, data = bins, FUN = length)
  colnames(counts)[2] <- "n"

  out <- merge(agg, counts, by = "bin", all = TRUE)
  out <- out[order(out$bin), ]
  out$p_mid <- (out$bin_low + out$bin_high) / 2
  out
}

brier_decomp <- function(p, y, n_bins = 10L) {
  bins <- make_bins(p, y, n_bins = n_bins)
  N <- length(p)
  o_bar <- mean(y)

  # reliability and resolution using bin means
  rel <- sum((bins$n / N) * (bins$p - bins$y)^2)
  res <- sum((bins$n / N) * (bins$y - o_bar)^2)
  unc <- o_bar * (1 - o_bar)

  list(reliability = rel, resolution = res, uncertainty = unc)
}

calc_ece <- function(p, y, n_bins = 10L) {
  bins <- make_bins(p, y, n_bins = n_bins)
  N <- length(p)
  abs_gap <- abs(bins$p - bins$y)
  ece <- sum((bins$n / N) * abs_gap)
  mce <- max(abs_gap)
  list(ece = ece, mce = mce)
}

make_hist <- function(p, n_bins = 20L) {
  p <- pmin(pmax(p, 0), 1)
  bin_id <- floor(p * n_bins) + 1L
  bin_id[bin_id > n_bins] <- n_bins
  bin_low <- (bin_id - 1L) / n_bins
  bin_high <- bin_id / n_bins

  bins <- data.frame(bin = bin_id, bin_low = bin_low, bin_high = bin_high)
  counts <- aggregate(bin ~ bin + bin_low + bin_high, data = bins, FUN = length)
  colnames(counts)[4] <- "n"
  counts <- counts[order(counts$bin), ]
  counts$p_mid <- (counts$bin_low + counts$bin_high) / 2
  counts
}

# ---- Main loop ----

rows_bins <- list()
rows_hist <- list()
rows_decomp <- list()
rows_summary <- list()

process_split <- function(split, split_id) {
  # Flatten test indices
  if (split$type == "holdout_by_period") {
    tests <- split$test
    i_idx <- unlist(lapply(tests, function(z) z$i))
    j_idx <- unlist(lapply(tests, function(z) z$j))
    y_val <- unlist(lapply(tests, function(z) z$y))
  } else {
    i_idx <- split$test$i
    j_idx <- split$test$j
    y_val <- split$test$y
  }

  test <- list(i = i_idx, j = j_idx, y = y_val)

  # Build training rc
  rc_train <- flow$rc
  rc_train[cbind(i_idx, j_idx)] <- 0L

  # Starts (seeded)
  if (!is.null(split$seed)) set.seed(split$seed)
  starts <- build_starts(rc_train, flow$T)

  # Run models
  res_em <- run_emirt(flow, rc_train, starts, priors, control_emirt)
  res_kd <- run_dynirt_kd(flow, rc_train, starts, priors, control_kd, K = K)

  # Predictions + scores
  m_em <- score_metrics(res_em$means$x, res_em$means$alpha, res_em$means$beta,
                        as.integer(flow$bill.session), test, K = K, return_probs = TRUE)
  m_kd <- score_metrics(res_kd$means$x, res_kd$means$alpha, res_kd$means$beta,
                        as.integer(flow$bill.session), test, K = K, return_probs = TRUE)

  list(em = m_em, kd = m_kd)
}

# Holdout splits
for (sp in splits$holdout) {
  split_id <- sprintf("holdout_seed_%d", sp$seed)
  cat(sprintf("\nRunning diagnostics %s...\n", split_id))
  res <- process_split(sp, split_id)

  for (model in c("em", "kd")) {
    m <- res[[model]]
    # Calibration bins
    bins <- make_bins(m$p, m$y, n_bins = 10L)
    bins$split_id <- split_id
    bins$split_type <- sp$type
    bins$model <- model
    rows_bins[[length(rows_bins) + 1L]] <- bins

    # Histogram
    hist <- make_hist(m$p, n_bins = 20L)
    hist$split_id <- split_id
    hist$split_type <- sp$type
    hist$model <- model
    rows_hist[[length(rows_hist) + 1L]] <- hist

    # Brier decomposition
    decomp <- brier_decomp(m$p, m$y, n_bins = 10L)
    ece <- calc_ece(m$p, m$y, n_bins = 10L)

    rows_decomp[[length(rows_decomp) + 1L]] <- data.frame(
      split_id = split_id,
      split_type = sp$type,
      model = model,
      brier = m$brier,
      logscore_mean = m$logscore_mean,
      reliability = decomp$reliability,
      resolution = decomp$resolution,
      uncertainty = decomp$uncertainty,
      ece = ece$ece,
      mce = ece$mce,
      n = m$n,
      stringsAsFactors = FALSE
    )

    rows_summary[[length(rows_summary) + 1L]] <- data.frame(
      split_id = split_id,
      split_type = sp$type,
      model = model,
      n = m$n,
      logscore_mean = m$logscore_mean,
      brier = m$brier,
      ece = ece$ece,
      mce = ece$mce,
      stringsAsFactors = FALSE
    )
  }
}

# Temporal split
split_id <- "temporal_last2"
cat(sprintf("\nRunning diagnostics %s...\n", split_id))
res <- process_split(splits$temporal, split_id)

for (model in c("em", "kd")) {
  m <- res[[model]]
  bins <- make_bins(m$p, m$y, n_bins = 10L)
  bins$split_id <- split_id
  bins$split_type <- splits$temporal$type
  bins$model <- model
  rows_bins[[length(rows_bins) + 1L]] <- bins

  hist <- make_hist(m$p, n_bins = 20L)
  hist$split_id <- split_id
  hist$split_type <- splits$temporal$type
  hist$model <- model
  rows_hist[[length(rows_hist) + 1L]] <- hist

  decomp <- brier_decomp(m$p, m$y, n_bins = 10L)
  ece <- calc_ece(m$p, m$y, n_bins = 10L)

  rows_decomp[[length(rows_decomp) + 1L]] <- data.frame(
    split_id = split_id,
    split_type = splits$temporal$type,
    model = model,
    brier = m$brier,
    logscore_mean = m$logscore_mean,
    reliability = decomp$reliability,
    resolution = decomp$resolution,
    uncertainty = decomp$uncertainty,
    ece = ece$ece,
    mce = ece$mce,
    n = m$n,
    stringsAsFactors = FALSE
  )

  rows_summary[[length(rows_summary) + 1L]] <- data.frame(
    split_id = split_id,
    split_type = splits$temporal$type,
    model = model,
    n = m$n,
    logscore_mean = m$logscore_mean,
    brier = m$brier,
    ece = ece$ece,
    mce = ece$mce,
    stringsAsFactors = FALSE
  )
}

# Write outputs
bins_df <- do.call(rbind, rows_bins)
hist_df <- do.call(rbind, rows_hist)
decomp_df <- do.call(rbind, rows_decomp)
summary_df <- do.call(rbind, rows_summary)

write.csv(bins_df, OUT_BINS, row.names = FALSE)
write.csv(hist_df, OUT_HIST, row.names = FALSE)
write.csv(decomp_df, OUT_DECOMP, row.names = FALSE)
write.csv(summary_df, OUT_SUMMARY, row.names = FALSE)

cat("\nWrote diagnostics outputs:\n")
cat("- ", OUT_BINS, "\n", sep = "")
cat("- ", OUT_HIST, "\n", sep = "")
cat("- ", OUT_DECOMP, "\n", sep = "")
cat("- ", OUT_SUMMARY, "\n", sep = "")
