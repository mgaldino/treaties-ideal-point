# Orchestrate V2 evaluation: holdout, temporal, robustness

suppressPackageStartupMessages({
  library(emIRT)
})

source("scripts/R/v2_eval/run_models.R")
source("scripts/R/v2_eval/score_metrics.R")

DATA_PATH <- "data/processed/environment_flow_matrix.rds"
SPLITS_PATH <- "outputs/v2_eval/splits.rds"
SUMMARY_CSV <- "outputs/v2_eval/summary.csv"

# Control parameters
MAXIT <- 1500L
THRESH <- 1e-6
CHECKFREQ <- 50L
NCORES <- 1L

flow <- readRDS(DATA_PATH)

# Build priors (anchors DNK/SAU)
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

# Prepare splits if missing
if (!file.exists(SPLITS_PATH)) {
  source("scripts/R/v2_eval/prepare_splits.R")
}

splits <- readRDS(SPLITS_PATH)

results <- list()
summary_rows <- list()

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

  # Starts (seeded for reproducibility)
  if (!is.null(split$seed)) set.seed(split$seed)
  starts <- build_starts(rc_train, flow$T)

  # Run models
  t0_em <- proc.time()
  res_em <- run_emirt(flow, rc_train, starts, priors, control_emirt)
  time_em <- as.numeric((proc.time() - t0_em)["elapsed"])

  t0_kd <- proc.time()
  res_kd <- run_dynirt_kd(flow, rc_train, starts, priors, control_kd, K = K)
  time_kd <- as.numeric((proc.time() - t0_kd)["elapsed"])

  # Score metrics
  metrics_em <- score_metrics(res_em$means$x, res_em$means$alpha, res_em$means$beta,
                              as.integer(flow$bill.session), test, K = K)
  metrics_kd <- score_metrics(res_kd$means$x, res_kd$means$alpha, res_kd$means$beta,
                              as.integer(flow$bill.session), test, K = K)

  list(
    split_id = split_id,
    split = split,
    metrics_em = metrics_em,
    metrics_kd = metrics_kd,
    time_em = time_em,
    time_kd = time_kd,
    iters_em = res_em$runtime$iters,
    iters_kd = res_kd$runtime$iters
  )
}

# Holdout splits (robustness via 5 seeds)
for (sp in splits$holdout) {
  split_id <- sprintf("holdout_seed_%d", sp$seed)
  cat(sprintf("\nRunning %s...\n", split_id))
  results[[split_id]] <- process_split(sp, split_id)
}

# Temporal split
split_id <- "temporal_last2"
cat(sprintf("\nRunning %s...\n", split_id))
results[[split_id]] <- process_split(splits$temporal, split_id)

# Save detailed results
out_dir <- "outputs/v2_eval"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(results, file.path(out_dir, "results.rds"))

# Build summary CSV
for (id in names(results)) {
  r <- results[[id]]
  summary_rows[[id]] <- data.frame(
    split_id = id,
    split_type = r$split$type,
    seed = if (is.null(r$split$seed)) NA_integer_ else r$split$seed,
    n_test = r$metrics_em$n,
    logscore_em = r$metrics_em$logscore,
    logscore_mean_em = r$metrics_em$logscore_mean,
    brier_em = r$metrics_em$brier,
    logscore_kd = r$metrics_kd$logscore,
    logscore_mean_kd = r$metrics_kd$logscore_mean,
    brier_kd = r$metrics_kd$brier,
    time_em = r$time_em,
    time_kd = r$time_kd,
    iters_em = r$iters_em,
    iters_kd = r$iters_kd
  )
}

summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, SUMMARY_CSV, row.names = FALSE)

cat("\nSaved detailed results to outputs/v2_eval/results.rds\n")
cat("Saved summary to outputs/v2_eval/summary.csv\n")
