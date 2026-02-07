# Prepare splits for V2 evaluation (holdout + temporal)

# ---- Configuração ----
DATA_PATH <- "data/processed/environment_flow_matrix.rds"
OUT_PATH  <- "outputs/v2_eval/splits.rds"
HOLDOUT_FRAC <- 0.20
SEEDS <- c(101, 102, 103, 104, 105)
TEST_PERIODS_TAIL <- 2  # last 2 periods for temporal generalization

flow <- readRDS(DATA_PATH)
rc <- flow$rc
bill_session <- as.integer(flow$bill.session)
T_periods <- as.integer(flow$T)
periods <- sort(unique(bill_session))

make_holdout_split <- function(seed) {
  set.seed(seed)
  test_list <- vector("list", length(periods))
  names(test_list) <- as.character(periods)

  for (t in periods) {
    j_idx <- which(bill_session == t)
    if (length(j_idx) == 0) next

    rc_sub <- rc[, j_idx, drop = FALSE]
    nonmiss <- which(rc_sub != 0L, arr.ind = TRUE)
    if (nrow(nonmiss) == 0) next

    n_test <- ceiling(HOLDOUT_FRAC * nrow(nonmiss))
    sel <- sample.int(nrow(nonmiss), n_test)

    i_idx <- nonmiss[sel, 1]
    j_idx_sel <- j_idx[nonmiss[sel, 2]]
    y_val <- rc_sub[cbind(i_idx, nonmiss[sel, 2])]

    test_list[[as.character(t)]] <- list(i = i_idx, j = j_idx_sel, y = y_val)
  }

  list(type = "holdout_by_period", seed = seed, test = test_list)
}

# Holdout splits
holdout_splits <- lapply(SEEDS, make_holdout_split)

# Temporal split (last periods)
if (length(periods) < TEST_PERIODS_TAIL) {
  stop("T_periods too small for temporal split.")
}

test_periods <- tail(periods, TEST_PERIODS_TAIL)
j_test <- which(bill_session %in% test_periods)
rc_sub <- rc[, j_test, drop = FALSE]
nonmiss <- which(rc_sub != 0L, arr.ind = TRUE)

i_idx <- nonmiss[, 1]
j_idx_sel <- j_test[nonmiss[, 2]]
y_val <- rc_sub[cbind(nonmiss[, 1], nonmiss[, 2])]

temporal_split <- list(
  type = "temporal",
  test_periods = test_periods,
  test = list(i = i_idx, j = j_idx_sel, y = y_val)
)

# Save
meta <- list(
  N = nrow(rc),
  J = ncol(rc),
  T_periods = T_periods,
  periods = periods,
  holdout_frac = HOLDOUT_FRAC,
  seeds = SEEDS,
  test_periods_tail = TEST_PERIODS_TAIL
)

dir.create("outputs/v2_eval", recursive = TRUE, showWarnings = FALSE)

saveRDS(list(
  holdout = holdout_splits,
  temporal = temporal_split,
  meta = meta
), OUT_PATH)

cat("Saved splits to outputs/v2_eval/splits.rds\n")
