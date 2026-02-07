#!/usr/bin/env Rscript
# Diagnose countries with largest shifts in ideal_points_mean between snapshots.
# Usage: Rscript scripts/R/v5_country_diagnostics.R <domain> <iter_prev> <iter_curr> [top_n]
# Example: Rscript scripts/R/v5_country_diagnostics.R investment 1250 1500 10

args <- commandArgs(trailingOnly = TRUE)

domain <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "investment"
iter_prev <- if (length(args) >= 2) as.integer(args[2]) else 1250L
iter_curr <- if (length(args) >= 3) as.integer(args[3]) else 1500L
top_n <- if (length(args) >= 4) as.integer(args[4]) else 10L

if (is.na(iter_prev) || is.na(iter_curr)) stop("iter_prev and iter_curr must be integers")
if (iter_prev <= 0L || iter_curr <= 0L) stop("iter_prev and iter_curr must be positive")
if (is.na(top_n) || top_n < 1L) stop("top_n must be a positive integer")

flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
if (!file.exists(flow_path)) stop("Missing: ", flow_path)
flow <- readRDS(flow_path)

change_file <- file.path(
  "outputs/v5_per_domain_2d_diag",
  paste0(domain, "_change_", iter_prev, "_to_", iter_curr, ".csv")
)

load_change <- function(file, domain, iter_prev, iter_curr) {
  if (!file.exists(file)) return(NULL)
  read.csv(file, stringsAsFactors = FALSE)
}

compute_change <- function(domain, iter_prev, iter_curr) {
  file_prev <- file.path("outputs/v5_per_domain_2d_diag",
                         paste0(domain, "_snapshot_iter", iter_prev, ".rds"))
  file_curr <- file.path("outputs/v5_per_domain_2d_diag",
                         paste0(domain, "_snapshot_iter", iter_curr, ".rds"))
  if (!file.exists(file_prev)) stop("Missing: ", file_prev)
  if (!file.exists(file_curr)) stop("Missing: ", file_curr)

  snap_prev <- readRDS(file_prev)
  snap_curr <- readRDS(file_curr)
  x_prev <- snap_prev$ideal_points_mean
  x_curr <- snap_curr$ideal_points_mean

  if (is.null(rownames(x_prev)) || is.null(rownames(x_curr))) {
    stop("ideal_points_mean must have rownames with country codes")
  }
  common <- intersect(rownames(x_prev), rownames(x_curr))
  if (length(common) == 0) stop("No overlapping country codes between snapshots")

  x_prev <- x_prev[common, , drop = FALSE]
  x_curr <- x_curr[common, , drop = FALSE]

  delta <- x_curr - x_prev
  delta_l2 <- sqrt(rowSums(delta^2))
  delta_l1 <- rowSums(abs(delta))
  delta_max <- apply(abs(delta), 1L, max)

  data.frame(
    country_code = common,
    dim1_prev = x_prev[, 1],
    dim2_prev = x_prev[, 2],
    dim1_curr = x_curr[, 1],
    dim2_curr = x_curr[, 2],
    delta_dim1 = delta[, 1],
    delta_dim2 = delta[, 2],
    delta_l2 = delta_l2,
    delta_l1 = delta_l1,
    delta_max = delta_max,
    stringsAsFactors = FALSE
  )
}

change_df <- load_change(change_file, domain, iter_prev, iter_curr)
if (is.null(change_df)) {
  change_df <- compute_change(domain, iter_prev, iter_curr)
  change_df <- change_df[order(change_df$delta_l2, decreasing = TRUE), , drop = FALSE]
  write.csv(change_df, change_file, row.names = FALSE)
}

change_df <- change_df[order(change_df$delta_l2, decreasing = TRUE), , drop = FALSE]
top_n <- min(top_n, nrow(change_df))
top_codes <- change_df$country_code[seq_len(top_n)]

rc <- flow$rc
J <- ncol(rc)
bill_session <- as.integer(flow$bill.session)
country_codes <- flow$country_codes
startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)

periods <- sort(unique(bill_session))
T_total <- flow$T

diagnose_country <- function(code) {
  idx <- match(code, country_codes)
  if (is.na(idx)) return(NULL)

  votes <- rc[idx, ]
  n_obs <- sum(votes != 0L)
  n_pos <- sum(votes == 1L)
  n_neg <- sum(votes == -1L)
  n_missing <- J - n_obs
  pct_missing <- if (J > 0) n_missing / J else NA_real_

  s <- startlegis[idx]
  e <- endlegis[idx]
  active_periods <- e - s + 1L
  if (active_periods < 1L) active_periods <- 0L

  active_item_mask <- bill_session >= s & bill_session <= e
  active_items_total <- sum(active_item_mask)
  obs_rate_active <- if (active_items_total > 0) n_obs / active_items_total else NA_real_

  obs_by_period <- rep(0L, length(periods))
  names(obs_by_period) <- periods
  for (p in periods) {
    j_idx <- which(bill_session == p)
    if (length(j_idx) > 0) {
      obs_by_period[as.character(p)] <- sum(votes[j_idx] != 0L)
    }
  }

  active_periods_with_votes <- 0L
  min_obs_active <- NA_integer_
  max_obs_active <- NA_integer_
  mean_obs_active <- NA_real_
  if (active_periods > 0L) {
    active_p <- periods[periods >= s & periods <= e]
    if (length(active_p) > 0) {
      obs_active <- obs_by_period[as.character(active_p)]
      active_periods_with_votes <- sum(obs_active > 0L)
      min_obs_active <- min(obs_active)
      max_obs_active <- max(obs_active)
      mean_obs_active <- mean(obs_active)
    }
  }

  data.frame(
    country_code = code,
    idx = idx,
    startlegis = s,
    endlegis = e,
    active_periods = active_periods,
    n_items_total = J,
    n_items_active = active_items_total,
    n_obs = n_obs,
    n_pos = n_pos,
    n_neg = n_neg,
    n_missing = n_missing,
    pct_missing = pct_missing,
    obs_rate_active = obs_rate_active,
    active_periods_with_votes = active_periods_with_votes,
    prop_active_periods_with_votes = if (active_periods > 0L) {
      active_periods_with_votes / active_periods
    } else NA_real_,
    mean_obs_per_active_period = mean_obs_active,
    min_obs_active_period = min_obs_active,
    max_obs_active_period = max_obs_active,
    stringsAsFactors = FALSE
  )
}

diag_list <- lapply(top_codes, diagnose_country)
diag_df <- do.call(rbind, diag_list)

out_df <- merge(change_df, diag_df, by = "country_code", all.x = TRUE)
out_df <- out_df[order(out_df$delta_l2, decreasing = TRUE), , drop = FALSE]

out_file <- file.path(
  "outputs/v5_per_domain_2d_diag",
  paste0(domain, "_country_diagnostics_", iter_prev, "_to_", iter_curr, ".csv")
)
write.csv(out_df, out_file, row.names = FALSE)

cat(sprintf("Domain: %s\n", domain))
cat(sprintf("Snapshot prev: %d | Snapshot curr: %d\n", iter_prev, iter_curr))
cat(sprintf("Top_n: %d | Output: %s\n\n", top_n, out_file))

print(utils::head(out_df, top_n), row.names = FALSE)
