#!/usr/bin/env Rscript
# Summarize top changes in ideal_points_mean between two snapshots.
# Usage: Rscript scripts/R/v5_snapshot_change_summary.R <domain> <iter_prev> <iter_curr> [top_n]
# Example: Rscript scripts/R/v5_snapshot_change_summary.R investment 1250 1500 10

args <- commandArgs(trailingOnly = TRUE)

domain <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "investment"
iter_prev <- if (length(args) >= 2) as.integer(args[2]) else 1250L
iter_curr <- if (length(args) >= 3) as.integer(args[3]) else 1500L
top_n <- if (length(args) >= 4) as.integer(args[4]) else 10L

if (is.na(iter_prev) || is.na(iter_curr)) stop("iter_prev and iter_curr must be integers")
if (iter_prev <= 0L || iter_curr <= 0L) stop("iter_prev and iter_curr must be positive")
if (is.na(top_n) || top_n < 1L) stop("top_n must be a positive integer")

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

out <- data.frame(
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

out <- out[order(out$delta_l2, decreasing = TRUE), , drop = FALSE]

out_file <- file.path("outputs/v5_per_domain_2d_diag",
                      paste0(domain, "_change_", iter_prev, "_to_", iter_curr, ".csv"))
write.csv(out, out_file, row.names = FALSE)

cat(sprintf("Domain: %s\n", domain))
cat(sprintf("Snapshot prev: %d | Snapshot curr: %d\n", iter_prev, iter_curr))
cat(sprintf("Rows: %d | Output: %s\n\n", nrow(out), out_file))

top_n <- min(top_n, nrow(out))
print(utils::head(out, top_n), row.names = FALSE)
