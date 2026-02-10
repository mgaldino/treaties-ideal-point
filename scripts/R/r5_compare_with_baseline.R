#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Please install it with install.packages('tibble').")
  }
  library(dplyr)
  library(readr)
  library(tibble)
})

message("R5 Phase 3: Compare R5 (3-year) with V7 baseline (5-year)")

domains <- c("investment", "security", "environment", "human_rights", "arms_control", "intellectual_property")

out_dir <- "outputs/r5_3year_windows"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

v7_dir <- "outputs/v7_country_anchors"
r5_dir <- "outputs/r5_3year_windows"

# Period mapping (R indexing)
# 5yr t=1..6 <-> 3yr t=1..10
map_tbl <- tibble::tibble(
  t5 = 1:6,
  period_5yr = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018"),
  t3 = c(1, 3, 5, 6, 8, 10),
  period_3yr = c("1990-1992", "1996-1998", "2002-2004", "2005-2007", "2011-2013", "2017-2018")
)

cmp_rows <- list()
trend_rows <- list()
report_lines <- c()

for (domain in domains) {
  v7_path <- file.path(v7_dir, paste0(domain, "_results.rds"))
  r5_path <- file.path(r5_dir, paste0(domain, "_results.rds"))
  if (!file.exists(v7_path)) stop(sprintf("Missing V7 results: %s", v7_path))
  if (!file.exists(r5_path)) stop(sprintf("Missing R5 results: %s", r5_path))

  v7 <- readRDS(v7_path)
  r5 <- readRDS(r5_path)

  if (is.null(v7$ideal_points) || is.null(r5$ideal_points)) stop(sprintf("Missing ideal_points in results for domain=%s", domain))
  if (dim(v7$ideal_points)[2] < 2 || dim(r5$ideal_points)[2] < 2) stop(sprintf("Expected K=2 in both results for domain=%s", domain))

  report_lines <- c(report_lines, sprintf("Domain: %s", domain))
  report_lines <- c(report_lines, sprintf("V7 periods (T=%d): %s", length(v7$period_labels), paste(v7$period_labels, collapse = ", ")))
  report_lines <- c(report_lines, sprintf("R5 periods (T=%d): %s", length(r5$period_labels), paste(r5$period_labels, collapse = ", ")))

  # Cross-sectional correlations at mapped periods.
  for (i in seq_len(nrow(map_tbl))) {
    t5 <- map_tbl$t5[i]
    t3 <- map_tbl$t3[i]

    c5 <- as.character(v7$country_codes)
    c3 <- as.character(r5$country_codes)
    common <- intersect(c5, c3)
    if (length(common) < 3) {
      cmp_rows[[length(cmp_rows) + 1L]] <- tibble::tibble(
        domain = domain,
        period_5yr = map_tbl$period_5yr[i],
        period_3yr = map_tbl$period_3yr[i],
        n_countries = length(common),
        cor_dim1 = NA_real_,
        cor_dim2 = NA_real_
      )
      next
    }

    idx5 <- match(common, c5)
    idx3 <- match(common, c3)

    x5_d1 <- v7$ideal_points[idx5, 1, t5]
    x5_d2 <- v7$ideal_points[idx5, 2, t5]
    x3_d1 <- r5$ideal_points[idx3, 1, t3]
    x3_d2 <- r5$ideal_points[idx3, 2, t3]

    ok1 <- is.finite(x5_d1) & is.finite(x3_d1)
    ok2 <- is.finite(x5_d2) & is.finite(x3_d2)

    cor_d1 <- if (sum(ok1) >= 3) stats::cor(x5_d1[ok1], x3_d1[ok1]) else NA_real_
    cor_d2 <- if (sum(ok2) >= 3) stats::cor(x5_d2[ok2], x3_d2[ok2]) else NA_real_

    cmp_rows[[length(cmp_rows) + 1L]] <- tibble::tibble(
      domain = domain,
      period_5yr = map_tbl$period_5yr[i],
      period_3yr = map_tbl$period_3yr[i],
      n_countries = length(common),
      cor_dim1 = cor_d1,
      cor_dim2 = cor_d2
    )
  }

  # Aggregate trend correlation for dim1, using mapped periods.
  v7_mean_dim1 <- v7$aggregate$mean_dim1
  r5_mean_dim1 <- r5$aggregate$mean_dim1
  if (length(v7_mean_dim1) != 6) {
    v7_mean_dim1 <- sapply(seq_len(dim(v7$ideal_points)[3]), function(t) mean(v7$ideal_points[, 1, t], na.rm = TRUE))
  }
  if (length(r5_mean_dim1) != 10) {
    r5_mean_dim1 <- sapply(seq_len(dim(r5$ideal_points)[3]), function(t) mean(r5$ideal_points[, 1, t], na.rm = TRUE))
  }

  v7_mapped <- v7_mean_dim1[map_tbl$t5]
  r5_mapped <- r5_mean_dim1[map_tbl$t3]
  trend_cor <- stats::cor(v7_mapped, r5_mapped)

  trend_rows[[length(trend_rows) + 1L]] <- tibble::tibble(
    domain = domain,
    cor_trend_dim1_mapped = trend_cor
  )

  report_lines <- c(report_lines, sprintf("Trend correlation (mean dim1, mapped 6 pairs): %.3f", trend_cor))
  report_lines <- c(report_lines, "")
}

cmp <- dplyr::bind_rows(cmp_rows)
trend <- dplyr::bind_rows(trend_rows)

cmp_path <- file.path(out_dir, "comparison_table.csv")
trend_path <- file.path(out_dir, "trend_comparison.csv")
report_path <- file.path(out_dir, "comparison_report.txt")

readr::write_csv(cmp, cmp_path)
readr::write_csv(trend, trend_path)
writeLines(report_lines, report_path, useBytes = TRUE)

message(sprintf("Saved: %s", cmp_path))
message(sprintf("Saved: %s", trend_path))
message(sprintf("Saved: %s", report_path))

message("\nR5 Phase 3 completed.")

