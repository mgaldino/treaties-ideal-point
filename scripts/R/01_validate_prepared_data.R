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

output_dir <- "outputs/validation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990
year_max <- 2018

baseline_path <- "data/processed/baseline_events.csv"
items_path <- "data/processed/item_codebook.csv"

if (!file.exists(baseline_path) || !file.exists(items_path)) {
  stop("Missing baseline outputs. Run scripts/R/01_prepare_data.R first.")
}

events <- readr::read_csv(baseline_path, show_col_types = FALSE)
items <- readr::read_csv(items_path, show_col_types = FALSE)

# Baseline checks
if (any(events$event_year < year_min, na.rm = TRUE) || any(events$event_year > year_max, na.rm = TRUE)) {
  stop("Found event_year outside 1990–2018 window.")
}

dup_pairs <- events %>%
  dplyr::count(issue_area, iso3, item_id, name = "n") %>%
  dplyr::filter(n > 1)
if (nrow(dup_pairs) > 0) {
  stop("Duplicate country-item pairs found in baseline events.")
}

# Flow matrix checks
issue_areas <- c("trade", "investment", "security", "environment")

for (area in issue_areas) {
  path <- file.path("data/processed", paste0(area, "_flow_matrix.rds"))
  if (!file.exists(path)) {
    stop("Missing flow matrix: ", path)
  }
  flow <- readRDS(path)

  rc <- flow$rc
  if (!is.matrix(rc)) {
    stop("rc is not a matrix for issue area: ", area)
  }

  if (!all(rc %in% c(-1, 0, 1))) {
    stop("rc contains values outside -1, 0, 1 for issue area: ", area)
  }

  if (nrow(rc) != length(flow$country_codes)) {
    stop("Row count mismatch with country_codes for issue area: ", area)
  }

  if (ncol(rc) != length(flow$item_labels)) {
    stop("Column count mismatch with item_labels for issue area: ", area)
  }

  if (ncol(flow$bill.session) != 1 || nrow(flow$bill.session) != ncol(rc)) {
    stop("bill.session dimension mismatch for issue area: ", area)
  }

  if (any(flow$bill.session < 0) || any(flow$bill.session > (flow$T - 1))) {
    stop("bill.session out of range for issue area: ", area)
  }

  if (nrow(flow$startlegis) != nrow(rc)) {
    stop("startlegis dimension mismatch for issue area: ", area)
  }

  if (nrow(flow$endlegis) != nrow(rc)) {
    stop("endlegis dimension mismatch for issue area: ", area)
  }

  # Variation checks
  col_nonzero <- colSums(rc != 0) > 0
  if (!all(col_nonzero)) {
    stop("Found all-zero columns in rc for issue area: ", area)
  }

  col_variation <- apply(rc, 2, function(x) {
    vals <- unique(x[x != 0])
    length(vals) >= 2
  })
  if (!all(col_variation)) {
    stop("Found columns with no variation for issue area: ", area)
  }

  row_nonzero <- rowSums(rc != 0) > 0
  if (!all(row_nonzero)) {
    stop("Found all-zero rows in rc for issue area: ", area)
  }

  # Summary
  total <- length(rc)
  n_zero <- sum(rc == 0)
  n_one <- sum(rc == 1)
  n_minus <- sum(rc == -1)

  summary_df <- tibble::tibble(
    issue_area = area,
    countries = nrow(rc),
    items = ncol(rc),
    T = flow$T,
    share_zero = n_zero / total,
    share_one = n_one / total,
    share_minus_one = n_minus / total
  )

  readr::write_csv(summary_df, file.path(output_dir, paste0("flow_matrix_summary_", area, ".csv")))
}

cat("Phase 1 validation passed. Summaries written to outputs/validation/\n")
