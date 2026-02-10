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

message("R5 Phase 1: Build 3-year window flow matrices (T=10)")

output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990L
year_max <- 2018L

# 3-year windows (last window is 2 years)
period_breaks <- c(1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2019)
period_labels <- c(
  "1990-1992", "1993-1995", "1996-1998", "1999-2001", "2002-2004",
  "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2018"
)

assign_period <- function(year) {
  cut(year, breaks = period_breaks, labels = period_labels, right = FALSE, include.lowest = TRUE)
}

period_index <- function(label) {
  match(label, period_labels) - 1L
}

events_path <- file.path(output_dir, "baseline_events.csv")
items_path <- file.path(output_dir, "item_codebook.csv")
if (!file.exists(events_path) || !file.exists(items_path)) {
  stop("Missing baseline extracts. Run scripts/R/01_prepare_data.R first to generate data/processed/baseline_events.csv and item_codebook.csv.")
}

all_events <- readr::read_csv(events_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    event_year = suppressWarnings(as.integer(.data$event_year)),
    iso3 = as.character(.data$iso3),
    item_id = as.character(.data$item_id),
    issue_area = as.character(.data$issue_area)
  )

all_items <- readr::read_csv(items_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    treaty_open_year = suppressWarnings(as.integer(.data$treaty_open_year)),
    item_id = as.character(.data$item_id),
    issue_area = as.character(.data$issue_area)
  )

# Basic validations (logical consistency of dates)
bad_year <- all_events %>%
  dplyr::filter(is.na(.data$event_year) | .data$event_year < year_min | .data$event_year > year_max)
if (nrow(bad_year) > 0) {
  msg <- sprintf("Found %d events with event_year outside [%d,%d] or missing; these will be dropped.",
                 nrow(bad_year), year_min, year_max)
  message(msg)
  all_events <- all_events %>%
    dplyr::filter(!is.na(.data$event_year) & .data$event_year >= year_min & .data$event_year <= year_max)
}

domains <- c("investment", "security", "environment", "human_rights", "arms_control", "intellectual_property")

# Same flow-matrix logic as scripts/R/01_prepare_data.R, but with 3-year periods.
build_flow_matrix_3yr <- function(events, items, issue_area) {
  ev <- events %>% dplyr::filter(.data$issue_area == !!issue_area)
  it <- items %>% dplyr::filter(.data$issue_area == !!issue_area)

  if (nrow(ev) == 0 || nrow(it) == 0) {
    return(NULL)
  }

  ev <- ev %>%
    dplyr::mutate(event_period = as.character(assign_period(.data$event_year)))

  it <- it %>%
    dplyr::mutate(open_period = as.character(assign_period(.data$treaty_open_year)))

  # For each country-item, keep the first observed event (baseline logic).
  ev <- ev %>%
    dplyr::group_by(.data$iso3, .data$item_id) %>%
    dplyr::summarise(
      event_period = dplyr::first(.data$event_period),
      event_year = dplyr::first(.data$event_year),
      .groups = "drop"
    )

  # Country start periods (proxy: first observed event).
  start_period <- ev %>%
    dplyr::filter(!is.na(.data$event_period)) %>%
    dplyr::mutate(start_index = period_index(.data$event_period)) %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::summarise(start_index = min(.data$start_index, na.rm = TRUE), .groups = "drop")

  # Phantom items: replicate each treaty-item over periods from open year forward.
  phantom <- expand.grid(
    item_id = it$item_id,
    period_label = period_labels,
    stringsAsFactors = FALSE
  )

  phantom <- phantom %>%
    dplyr::left_join(it, by = "item_id") %>%
    dplyr::mutate(
      period_index = period_index(.data$period_label),
      open_index = period_index(.data$open_period),
      phantom_item_id = paste0(.data$item_id, "__", .data$period_label)
    )

  phantom <- phantom %>%
    dplyr::filter(!is.na(.data$open_index)) %>%
    dplyr::filter(.data$period_index >= .data$open_index)

  # Country-item join period (event year binned into 3-year period).
  ev_join <- ev %>%
    dplyr::mutate(join_index = period_index(.data$event_period)) %>%
    dplyr::select(iso3, item_id, join_index)

  # Grid: all observed countries x all phantom items.
  grid <- expand.grid(
    iso3 = unique(ev$iso3),
    phantom_item_id = phantom$phantom_item_id,
    stringsAsFactors = FALSE
  )

  grid <- grid %>%
    dplyr::left_join(
      phantom %>% dplyr::select(phantom_item_id, item_id, period_index, open_index),
      by = "phantom_item_id"
    ) %>%
    dplyr::left_join(ev_join, by = c("iso3", "item_id")) %>%
    dplyr::left_join(start_period %>% dplyr::select(iso3, start_index), by = "iso3")

  # Flow coding:
  # +1 = join in this period; -1 = did not join yet (opportunity); 0 = not applicable/missing.
  grid$rc <- ifelse(
    grid$period_index < grid$open_index,
    0L,
    ifelse(
      grid$period_index < grid$start_index,
      0L,
      ifelse(
        !is.na(grid$join_index) & grid$period_index == grid$join_index,
        1L,
        ifelse(
          !is.na(grid$join_index) & grid$period_index > grid$join_index,
          0L,
          -1L
        )
      )
    )
  )

  countries <- unique(ev$iso3)
  phantom_items <- phantom$phantom_item_id

  rc_matrix <- matrix(0L, nrow = length(countries), ncol = length(phantom_items))
  row_idx <- match(grid$iso3, countries)
  col_idx <- match(grid$phantom_item_id, phantom_items)
  rc_matrix[cbind(row_idx, col_idx)] <- grid$rc

  # Drop all-zero rows.
  row_keep <- rowSums(rc_matrix != 0) > 0
  dropped_countries <- countries[!row_keep]
  rc_matrix <- rc_matrix[row_keep, , drop = FALSE]
  countries <- countries[row_keep]

  # Drop columns with no variation or all-zero.
  col_nonzero <- colSums(rc_matrix != 0) > 0
  col_variation <- apply(rc_matrix, 2, function(x) {
    vals <- unique(x[x != 0])
    length(vals) >= 2
  })
  col_keep <- col_nonzero & col_variation
  dropped_items <- phantom_items[!col_keep]
  rc_matrix <- rc_matrix[, col_keep, drop = FALSE]
  phantom_items <- phantom_items[col_keep]
  phantom <- phantom %>% dplyr::filter(.data$phantom_item_id %in% phantom_items)

  # startlegis/endlegis/bill.session (0-indexed).
  startlegis <- start_period %>%
    dplyr::filter(.data$iso3 %in% countries) %>%
    dplyr::arrange(match(.data$iso3, countries)) %>%
    dplyr::pull(start_index)

  startlegis[is.na(startlegis)] <- 0L
  endlegis <- rep(length(period_labels) - 1L, length(countries))

  bill_session <- phantom %>%
    dplyr::arrange(match(.data$phantom_item_id, phantom_items)) %>%
    dplyr::pull(period_index)

  list(
    rc = rc_matrix,
    startlegis = matrix(as.integer(startlegis), ncol = 1),
    endlegis = matrix(as.integer(endlegis), ncol = 1),
    bill.session = matrix(as.integer(bill_session), ncol = 1),
    T = length(period_labels),
    country_codes = countries,
    item_labels = phantom_items,
    period_labels = period_labels,
    dropped_countries = dropped_countries,
    dropped_items = dropped_items
  )
}

summaries <- list()
for (domain in domains) {
  message(sprintf("\nBuilding 3-year flow matrix: %s", domain))
  flow <- build_flow_matrix_3yr(all_events, all_items, domain)
  if (is.null(flow)) {
    message(sprintf("Skipping %s: no events/items.", domain))
    next
  }

  out_path <- file.path(output_dir, paste0(domain, "_flow_matrix_3year.rds"))
  saveRDS(flow, out_path)

  N <- nrow(flow$rc)
  J <- ncol(flow$rc)
  T_periods <- flow$T
  density <- mean(flow$rc != 0) * 100

  # Sanity checks.
  if (T_periods != 10L) stop(sprintf("Expected T=10 for %s; got %d", domain, T_periods))
  bs_rng <- range(as.integer(flow$bill.session))
  if (bs_rng[1] < 0L || bs_rng[2] > 9L) stop(sprintf("bill.session out of range for %s: [%d,%d]", domain, bs_rng[1], bs_rng[2]))

  message(sprintf("Saved: %s", out_path))
  message(sprintf("Summary %s: N=%d, J=%d, T=%d, density=%.2f%% non-zero", domain, N, J, T_periods, density))

  summaries[[domain]] <- tibble::tibble(
    domain = domain,
    N = N,
    J = J,
    T = T_periods,
    density_pct = density
  )
}

if (length(summaries) > 0) {
  summary_df <- dplyr::bind_rows(summaries)
  message("\nR5 Phase 1 summary table:")
  print(summary_df)
}

message("\nR5 Phase 1 completed.")
