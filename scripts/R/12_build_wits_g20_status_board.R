#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

parse_bool <- function(value, default = FALSE) {
  if (is.na(value) || value == "") {
    return(default)
  }
  tolower(value) %in% c("1", "true", "yes", "y")
}

parse_args <- function(args) {
  out <- list(
    start_year = 1990,
    end_year = 2022,
    manifest_dir = "data/processed",
    raw_dir = "data/raw/tariffs/wits_trn",
    out_dir = "data/processed",
    include_eun = FALSE
  )
  if (length(args) == 0) {
    return(out)
  }
  if (length(args) %% 2 != 0) {
    stop("Arguments must be provided as --key value pairs.")
  }
  for (i in seq(1, length(args), by = 2)) {
    key <- gsub("^--", "", args[i])
    key <- gsub("-", "_", key)
    out[[key]] <- args[i + 1]
  }
  out$start_year <- as.integer(out$start_year)
  out$end_year <- as.integer(out$end_year)
  out$include_eun <- parse_bool(out$include_eun, default = FALSE)
  out
}

read_manifest <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(manifest_file = basename(path))
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  g20 <- c(
    "ARG", "AUS", "BRA", "CAN", "CHN", "DEU", "FRA", "GBR",
    "IDN", "IND", "ITA", "JPN", "KOR", "MEX", "RUS", "SAU",
    "TUR", "USA", "ZAF", "EUN"
  )
  if (!args$include_eun) {
    g20 <- setdiff(g20, "EUN")
  }

  years <- seq(args$start_year, args$end_year)
  manifest_files <- list.files(
    args$manifest_dir,
    pattern = "^wits_tariffs_manifest_.*\\.csv$",
    full.names = TRUE
  )
  if (length(manifest_files) == 0) {
    stop("No manifest files found in: ", args$manifest_dir)
  }

  manifest <- manifest_files %>%
    lapply(read_manifest) %>%
    bind_rows() %>%
    mutate(
      reporter_iso3 = toupper(reporter_iso3),
      year = as.integer(year),
      timestamp_utc = as.POSIXct(
        timestamp_utc,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      )
    )

  last_status <- manifest %>%
    arrange(reporter_iso3, year, timestamp_utc) %>%
    group_by(reporter_iso3, year) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    dplyr::select(
      reporter_iso3,
      year,
      last_status = status,
      last_attempt_utc = timestamp_utc,
      last_errors = errors,
      last_manifest_file = manifest_file,
      last_out_dir = out_dir
    )

  board <- expand.grid(
    reporter_iso3 = g20,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(reporter_iso3 = toupper(reporter_iso3)) %>%
    left_join(last_status, by = c("reporter_iso3", "year")) %>%
    mutate(
      raw_dir_present = file.exists(
        file.path(args$raw_dir, reporter_iso3, as.character(year))
      )
    ) %>%
    arrange(reporter_iso3, year)

  summary <- board %>%
    group_by(reporter_iso3) %>%
    summarise(
      years_total = n(),
      raw_years = sum(raw_dir_present, na.rm = TRUE),
      manifest_years = sum(!is.na(last_status)),
      success_years = sum(last_status == "success", na.rm = TRUE),
      no_data_years = sum(last_status == "no_data", na.rm = TRUE),
      partial_error_years = sum(last_status == "partial_error", na.rm = TRUE),
      failed_years = sum(last_status == "failed", na.rm = TRUE),
      skipped_years = sum(last_status == "skipped", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      overall_status = case_when(
        failed_years > 0 ~ "failed",
        partial_error_years > 0 ~ "partial_error",
        no_data_years > 0 ~ "no_data",
        manifest_years == years_total ~ "success",
        manifest_years == 0 & raw_years > 0 ~ "raw_only",
        manifest_years == 0 & raw_years == 0 ~ "not_attempted",
        TRUE ~ "mixed"
      ),
      remaining_for_batch = manifest_years == 0 & raw_years == 0
    ) %>%
    arrange(reporter_iso3)

  remaining <- summary %>%
    filter(remaining_for_batch) %>%
    dplyr::select(reporter_iso3, overall_status)

  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  status_board_path <- file.path(
    args$out_dir,
    paste0("wits_tariffs_g20_status_board_", timestamp, ".csv")
  )
  summary_path <- file.path(
    args$out_dir,
    paste0("wits_tariffs_g20_reporter_status_", timestamp, ".csv")
  )
  remaining_path <- file.path(
    args$out_dir,
    paste0("wits_tariffs_g20_remaining_reporters_", timestamp, ".csv")
  )

  dir.create(dirname(status_board_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(board, status_board_path)
  readr::write_csv(summary, summary_path)
  readr::write_csv(remaining, remaining_path)

  message("Wrote: ", status_board_path)
  message("Wrote: ", summary_path)
  message("Wrote: ", remaining_path)
}

main()
