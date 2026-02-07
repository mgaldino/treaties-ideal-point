#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

parse_args <- function(args) {
  out <- list(
    manifest_dir = "data/processed",
    out_path = NA_character_
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
  out
}

read_manifest <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(manifest_file = basename(path))
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  manifest_dir <- args$manifest_dir
  manifest_files <- list.files(
    manifest_dir,
    pattern = "^wits_tariffs_manifest_.*\\.csv$",
    full.names = TRUE
  )
  if (length(manifest_files) == 0) {
    stop("No manifest files found in: ", manifest_dir)
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

  summary_counts <- manifest %>%
    group_by(reporter_iso3, year) %>%
    summarise(
      attempts_total = n(),
      success_count = sum(status == "success", na.rm = TRUE),
      no_data_count = sum(status == "no_data", na.rm = TRUE),
      failed_count = sum(status == "failed", na.rm = TRUE),
      partial_error_count = sum(status == "partial_error", na.rm = TRUE),
      skipped_count = sum(status == "skipped", na.rm = TRUE),
      first_attempt_utc = min(timestamp_utc, na.rm = TRUE),
      last_attempt_utc = max(timestamp_utc, na.rm = TRUE),
      .groups = "drop"
    )

  latest <- manifest %>%
    arrange(reporter_iso3, year, timestamp_utc) %>%
    group_by(reporter_iso3, year) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    dplyr::select(
      reporter_iso3,
      year,
      last_status = status,
      last_exit_code = exit_code,
      last_retry_count = retry_count,
      last_partner_count = partner_count,
      last_error_count = error_count,
      last_errors = errors,
      last_dns_error = dns_error,
      last_manifest_file = manifest_file,
      last_out_dir = out_dir
    )

  summary <- summary_counts %>%
    left_join(latest, by = c("reporter_iso3", "year")) %>%
    arrange(reporter_iso3, year)

  out_path <- args$out_path
  if (is.na(out_path) || out_path == "") {
    timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    out_path <- file.path(
      manifest_dir,
      paste0("wits_tariffs_manifest_summary_", timestamp, ".csv")
    )
  }
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(summary, out_path)
  message("Wrote: ", out_path)
}

main()
