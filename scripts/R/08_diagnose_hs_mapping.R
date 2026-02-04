#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

parse_args <- function(args) {
  out <- list(
    reporter_iso3 = "",
    years = "",
    top_n = 20,
    in_root = "data/processed/tariffs/wits_trn",
    validation_root = "outputs/validation"
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
  out$top_n <- as.integer(out$top_n)
  out
}

list_targets <- function(root, reporters, years) {
  reporter_dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  reporter_codes <- basename(reporter_dirs)
  targets <- list()
  for (i in seq_along(reporter_dirs)) {
    reporter <- reporter_codes[[i]]
    if (length(reporters) > 0 && !(reporter %in% reporters)) {
      next
    }
    year_dirs <- list.dirs(reporter_dirs[[i]], full.names = TRUE, recursive = FALSE)
    for (yd in year_dirs) {
      year <- basename(yd)
      if (length(years) > 0 && !(year %in% years)) {
        next
      }
      targets[[length(targets) + 1]] <- list(reporter = reporter, year = year, dir = yd)
    }
  }
  targets
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  dir.create(args$validation_root, recursive = TRUE, showWarnings = FALSE)

  reporters <- if (nzchar(args$reporter_iso3)) toupper(strsplit(args$reporter_iso3, ",")[[1]]) else character(0)
  years <- if (nzchar(args$years)) strsplit(args$years, ",")[[1]] else character(0)

  targets <- list_targets(args$in_root, reporters, years)
  if (length(targets) == 0) {
    stop("No targets found to process.")
  }

  sensitivity_all <- list()

  for (target in targets) {
    reporter <- target$reporter
    year <- target$year

    mapped_path <- file.path(target$dir, "line_level_ad_valorem_hs2012.csv")
    raw_path <- file.path(target$dir, "line_level_ad_valorem.csv")
    if (!file.exists(mapped_path) || !file.exists(raw_path)) {
      next
    }

    mapped <- readr::read_csv(mapped_path, show_col_types = FALSE) %>%
      dplyr::select(reporter_iso3, year, partner_code, partner_is_group, hs88, hs6_code, PRODUCTCODE_LABEL, mapping_missing, ambiguous_mapping, hs2012)

    mapped <- mapped %>%
      mutate(
        hs2 = str_sub(hs88, 1, 2),
        hs4_mapped = if_else(!is.na(hs2012), str_sub(hs2012, 1, 4), NA_character_),
        mapping_missing = if_else(is.na(mapping_missing), TRUE, mapping_missing),
        ambiguous_mapping = if_else(is.na(ambiguous_mapping), FALSE, ambiguous_mapping)
      )

    by_hs2 <- mapped %>%
      group_by(reporter_iso3, year, hs2) %>%
      summarise(
        lines = n(),
        missing_lines = sum(mapping_missing),
        ambiguous_lines = sum(ambiguous_mapping),
        missing_share = if_else(lines > 0, missing_lines / lines, NA_real_),
        ambiguous_share = if_else(lines > 0, ambiguous_lines / lines, NA_real_),
        .groups = "drop"
      )

    out_hs2 <- file.path(args$validation_root, sprintf("wits_hs_mapping_by_hs2_%s_%s.csv", reporter, year))
    readr::write_csv(by_hs2, out_hs2)

    total_lines <- nrow(mapped)
    top_missing <- mapped %>%
      filter(mapping_missing) %>%
      mutate(hs6 = if_else(!is.na(hs88), hs88, hs6_code)) %>%
      group_by(reporter_iso3, year, hs6, PRODUCTCODE_LABEL) %>%
      summarise(
        missing_lines = n(),
        missing_share_total = if_else(total_lines > 0, missing_lines / total_lines, NA_real_),
        .groups = "drop"
      ) %>%
      arrange(desc(missing_lines)) %>%
      slice_head(n = args$top_n)

    out_top <- file.path(args$validation_root, sprintf("wits_hs_mapping_top_missing_%s_%s.csv", reporter, year))
    readr::write_csv(top_missing, out_top)

    raw <- readr::read_csv(raw_path, show_col_types = FALSE) %>%
      dplyr::select(hs4_code_raw)

    raw_hs4 <- raw %>%
      distinct(hs4_code_raw) %>%
      filter(!is.na(hs4_code_raw))

    mapped_hs4 <- mapped %>%
      filter(!mapping_missing, !is.na(hs4_mapped)) %>%
      distinct(hs4_mapped)

    sensitivity <- tibble(
      reporter_iso3 = reporter,
      year = as.integer(year),
      raw_hs4_count = nrow(raw_hs4),
      mapped_hs4_count = nrow(mapped_hs4),
      hs4_coverage_ratio = if_else(raw_hs4_count > 0, mapped_hs4_count / raw_hs4_count, NA_real_),
      raw_lines = nrow(raw),
      mapped_lines = sum(!mapped$mapping_missing),
      mapped_line_share = if_else(raw_lines > 0, mapped_lines / raw_lines, NA_real_)
    )

    out_sens <- file.path(args$validation_root, sprintf("wits_hs_mapping_sensitivity_%s_%s.csv", reporter, year))
    readr::write_csv(sensitivity, out_sens)

    sensitivity_all[[length(sensitivity_all) + 1]] <- sensitivity
  }

  if (length(sensitivity_all) > 0) {
    sensitivity_all <- bind_rows(sensitivity_all)
    out_all <- file.path(args$validation_root, "wits_hs_mapping_sensitivity_all.csv")
    readr::write_csv(sensitivity_all, out_all)
  }
}

main()
