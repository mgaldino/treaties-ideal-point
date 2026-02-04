#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

parse_args <- function(args) {
  out <- list(
    reporter_iso3 = NA_character_,
    year = NA_character_,
    in_root = "data/processed/tariffs/wits_trn",
    out_root = "data/processed/tariffs/wits_trn",
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
  out
}

weighted_mean_safe <- function(x, w) {
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (all(is.na(x))) {
    return(NA_real_)
  }
  if (all(is.na(w))) {
    return(mean(x, na.rm = TRUE))
  }
  stats::weighted.mean(x, w, na.rm = TRUE)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  if (is.na(args$reporter_iso3) || is.na(args$year)) {
    stop("Usage: --reporter-iso3 ISO3 --year YYYY")
  }

  reporter_iso3 <- toupper(args$reporter_iso3)
  year <- as.integer(args$year)

  in_dir <- file.path(args$in_root, reporter_iso3, year)
  out_dir <- file.path(args$out_root, reporter_iso3, year)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(args$validation_root, recursive = TRUE, showWarnings = FALSE)

  in_path <- file.path(in_dir, "line_level_ad_valorem_hs2012.csv")
  if (!file.exists(in_path)) {
    stop("Missing input file: line_level_ad_valorem_hs2012.csv")
  }

  data <- readr::read_csv(in_path, show_col_types = FALSE) %>%
    mutate(
      hs2012 = as.character(hs2012),
      hs4_code = if_else(!is.na(hs2012), str_sub(hs2012, 1, 4), NA_character_),
      mapping_weight = if_else(is.na(mapping_weight), 1, mapping_weight)
    )

  mfn_data <- data %>%
    filter(partner_code == "000") %>%
    group_by(reporter_iso3, year, hs4_code) %>%
    summarise(
      mfn_tariff = weighted_mean_safe(tariff_value, mapping_weight),
      mfn_lines = n(),
      mapping_missing_share = mean(mapping_missing),
      .groups = "drop"
    )

  applied_data <- data %>%
    filter(partner_code != "000") %>%
    group_by(
      reporter_iso3,
      year,
      partner_code,
      partner_iso3,
      partner_name,
      partner_is_group,
      partner_group_type,
      hs4_code
    ) %>%
    summarise(
      applied_tariff = weighted_mean_safe(tariff_value, mapping_weight),
      applied_lines = n(),
      mapping_missing_share = mean(mapping_missing),
      .groups = "drop"
    )

  margins <- applied_data %>%
    left_join(mfn_data, by = c("reporter_iso3", "year", "hs4_code")) %>%
    mutate(
      preferential_margin = mfn_tariff - applied_tariff,
      is_preferential = if_else(!is.na(preferential_margin) & preferential_margin > 0, 1L, 0L),
      negative_margin = if_else(!is.na(preferential_margin) & preferential_margin < 0, 1L, 0L),
      hs_level = "HS4",
      source = "WITS/TRAINS"
    )

  out_full <- file.path(out_dir, "margins_hs4.csv")
  out_mfn <- file.path(out_dir, "mfn_hs4.csv")
  out_summary <- file.path(args$validation_root, sprintf("wits_margin_summary_%s_%s.csv", reporter_iso3, year))

  readr::write_csv(margins, out_full)
  readr::write_csv(mfn_data, out_mfn)

  summary_tbl <- tibble(
    reporter_iso3 = reporter_iso3,
    year = year,
    partners = n_distinct(margins$partner_code),
    partner_groups = n_distinct(margins$partner_code[margins$partner_is_group == "Yes"]),
    hs4_products = n_distinct(margins$hs4_code),
    negative_margin_share = mean(margins$negative_margin == 1, na.rm = TRUE),
    missing_mfn_share = mean(is.na(margins$mfn_tariff)),
    rows = nrow(margins)
  )
  readr::write_csv(summary_tbl, out_summary)

  message("Wrote: ", out_full)
  message("Wrote: ", out_mfn)
  message("Wrote: ", out_summary)
}

main()
