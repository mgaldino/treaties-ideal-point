#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

parse_args <- function(args) {
  out <- list(
    reporter_iso3 = NA_character_,
    in_root = "data/processed/tariffs/wits_trn",
    out_root = "data/processed/tariffs/wits_trn"
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

read_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  if (is.na(args$reporter_iso3)) {
    stop("Usage: --reporter-iso3 ISO3")
  }

  reporter_iso3 <- toupper(args$reporter_iso3)
  base_dir <- file.path(args$in_root, reporter_iso3)
  if (!dir.exists(base_dir)) {
    stop("Reporter directory not found: ", base_dir)
  }

  year_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  year_dirs <- year_dirs[grepl("^[0-9]{4}$", basename(year_dirs))]
  if (length(year_dirs) == 0) {
    stop("No year directories found under: ", base_dir)
  }

  margins_list <- list()
  mfn_list <- list()

  normalize_df <- function(df) {
    if ("year" %in% names(df)) {
      df <- df %>%
        mutate(year = as.integer(year))
    }
    if ("hs4_code" %in% names(df)) {
      df <- df %>%
        mutate(hs4_code = str_pad(as.character(hs4_code), 4, pad = "0"))
    }
    if ("partner_code" %in% names(df)) {
      df <- df %>%
        mutate(partner_code = as.character(partner_code))
    }
    numeric_cols <- c(
      "applied_tariff",
      "applied_lines",
      "mapping_missing_share.x",
      "mfn_tariff",
      "mfn_lines",
      "mapping_missing_share.y",
      "preferential_margin",
      "is_preferential",
      "negative_margin",
      "mapping_missing_share"
    )
    for (col in numeric_cols) {
      if (col %in% names(df)) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    df
  }

  for (yd in year_dirs) {
    margins_path <- file.path(yd, "margins_hs4.csv")
    mfn_path <- file.path(yd, "mfn_hs4.csv")
    margins <- read_if_exists(margins_path)
    mfn <- read_if_exists(mfn_path)
    if (!is.null(margins)) {
      margins <- normalize_df(margins)
    }
    if (!is.null(mfn)) {
      mfn <- normalize_df(mfn)
    }
    if (!is.null(margins)) {
      margins_list[[length(margins_list) + 1]] <- margins
    }
    if (!is.null(mfn)) {
      mfn_list[[length(mfn_list) + 1]] <- mfn
    }
  }

  if (length(margins_list) == 0 && length(mfn_list) == 0) {
    stop("No margins or MFN files found to consolidate.")
  }

  out_dir <- file.path(args$out_root, reporter_iso3)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(margins_list) > 0) {
    margins_all <- bind_rows(margins_list) %>%
      mutate(year = as.integer(year)) %>%
      arrange(year, partner_code, hs4_code)

    margins_order <- c(
      "reporter_iso3",
      "year",
      "partner_code",
      "partner_iso3",
      "partner_name",
      "partner_is_group",
      "partner_group_type",
      "hs4_code",
      "applied_tariff",
      "applied_lines",
      "mapping_missing_share.x",
      "mfn_tariff",
      "mfn_lines",
      "mapping_missing_share.y",
      "preferential_margin",
      "is_preferential",
      "negative_margin",
      "hs_level",
      "source"
    )

    margins_all <- margins_all %>%
      dplyr::select(dplyr::all_of(intersect(margins_order, names(margins_all))), dplyr::everything())

    out_margins <- file.path(out_dir, "margins_hs4_timeseries.csv")
    readr::write_csv(margins_all, out_margins)
    message("Wrote: ", out_margins)
  }

  if (length(mfn_list) > 0) {
    mfn_all <- bind_rows(mfn_list) %>%
      mutate(year = as.integer(year)) %>%
      arrange(year, hs4_code)

    mfn_order <- c(
      "reporter_iso3",
      "year",
      "hs4_code",
      "mfn_tariff",
      "mfn_lines",
      "mapping_missing_share"
    )

    mfn_all <- mfn_all %>%
      dplyr::select(dplyr::all_of(intersect(mfn_order, names(mfn_all))), dplyr::everything())

    out_mfn <- file.path(out_dir, "mfn_hs4_timeseries.csv")
    readr::write_csv(mfn_all, out_mfn)
    message("Wrote: ", out_mfn)
  }
}

main()
