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
    concordance_path = "data/raw/concordance/wits/extracted/JobID-77_Concordance_H4_to_H0.CSV",
    in_root = "data/processed/tariffs/wits_trn",
    out_root = "data/processed/tariffs/wits_trn",
    concordance_out = "data/processed/concordance/wits_hs88_to_hs2012.csv",
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

standardize_code <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9]", "")
  x <- str_pad(x, 6, pad = "0")
  x
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
  dir.create(dirname(args$concordance_out), recursive = TRUE, showWarnings = FALSE)

  in_path <- file.path(in_dir, "line_level_ad_valorem.csv")
  if (!file.exists(in_path)) {
    stop("Missing input file: line_level_ad_valorem.csv")
  }

  concordance <- readr::read_csv(args$concordance_path, show_col_types = FALSE)
  names(concordance) <- names(concordance) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+$", "")

  concordance <- concordance %>%
    dplyr::select(hs_2012_product_code, hs_1988_92_product_code) %>%
    mutate(
      hs2012 = standardize_code(hs_2012_product_code),
      hs88 = standardize_code(hs_1988_92_product_code)
    ) %>%
    dplyr::select(hs88, hs2012) %>%
    distinct()

  hs88_counts <- concordance %>%
    count(hs88, name = "hs2012_count")

  hs2012_counts <- concordance %>%
    count(hs2012, name = "hs88_count")

  concordance <- concordance %>%
    left_join(hs88_counts, by = "hs88") %>%
    left_join(hs2012_counts, by = "hs2012") %>%
    mutate(
      mapping_type = case_when(
        hs2012_count == 1 & hs88_count == 1 ~ "one_to_one",
        hs2012_count > 1 & hs88_count == 1 ~ "one_to_many",
        hs2012_count == 1 & hs88_count > 1 ~ "many_to_one",
        hs2012_count > 1 & hs88_count > 1 ~ "many_to_many",
        TRUE ~ "unknown"
      ),
      ambiguous_mapping = hs2012_count > 1,
      mapping_weight = if_else(hs2012_count > 0, 1 / hs2012_count, NA_real_)
    )

  readr::write_csv(concordance, args$concordance_out)

  data <- readr::read_csv(in_path, show_col_types = FALSE) %>%
    mutate(
      hs88 = standardize_code(hs6_code)
    )

  data_mapped <- data %>%
    left_join(concordance, by = "hs88", relationship = "many-to-many") %>%
    mutate(
      hs2012 = as.character(hs2012),
      mapping_missing = is.na(hs2012),
      mapping_weight = if_else(is.na(mapping_weight), 1, mapping_weight)
    )

  original_lines <- data %>%
    mutate(line_id = paste(reporter_code, partner_code, year, hs88, tariff_type, sep = "|"))

  mapping_summary <- original_lines %>%
    left_join(hs88_counts, by = "hs88") %>%
    mutate(
      hs2012_count = if_else(is.na(hs2012_count), 0L, hs2012_count),
      mapping_missing = hs2012_count == 0,
      ambiguous_mapping = hs2012_count > 1
    ) %>%
    summarise(
      reporter_iso3 = dplyr::first(reporter_iso3),
      year = dplyr::first(year),
      lines = n(),
      unique_hs88 = n_distinct(hs88),
      share_mapping_missing = mean(mapping_missing),
      share_ambiguous = mean(ambiguous_mapping),
      max_mappings = max(hs2012_count)
    )

  out_mapped <- file.path(out_dir, "line_level_ad_valorem_hs2012.csv")
  out_summary <- file.path(args$validation_root, sprintf("wits_hs_mapping_summary_%s_%s.csv", reporter_iso3, year))

  readr::write_csv(data_mapped, out_mapped)
  readr::write_csv(mapping_summary, out_summary)

  message("Wrote: ", out_mapped)
  message("Wrote: ", out_summary)
}

main()
