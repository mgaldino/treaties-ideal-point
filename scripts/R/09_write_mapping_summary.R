#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

parse_args <- function(args) {
  out <- list(
    validation_root = "outputs/validation",
    out_path = "docs/tariff_mapping_summary.md"
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

fmt_pct <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", 100 * x))
}

fmt_num <- function(x) {
  ifelse(is.na(x), "NA", format(round(x, 3), nsmall = 3, trim = TRUE, scientific = FALSE))
}

write_table <- function(df, headers) {
  lines <- character(0)
  lines <- c(lines, paste0("| ", paste(headers, collapse = " | "), " |"))
  lines <- c(lines, paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |"))
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    values <- vapply(seq_len(ncol(row)), function(j) as.character(row[[j]]), character(1))
    lines <- c(lines, paste0("| ", paste(values, collapse = " | "), " |"))
  }
  lines
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  dir.create(dirname(args$out_path), recursive = TRUE, showWarnings = FALSE)

  files <- list.files(
    args$validation_root,
    pattern = "^wits_hs_mapping_summary_.*\\.csv$",
    full.names = TRUE
  )
  if (length(files) == 0) {
    stop("No mapping summary files found.")
  }

  summaries <- files %>%
    lapply(readr::read_csv, show_col_types = FALSE) %>%
    bind_rows() %>%
    mutate(
      reporter_iso3 = as.character(reporter_iso3),
      year = as.integer(year),
      lines = as.integer(lines),
      share_mapping_missing = as.numeric(share_mapping_missing),
      share_ambiguous = as.numeric(share_ambiguous),
      max_mappings = as.integer(max_mappings)
    ) %>%
    arrange(reporter_iso3, year)

  overall <- summaries %>%
    summarise(
      total_lines = sum(lines, na.rm = TRUE),
      weighted_missing = stats::weighted.mean(share_mapping_missing, lines, na.rm = TRUE),
      weighted_ambiguous = stats::weighted.mean(share_ambiguous, lines, na.rm = TRUE),
      max_mappings = max(max_mappings, na.rm = TRUE)
    )

  table_summary <- summaries %>%
    mutate(
      missing_share = fmt_pct(share_mapping_missing),
      ambiguous_share = fmt_pct(share_ambiguous)
    ) %>%
    dplyr::select(reporter_iso3, year, lines, missing_share, ambiguous_share, max_mappings)

  sensitivity_path <- file.path(args$validation_root, "wits_hs_mapping_sensitivity_all.csv")
  sensitivity <- NULL
  if (file.exists(sensitivity_path)) {
    sensitivity <- readr::read_csv(sensitivity_path, show_col_types = FALSE) %>%
      mutate(
        reporter_iso3 = as.character(reporter_iso3),
        year = as.integer(year),
        raw_hs4_count = as.integer(raw_hs4_count),
        mapped_hs4_count = as.integer(mapped_hs4_count),
        hs4_coverage_ratio = as.numeric(hs4_coverage_ratio),
        raw_lines = as.integer(raw_lines),
        mapped_lines = as.integer(mapped_lines),
        mapped_line_share = as.numeric(mapped_line_share)
      ) %>%
      arrange(reporter_iso3, year)
  }

  sensitivity_summary <- NULL
  if (!is.null(sensitivity) && nrow(sensitivity) > 0) {
    sensitivity_summary <- sensitivity %>%
      summarise(
        weighted_coverage = stats::weighted.mean(hs4_coverage_ratio, raw_hs4_count, na.rm = TRUE),
        min_coverage = min(hs4_coverage_ratio, na.rm = TRUE),
        max_coverage = max(hs4_coverage_ratio, na.rm = TRUE),
        weighted_line_share = stats::weighted.mean(mapped_line_share, raw_lines, na.rm = TRUE)
      )
  }

  table_sensitivity <- NULL
  if (!is.null(sensitivity) && nrow(sensitivity) > 0) {
    table_sensitivity <- sensitivity %>%
      mutate(
        hs4_coverage_ratio = fmt_num(hs4_coverage_ratio),
        mapped_line_share = fmt_num(mapped_line_share)
      ) %>%
      dplyr::select(
        reporter_iso3,
        year,
        raw_hs4_count,
        mapped_hs4_count,
        hs4_coverage_ratio,
        mapped_line_share
      )
  }

  lines <- character(0)
  lines <- c(lines, "# Tariff Mapping Summary (HS88/92 to HS2012)")
  lines <- c(lines, "")
  lines <- c(
    lines,
    paste0(
      "- Coverage computed from WITS tariff line files mapped to HS2012 using the WITS H4 to H0 concordance."
    )
  )
  lines <- c(
    lines,
    "- Missing mapping: HS88/92 code not found in the concordance.",
    "- Ambiguous mapping: HS88/92 code maps to multiple HS2012 codes (split kept with 1/n weight)."
  )
  lines <- c(lines, "")

  lines <- c(lines, "## Summary")
  lines <- c(
    lines,
    sprintf(
      "- Total lines in scope: %s.",
      format(overall$total_lines, big.mark = ",")
    ),
    sprintf(
      "- Weighted missing share: %s.",
      fmt_pct(overall$weighted_missing)
    ),
    sprintf(
      "- Weighted ambiguous share: %s.",
      fmt_pct(overall$weighted_ambiguous)
    ),
    sprintf(
      "- Maximum HS2012 mappings for a single HS88/92 code: %s.",
      overall$max_mappings
    )
  )

  if (!is.null(sensitivity_summary)) {
    lines <- c(
      lines,
      sprintf(
        "- Weighted HS4 coverage ratio: %s (min %s, max %s).",
        fmt_num(sensitivity_summary$weighted_coverage),
        fmt_num(sensitivity_summary$min_coverage),
        fmt_num(sensitivity_summary$max_coverage)
      ),
      sprintf(
        "- Weighted mapped line share: %s (values > 1 indicate one-to-many splits).",
        fmt_num(sensitivity_summary$weighted_line_share)
      )
    )
  }

  lines <- c(lines, "")
  lines <- c(lines, "Table 1. Mapping shares by reporter-year (ad-valorem lines).")
  lines <- c(lines, write_table(table_summary, c("reporter_iso3", "year", "lines", "missing_share", "ambiguous_share", "max_mappings")))

  if (!is.null(table_sensitivity)) {
    lines <- c(lines, "")
    lines <- c(lines, "Table 2. HS4 coverage by reporter-year.")
    lines <- c(
      lines,
      write_table(
        table_sensitivity,
        c("reporter_iso3", "year", "raw_hs4_count", "mapped_hs4_count", "hs4_coverage_ratio", "mapped_line_share")
      )
    )
  }

  readr::write_lines(lines, args$out_path)
  message("Wrote: ", args$out_path)
}

main()
