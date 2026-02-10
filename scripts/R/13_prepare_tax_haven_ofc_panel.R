suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Please install it with install.packages('tidyr').")
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required. Please install it with install.packages('pdftools').")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please install it with install.packages('countrycode').")
  }
  library(dplyr)
  library(readr)
  library(tidyr)
  library(pdftools)
  library(jsonlite)
  library(countrycode)
})

dir_raw <- "data/raw/tax_haven_ofc"
dir_sources <- file.path(dir_raw, "sources")
dir_extracted <- file.path(dir_raw, "extracted")
dir_processed <- "data/processed"
dir_validation <- "outputs/validation"

dir.create(dir_sources, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_extracted, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_processed, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_validation, recursive = TRUE, showWarnings = FALSE)

access_date <- as.character(Sys.Date())

sources <- list(
  list(
    id = "nber_w12802",
    title = "Which Countries Become Tax Havens?",
    authors = "Dhammika Dharmapala; James R. Hines Jr.",
    year = 2006,
    url = "https://www.nber.org/system/files/working_papers/w12802/w12802.pdf",
    local_path = file.path(dir_sources, "nber_2006_w12802_which_countries_become_tax_havens.pdf"),
    note = "Table 1 provides a machine-readable list of tax havens (Hines-Rice) and an OECD-based list described in the paper's Data Appendix."
  ),
  list(
    id = "imf_wp0787",
    title = "Revisiting the Definition of Offshore Financial Centers",
    authors = "Ahmed Zoromé",
    year = 2007,
    url = "https://www.imf.org/external/pubs/ft/wp/2007/wp0787.pdf",
    local_path = file.path(dir_sources, "imf_2007_wp0787_ofc.pdf"),
    note = "Table 10 lists OFCs according to IMF and the Financial Stability Forum (FSF). We use FSF-42 jurisdictions (1-42)."
  )
)

download_if_missing <- function(url, dest) {
  if (file.exists(dest)) {
    return(invisible(FALSE))
  }
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  invisible(TRUE)
}

for (s in sources) {
  download_if_missing(s$url, s$local_path)
  if (!file.exists(s$local_path)) {
    stop("Missing source file: ", s$local_path)
  }
}

manifest <- list(
  access_date = access_date,
  generated_by = "scripts/R/13_prepare_tax_haven_ofc_panel.R",
  sources = lapply(sources, function(s) {
    s$md5 <- unname(tools::md5sum(s$local_path))
    s
  })
)
jsonlite::write_json(manifest, file.path(dir_raw, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)

parse_nber_table1 <- function(pdf_path) {
  pages <- pdftools::pdf_text(pdf_path)
  idx <- which(grepl("Table 1: List of Tax Havens", pages, fixed = TRUE))
  if (length(idx) == 0) {
    stop("Could not find Table 1 in NBER PDF: ", pdf_path)
  }
  # Table 1 is fully contained on the matched page in this PDF.
  page <- pages[[idx[1]]]
  lines <- strsplit(page, "\n", fixed = TRUE)[[1]]

  # Keep from the first country row until "Note:"
  start <- which(grepl("^\\s*Andorra\\b", lines))
  if (length(start) == 0) {
    stop("Could not locate first row (Andorra) in NBER Table 1.")
  }
  end <- which(grepl("^\\s*Note:\\s*", lines))
  if (length(end) == 0) {
    stop("Could not locate end of table (Note:) in NBER Table 1.")
  }
  rows <- lines[start[1]:(end[1] - 1)]

  out <- vector("list", length(rows))
  k <- 0L
  for (ln in rows) {
    ln <- trimws(ln)
    if (!nzchar(ln)) next

    # Parse trailing "... <hines> <oecd>"
    if (!grepl("[01]\\s+[01]\\s*$", ln)) next
    flags <- regmatches(ln, gregexpr("[01]", ln))[[1]]
    if (length(flags) < 2) next
    hines <- as.integer(flags[length(flags) - 1])
    oecd <- as.integer(flags[length(flags)])

    core <- sub("\\s*[01]\\s+[01]\\s*$", "", ln)
    # Optional 3-letter code at end
    m <- regexpr("\\b[A-Z]{3}\\b\\s*$", core)
    if (m[1] != -1) {
      code <- trimws(regmatches(core, m))
      name <- trimws(sub("\\b[A-Z]{3}\\b\\s*$", "", core))
    } else {
      code <- NA_character_
      name <- trimws(core)
    }

    k <- k + 1L
    out[[k]] <- data.frame(
      jurisdiction_name = name,
      world_bank_code_raw = code,
      tax_haven_hines_rice_1994 = hines,
      tax_haven_oecd2000_any = oecd,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(out[seq_len(k)])
}

parse_imf_table10 <- function(pdf_path) {
  pages <- pdftools::pdf_text(pdf_path)
  idx <- which(grepl("Table 10. Lists of Offshore Financial Centers", pages, fixed = TRUE))
  if (length(idx) == 0) {
    stop("Could not find Table 10 in IMF PDF: ", pdf_path)
  }
  page <- pages[[idx[1]]]
  lines <- strsplit(page, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  # Rows look like: "1 Andorra", ..., "46 Palau"
  keep <- lines[grepl("^\\d+\\s+", lines)]
  if (length(keep) == 0) {
    stop("Could not parse jurisdiction rows in IMF Table 10.")
  }

  nums <- as.integer(sub("^([0-9]+)\\s+.*$", "\\1", keep))
  names <- sub("^[0-9]+\\s+", "", keep)

  tibble::tibble(
    imf_table10_row = nums,
    jurisdiction_name = names,
    ofc_fsf_2000 = as.integer(nums >= 1 & nums <= 42),
    ofc_imf_2000 = as.integer(nums >= 1 & nums <= 46)
  )
}

map_to_iso3 <- function(jurisdiction_name) {
  # countrycode has decent coverage for territories; add a few common variants.
  custom <- c(
    "Bahamas, The" = "BHS",
    "Hong Kong SAR" = "HKG",
    "Macao SAR" = "MAC",
    "St. Kitts and Nevis" = "KNA",
    "St. Lucia" = "LCA",
    "St. Vincent and the Grenadines" = "VCT",
    "Turks and Caicos Islands" = "TCA",
    "Virgin Islands (U.S.)" = "VIR"
  )

  out <- countrycode::countrycode(
    jurisdiction_name,
    origin = "country.name",
    destination = "iso3c",
    custom_match = custom,
    warn = FALSE
  )

  # Fallback for cases that countrycode does not map by default.
  out[is.na(out) & jurisdiction_name == "British Virgin Islands"] <- "VGB"
  out[is.na(out) & jurisdiction_name == "Channel Islands"] <- NA_character_ # ambiguous (Jersey/Guernsey)
  out[is.na(out) & jurisdiction_name == "Gibraltar"] <- "GIB"
  out[is.na(out) & jurisdiction_name == "Isle of Man"] <- "IMN"
  out[is.na(out) & jurisdiction_name == "Montserrat"] <- "MSR"
  out
}

nber_tbl1 <- parse_nber_table1(sources[[1]]$local_path) %>%
  dplyr::mutate(
    iso3 = map_to_iso3(jurisdiction_name),
    source_id = "nber_w12802",
    extracted_on = access_date
  )

imf_tbl10 <- parse_imf_table10(sources[[2]]$local_path) %>%
  dplyr::mutate(
    iso3 = map_to_iso3(jurisdiction_name),
    source_id = "imf_wp0787",
    extracted_on = access_date
  )

extracted_path <- file.path(dir_extracted, "tax_haven_ofc_extracted_jurisdictions.csv")
extracted <- dplyr::full_join(
  nber_tbl1 %>%
    dplyr::select(source_id, extracted_on, jurisdiction_name, iso3, tax_haven_hines_rice_1994, tax_haven_oecd2000_any),
  imf_tbl10 %>%
    dplyr::select(source_id, extracted_on, jurisdiction_name, iso3, ofc_fsf_2000, ofc_imf_2000),
  by = c("jurisdiction_name", "iso3")
) %>%
  dplyr::arrange(dplyr::coalesce(iso3, ""), jurisdiction_name)

readr::write_csv(extracted, extracted_path)

# Universe for annual panel: all ISO3 codes known to countrycode.
iso3_universe <- countrycode::codelist %>%
  dplyr::filter(!is.na(iso3c) & nzchar(iso3c)) %>%
  dplyr::distinct(iso3 = iso3c) %>%
  dplyr::arrange(iso3) %>%
  dplyr::pull(iso3)

years <- 1990:2022

hines_iso3 <- nber_tbl1 %>%
  dplyr::filter(tax_haven_hines_rice_1994 == 1, !is.na(iso3)) %>%
  dplyr::distinct(iso3)

oecd_iso3 <- nber_tbl1 %>%
  dplyr::filter(tax_haven_oecd2000_any == 1, !is.na(iso3)) %>%
  dplyr::distinct(iso3)

ofc_iso3 <- imf_tbl10 %>%
  dplyr::filter(ofc_fsf_2000 == 1, !is.na(iso3)) %>%
  dplyr::distinct(iso3)

# "Advance commitment" jurisdictions referenced in OECD Harmful Tax Practices work
# (as commonly operationalized in the literature; see NBER w12802 Data Appendix).
oecd_advance6_names <- c("Bermuda", "Cayman Islands", "Cyprus", "Malta", "Mauritius", "San Marino")
oecd_advance6_iso3 <- tibble::tibble(
  jurisdiction_name = oecd_advance6_names,
  iso3 = map_to_iso3(oecd_advance6_names)
) %>%
  dplyr::filter(!is.na(iso3)) %>%
  dplyr::distinct(iso3)

panel <- tidyr::crossing(
  iso3 = iso3_universe,
  year = years
) %>%
  dplyr::left_join(dplyr::mutate(hines_iso3, tax_haven_hines_rice_1994 = 1L), by = "iso3") %>%
  dplyr::left_join(dplyr::mutate(oecd_iso3, tax_haven_oecd2000_any = 1L), by = "iso3") %>%
  dplyr::left_join(dplyr::mutate(ofc_iso3, ofc_fsf_2000 = 1L), by = "iso3") %>%
  dplyr::left_join(dplyr::mutate(oecd_advance6_iso3, tax_haven_oecd2000_advance6 = 1L), by = "iso3") %>%
  dplyr::mutate(
    tax_haven_hines_rice_1994 = dplyr::coalesce(tax_haven_hines_rice_1994, 0L),
    tax_haven_oecd2000_any = dplyr::coalesce(tax_haven_oecd2000_any, 0L),
    ofc_fsf_2000 = dplyr::coalesce(ofc_fsf_2000, 0L),
    tax_haven_oecd2000_advance6 = dplyr::coalesce(tax_haven_oecd2000_advance6, 0L),
    # Time-invariant over 1990-2022 (used as controls; no attempt to infer historical changes).
    tax_haven_hines_rice_1994 = tax_haven_hines_rice_1994,
    tax_haven_oecd2000_any = tax_haven_oecd2000_any,
    ofc_fsf_2000 = ofc_fsf_2000,
    tax_haven_oecd2000_advance6 = tax_haven_oecd2000_advance6,
    tax_haven_oecd2000_strict35 = as.integer(tax_haven_oecd2000_any == 1L & tax_haven_oecd2000_advance6 == 0L),
    tax_haven_or_ofc = as.integer((tax_haven_hines_rice_1994 + tax_haven_oecd2000_any + ofc_fsf_2000) > 0),
    tax_haven_and_ofc = as.integer(((tax_haven_hines_rice_1994 + tax_haven_oecd2000_any) > 0) & (ofc_fsf_2000 == 1))
  )

out_path <- file.path(dir_processed, "tax_haven_ofc_country_year_1990_2022.csv")
readr::write_csv(panel, out_path)

# Basic validations
if (!identical(sort(unique(panel$year)), years)) {
  stop("Year coverage is not exactly 1990-2022.")
}
if (anyDuplicated(panel %>% dplyr::select(iso3, year)) > 0) {
  stop("Duplicate iso3-year rows detected.")
}
flag_cols <- c(
  "tax_haven_hines_rice_1994",
  "tax_haven_oecd2000_any",
  "tax_haven_oecd2000_advance6",
  "tax_haven_oecd2000_strict35",
  "ofc_fsf_2000",
  "tax_haven_or_ofc",
  "tax_haven_and_ofc"
)
bad_flags <- panel %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(flag_cols), ~ any(!(.x %in% c(0L, 1L))))) %>%
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "col", values_to = "has_bad")
if (any(bad_flags$has_bad)) {
  stop("Non-binary values found in flags: ", paste(bad_flags$col[bad_flags$has_bad], collapse = ", "))
}

counts_by_year <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_iso3 = dplyr::n_distinct(iso3),
    n_hines = sum(tax_haven_hines_rice_1994),
    n_oecd = sum(tax_haven_oecd2000_any),
    n_oecd_strict35 = sum(tax_haven_oecd2000_strict35),
    n_oecd_advance6 = sum(tax_haven_oecd2000_advance6),
    n_ofc = sum(ofc_fsf_2000),
    n_any = sum(tax_haven_or_ofc),
    .groups = "drop"
  )
readr::write_csv(counts_by_year, file.path(dir_validation, "tax_haven_ofc_counts_by_year.csv"))

unmapped <- extracted %>%
  dplyr::filter(is.na(iso3)) %>%
  dplyr::select(jurisdiction_name, dplyr::starts_with("source_id")) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("source_id"),
    names_to = "source_col",
    values_to = "source_id"
  ) %>%
  dplyr::filter(!is.na(source_id)) %>%
  dplyr::distinct(jurisdiction_name, source_id)
readr::write_csv(unmapped, file.path(dir_validation, "tax_haven_ofc_unmapped_jurisdictions.csv"))

cat("Tax haven / OFC panel written to: ", out_path, "\n")
cat("Extracted jurisdictions written to: ", extracted_path, "\n")
cat("Manifest written to: ", file.path(dir_raw, "manifest.json"), "\n")
