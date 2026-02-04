suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  library(dplyr)
  library(readr)
})

in_path <- "data/raw/wto_accession/wto_members.csv"
if (!file.exists(in_path)) {
  stop("Missing input file: ", in_path)
}

wto <- readr::read_csv(in_path, show_col_types = FALSE)

expected_cols <- c(
  "country_name",
  "accession_date",
  "accession_date_raw",
  "is_original_member",
  "country_code_iso3",
  "notes"
)
missing_cols <- setdiff(expected_cols, names(wto))
if (length(missing_cols) > 0) {
  stop("Missing columns: ", paste(missing_cols, collapse = ", "))
}

n_rows <- nrow(wto)
if (n_rows < 150 || n_rows > 210) {
  stop("Unexpected row count: ", n_rows)
}

# Date validation
wto <- wto %>%
  dplyr::mutate(
    accession_date_parsed = as.Date(accession_date),
    accession_year = as.integer(format(accession_date_parsed, "%Y"))
  )

missing_dates <- sum(is.na(wto$accession_date_parsed))
if (missing_dates > 10) {
  stop("Too many missing accession dates: ", missing_dates)
}

bad_years <- wto %>%
  dplyr::filter(!is.na(accession_year) & (accession_year < 1948 | accession_year > 2026))
if (nrow(bad_years) > 0) {
  stop("Found accession years outside expected range.")
}

# Duplicate names
dup_names <- wto %>%
  dplyr::filter(!is.na(country_name)) %>%
  dplyr::count(country_name, name = "n") %>%
  dplyr::filter(n > 1)
if (nrow(dup_names) > 0) {
  stop("Duplicate country_name entries detected.")
}

# Basic summary output
summary_df <- tibble::tibble(
  rows = n_rows,
  missing_dates = missing_dates,
  original_members = sum(wto$is_original_member == "TRUE", na.rm = TRUE),
  min_year = min(wto$accession_year, na.rm = TRUE),
  max_year = max(wto$accession_year, na.rm = TRUE)
)

out_dir <- "outputs/validation"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(summary_df, file.path(out_dir, "wto_accession_summary.csv"))

cat("WTO accession validation passed. Summary written to outputs/validation/wto_accession_summary.csv\n")
