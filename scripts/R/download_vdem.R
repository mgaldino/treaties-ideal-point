
# Script to download and process V-Dem data
# Requirements: vdemdata package, data/processed/country_codebook.csv

suppressPackageStartupMessages({
  library(vdemdata)
  library(dplyr)
  library(readr)
})

# Define paths
codebook_path <- "data/processed/country_codebook.csv"
output_dir <- "data/raw/vdem"
output_file <- file.path(output_dir, "vdem_country_year_v14.rds")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 1. Read country codebook
if (!file.exists(codebook_path)) {
  stop("Country codebook not found at: ", codebook_path)
}
codebook <- read_csv(codebook_path, show_col_types = FALSE)
target_iso3 <- codebook$iso3

cat("Loaded codebook with", length(target_iso3), "countries.
")

# 2. Load V-Dem data
cat("Loading V-Dem data from vdemdata package...
")
if (!exists("vdem")) {
  try(data("vdem", package = "vdemdata"))
}

if (!exists("vdem")) {
  stop("Could not load 'vdem' dataset from vdemdata package.")
}

raw_vdem <- vdem
cat("V-Dem raw dimensions:", nrow(raw_vdem), "rows,", ncol(raw_vdem), "columns.
")

# 3. Filter and Select
# User request: 
# - Years: 1990-2022
# - Columns: country_text_id, year, v2x_libdem
# - Countries: All in the database (implied match with codebook)

processed_vdem <- raw_vdem %>%
  filter(year >= 1990 & year <= 2022) %>%
  filter(country_text_id %in% target_iso3) %>%
  select(country_text_id, year, v2x_libdem)

cat("Processed V-Dem dimensions:", nrow(processed_vdem), "rows.
")

# Check for missing countries
found_countries <- unique(processed_vdem$country_text_id)
missing_countries <- setdiff(target_iso3, found_countries)
if (length(missing_countries) > 0) {
  cat("Warning: The following", length(missing_countries), "countries from codebook were not found in V-Dem (or had no data in range):
")
  print(head(missing_countries, 20))
}

# 4. Save
saveRDS(processed_vdem, output_file)
cat("Saved processed V-Dem data to:", output_file, "
")
