suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  library(dplyr)
})

input_path <- "data/processed/country_year_long.csv"
output_dir <- "outputs/validation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path)) {
  stop("Input file not found: data/processed/country_year_long.csv")
}

x <- read.csv(input_path, stringsAsFactors = FALSE)

# Basic checks
missing_iso3 <- x %>% dplyr::filter(is.na(iso3) | iso3 == "")
missing_year <- x %>% dplyr::filter(is.na(year))
missing_event <- x %>% dplyr::filter(is.na(event_type) | event_type == "")

write.csv(missing_iso3, file = file.path(output_dir, "missing_iso3.csv"), row.names = FALSE)
write.csv(missing_year, file = file.path(output_dir, "missing_year.csv"), row.names = FALSE)
write.csv(missing_event, file = file.path(output_dir, "missing_event_type.csv"), row.names = FALSE)

# Year range checks
invalid_years <- x %>% dplyr::filter(!is.na(year) & (year < 1990 | year > 2025))
write.csv(invalid_years, file = file.path(output_dir, "invalid_years.csv"), row.names = FALSE)

# Duplicates (full row)
duplicates <- x %>%
  dplyr::group_by(source, issue_area, iso3, year, event_type, org_or_treaty_id, status, treaty_type) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1)
write.csv(duplicates, file = file.path(output_dir, "duplicates.csv"), row.names = FALSE)

# Logical date checks: entry into force should not be earlier than signature for same treaty-country
# (only if both events exist for same country/treaty)

sig <- x %>%
  dplyr::filter(event_type == "signature") %>%
  dplyr::group_by(iso3, org_or_treaty_id) %>%
  dplyr::summarise(sig_year = min(year, na.rm = TRUE), .groups = "drop")

eif <- x %>%
  dplyr::filter(event_type == "entry_into_force") %>%
  dplyr::group_by(iso3, org_or_treaty_id) %>%
  dplyr::summarise(eif_year = min(year, na.rm = TRUE), .groups = "drop")

sig_eif <- dplyr::inner_join(sig, eif, by = c("iso3", "org_or_treaty_id"))
invalid_order <- sig_eif %>%
  dplyr::filter(!is.na(sig_year) & !is.na(eif_year) & eif_year < sig_year)

write.csv(invalid_order, file = file.path(output_dir, "invalid_signature_eif_order.csv"), row.names = FALSE)

# Summary report
summary <- data.frame(
  check = c(
    "missing_iso3",
    "missing_year",
    "missing_event_type",
    "invalid_years",
    "duplicates",
    "invalid_signature_eif_order"
  ),
  n_rows = c(
    nrow(missing_iso3),
    nrow(missing_year),
    nrow(missing_event),
    nrow(invalid_years),
    nrow(duplicates),
    nrow(invalid_order)
  ),
  stringsAsFactors = FALSE
)

write.csv(summary, file = file.path(output_dir, "validation_summary.csv"), row.names = FALSE)
