## ============================================================
## Aggregate WITS tariff data for trade dimension
## ============================================================
## Input:  data/processed/tariffs/wits_trn/{ISO3}/mfn_hs4_timeseries.csv
##         data/processed/tariffs/wits_trn/{ISO3}/margins_hs4_timeseries.csv
## Output: data/processed/trade_panel.csv
##         (country-period panel with MFN mean and preferential margin mean)
## ============================================================

library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

# === 1. HS4 → HS Section mapping (21 sections) ===
# Based on WCO HS2012 section structure
hs2_to_section <- tibble::tribble(
  ~hs2_start, ~hs2_end, ~section, ~section_label,
  1,  5,  "I",     "Live animals",
  6,  14, "II",    "Vegetable products",
  15, 15, "III",   "Fats and oils",
  16, 24, "IV",    "Food, beverages, tobacco",
  25, 27, "V",     "Mineral products",
  28, 38, "VI",    "Chemicals",
  39, 40, "VII",   "Plastics, rubber",
  41, 43, "VIII",  "Leather, furs",
  44, 46, "IX",    "Wood",
  47, 49, "X",     "Paper",
  50, 63, "XI",    "Textiles",
  64, 67, "XII",   "Footwear",
  68, 70, "XIII",  "Stone, cement, ceramics",
  71, 71, "XIV",   "Precious metals",
  72, 83, "XV",    "Base metals",
  84, 85, "XVI",   "Machinery, electronics",
  86, 89, "XVII",  "Transport",
  90, 92, "XVIII", "Instruments",
  93, 93, "XIX",   "Arms, ammunition",
  94, 96, "XX",    "Misc manufactured",
  97, 97, "XXI",   "Art, antiques"
)

map_hs4_to_section <- function(hs4_code) {
  hs2 <- as.integer(substr(as.character(hs4_code), 1, 2))
  section <- rep(NA_character_, length(hs2))
  for (i in seq_len(nrow(hs2_to_section))) {
    idx <- hs2 >= hs2_to_section$hs2_start[i] & hs2 <= hs2_to_section$hs2_end[i]
    idx[is.na(idx)] <- FALSE
    section[idx] <- hs2_to_section$section[i]
  }
  section
}

# === 2. Period mapping ===
assign_period <- function(year) {
  dplyr::case_when(
    year >= 1990 & year <= 1994 ~ 1L,
    year >= 1995 & year <= 1999 ~ 2L,
    year >= 2000 & year <= 2004 ~ 3L,
    year >= 2005 & year <= 2009 ~ 4L,
    year >= 2010 & year <= 2014 ~ 5L,
    year >= 2015 & year <= 2018 ~ 6L,
    year >= 2019 & year <= 2022 ~ 7L,
    TRUE ~ NA_integer_
  )
}

period_labels <- c("1990-1994", "1995-1999", "2000-2004",
                   "2005-2009", "2010-2014", "2015-2018", "2019-2022")

# === 3. Discover available countries ===
tariff_dir <- here("data", "processed", "tariffs", "wits_trn")
country_dirs <- list.dirs(tariff_dir, recursive = FALSE, full.names = TRUE)

cat(sprintf("Found %d country directories\n", length(country_dirs)))

# Filter to those with timeseries files
has_mfn <- sapply(country_dirs, function(d)
  file.exists(file.path(d, "mfn_hs4_timeseries.csv")))
has_margins <- sapply(country_dirs, function(d)
  file.exists(file.path(d, "margins_hs4_timeseries.csv")))

country_dirs <- country_dirs[has_mfn & has_margins]
cat(sprintf("Countries with timeseries: %d\n", length(country_dirs)))

# === 4. Read and aggregate MFN data ===
cat("\n=== Reading MFN timeseries ===\n")

read_mfn <- function(dir) {
  iso3 <- basename(dir)
  f <- file.path(dir, "mfn_hs4_timeseries.csv")
  tryCatch({
    df <- read_csv(f, show_col_types = FALSE) %>%
      filter(!is.na(mfn_tariff), !is.na(hs4_code)) %>%
      mutate(
        section = map_hs4_to_section(hs4_code),
        period = assign_period(year)
      ) %>%
      filter(!is.na(section), !is.na(period))
    if (nrow(df) == 0) return(NULL)
    df %>% select(reporter_iso3, year, period, hs4_code, section, mfn_tariff)
  }, error = function(e) {
    cat(sprintf("  Error reading MFN for %s: %s\n", iso3, e$message))
    NULL
  })
}

mfn_all <- map_dfr(country_dirs, read_mfn)
cat(sprintf("MFN data: %d rows, %d countries\n",
            nrow(mfn_all), n_distinct(mfn_all$reporter_iso3)))

# === 5. Read and aggregate margins data ===
cat("\n=== Reading margins timeseries ===\n")

read_margins <- function(dir) {
  iso3 <- basename(dir)
  f <- file.path(dir, "margins_hs4_timeseries.csv")
  tryCatch({
    df <- read_csv(f, show_col_types = FALSE) %>%
      filter(!is.na(preferential_margin), !is.na(hs4_code)) %>%
      mutate(
        section = map_hs4_to_section(hs4_code),
        period = assign_period(year)
      ) %>%
      filter(!is.na(section), !is.na(period))
    if (nrow(df) == 0) return(NULL)
    df %>% select(reporter_iso3, year, period, hs4_code, section,
                  partner_iso3, applied_tariff, mfn_tariff,
                  preferential_margin, is_preferential)
  }, error = function(e) {
    cat(sprintf("  Error reading margins for %s: %s\n", iso3, e$message))
    NULL
  })
}

margins_all <- map_dfr(country_dirs, read_margins)
cat(sprintf("Margins data: %d rows, %d countries\n",
            nrow(margins_all), n_distinct(margins_all$reporter_iso3)))

# === 6. Compute best preferential margin per country-year-HS4 ===
# For each product, take the max margin across all partners
# (= deepest preference = lowest applied tariff relative to MFN)
cat("\n=== Computing best preferential margins ===\n")

best_margin <- margins_all %>%
  group_by(reporter_iso3, year, period, hs4_code, section) %>%
  summarise(
    best_pref_margin = max(preferential_margin, na.rm = TRUE),
    n_partners = n_distinct(partner_iso3),
    any_preferential = any(is_preferential == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Replace -Inf (from max of empty) with 0
  mutate(best_pref_margin = if_else(is.finite(best_pref_margin),
                                     best_pref_margin, 0))

cat(sprintf("Best margins: %d rows\n", nrow(best_margin)))

# === 7. Aggregate to section × period level ===
cat("\n=== Aggregating to section-period ===\n")

# 7a. MFN: mean tariff by section-period
mfn_section_period <- mfn_all %>%
  group_by(reporter_iso3, period, section) %>%
  summarise(
    mfn_mean = mean(mfn_tariff, na.rm = TRUE),
    n_hs4 = n_distinct(hs4_code),
    n_years = n_distinct(year),
    .groups = "drop"
  )

# 7b. Margins: mean best margin by section-period
margin_section_period <- best_margin %>%
  group_by(reporter_iso3, period, section) %>%
  summarise(
    margin_mean = mean(best_pref_margin, na.rm = TRUE),
    n_hs4 = n_distinct(hs4_code),
    n_years = n_distinct(year),
    n_partners_mean = mean(n_partners, na.rm = TRUE),
    .groups = "drop"
  )

# === 8. Aggregate to country-period level ===
cat("\n=== Aggregating to country-period ===\n")

# 8a. MFN: mean across sections (simple average, equal weight per section)
mfn_panel <- mfn_section_period %>%
  group_by(reporter_iso3, period) %>%
  summarise(
    mfn_mean = mean(mfn_mean, na.rm = TRUE),
    n_sections = n(),
    .groups = "drop"
  )

# 8b. Margins: mean across sections
margin_panel <- margin_section_period %>%
  group_by(reporter_iso3, period) %>%
  summarise(
    margin_mean = mean(margin_mean, na.rm = TRUE),
    n_sections = n(),
    .groups = "drop"
  )

# === 9. Merge into final panel ===
trade_panel <- mfn_panel %>%
  full_join(margin_panel, by = c("reporter_iso3", "period"),
            suffix = c("_mfn", "_margin")) %>%
  mutate(period_label = period_labels[period]) %>%
  arrange(reporter_iso3, period)

cat(sprintf("\nFinal panel: %d rows, %d countries, %d periods\n",
            nrow(trade_panel),
            n_distinct(trade_panel$reporter_iso3),
            n_distinct(trade_panel$period)))

# === 10. Summary statistics ===
cat("\n=== Summary by period ===\n")
summary_by_period <- trade_panel %>%
  group_by(period, period_label) %>%
  summarise(
    n_countries = n(),
    mfn_mean = mean(mfn_mean, na.rm = TRUE),
    mfn_sd = sd(mfn_mean, na.rm = TRUE),
    margin_mean = mean(margin_mean, na.rm = TRUE),
    margin_sd = sd(margin_mean, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(summary_by_period))

# Country type classification (for descriptive purposes)
cat("\n=== Country types (2015-2018) ===\n")
latest <- trade_panel %>% filter(period == 6)
if (nrow(latest) > 0) {
  mfn_median <- median(latest$mfn_mean, na.rm = TRUE)
  margin_median <- median(latest$margin_mean, na.rm = TRUE)
  latest <- latest %>%
    mutate(type = case_when(
      mfn_mean <= mfn_median & margin_mean <= margin_median ~ "Universally open",
      mfn_mean > mfn_median & margin_mean > margin_median ~ "Club",
      mfn_mean > mfn_median & margin_mean <= margin_median ~ "Closed",
      TRUE ~ "Low MFN + high margin"
    ))
  cat(sprintf("MFN median: %.1f%%, Margin median: %.1f%%\n",
              mfn_median, margin_median))
  print(table(latest$type))
}

# === 11. Save ===
out_file <- here("data", "processed", "trade_panel.csv")
write_csv(trade_panel, out_file)
cat(sprintf("\nSaved: %s\n", out_file))

# Also save section-period detail for potential IRT use later
out_detail <- here("data", "processed", "trade_section_period.csv")
detail <- mfn_section_period %>%
  full_join(margin_section_period,
            by = c("reporter_iso3", "period", "section"),
            suffix = c("_mfn", "_margin"))
write_csv(detail, out_detail)
cat(sprintf("Saved detail: %s\n", out_detail))

# === 12. Year-level panel (no period aggregation) ===
cat("\n=== Building year-level panel ===\n")

# MFN by country-year (mean across sections)
mfn_section_year <- mfn_all %>%
  group_by(reporter_iso3, year, section) %>%
  summarise(mfn_mean = mean(mfn_tariff, na.rm = TRUE),
            n_hs4 = n_distinct(hs4_code), .groups = "drop")

mfn_yearly <- mfn_section_year %>%
  group_by(reporter_iso3, year) %>%
  summarise(mfn_mean = mean(mfn_mean, na.rm = TRUE),
            n_sections = n(), .groups = "drop")

# Margins by country-year
best_margin_year <- margins_all %>%
  group_by(reporter_iso3, year, hs4_code, section) %>%
  summarise(best_pref_margin = max(preferential_margin, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(best_pref_margin = if_else(is.finite(best_pref_margin),
                                     best_pref_margin, 0))

margin_section_year <- best_margin_year %>%
  group_by(reporter_iso3, year, section) %>%
  summarise(margin_mean = mean(best_pref_margin, na.rm = TRUE),
            n_hs4 = n_distinct(hs4_code), .groups = "drop")

margin_yearly <- margin_section_year %>%
  group_by(reporter_iso3, year) %>%
  summarise(margin_mean = mean(margin_mean, na.rm = TRUE),
            n_sections = n(), .groups = "drop")

trade_yearly <- mfn_yearly %>%
  full_join(margin_yearly, by = c("reporter_iso3", "year"),
            suffix = c("_mfn", "_margin")) %>%
  arrange(reporter_iso3, year)

cat(sprintf("Year-level panel: %d rows, %d countries, %d years\n",
            nrow(trade_yearly),
            n_distinct(trade_yearly$reporter_iso3),
            n_distinct(trade_yearly$year)))

out_yearly <- here("data", "processed", "trade_panel_yearly.csv")
write_csv(trade_yearly, out_yearly)
cat(sprintf("Saved: %s\n", out_yearly))

cat("\n=== DONE ===\n")
