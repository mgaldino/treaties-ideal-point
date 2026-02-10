#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

in_path <- "data/raw/vdem/vdem_country_year_v14.rds"
out_path <- "data/processed/vdem_liberal_democracy_period.csv"

if (!file.exists(in_path)) {
  stop("Missing V-Dem RDS: ", in_path)
}

v <- readRDS(in_path)

# Expect country-year with iso3c or country_text_id and v2x_libdem
if (!("iso3c" %in% names(v)) && !("country_text_id" %in% names(v))) {
  stop("V-Dem data missing iso3c or country_text_id")
}
if (!("year" %in% names(v))) {
  stop("V-Dem data missing year")
}
if (!("v2x_libdem" %in% names(v))) {
  stop("V-Dem data missing v2x_libdem")
}

# Map year -> period label
period_label <- function(y) {
  dplyr::case_when(
    y >= 1990 & y <= 1994 ~ "1990-1994",
    y >= 1995 & y <= 1999 ~ "1995-1999",
    y >= 2000 & y <= 2004 ~ "2000-2004",
    y >= 2005 & y <= 2009 ~ "2005-2009",
    y >= 2010 & y <= 2014 ~ "2010-2014",
    y >= 2015 & y <= 2018 ~ "2015-2018",
    TRUE ~ NA_character_
  )
}

if ("iso3c" %in% names(v)) {
  v$iso3c <- as.character(v$iso3c)
} else {
  v$iso3c <- as.character(v$country_text_id)
}

v_period <- v %>%
  dplyr::mutate(
    period = period_label(year)
  ) %>%
  dplyr::filter(!is.na(period), !is.na(iso3c)) %>%
  dplyr::group_by(iso3c, period) %>%
  dplyr::summarise(
    vdem_libdem = mean(v2x_libdem, na.rm = TRUE),
    n_years = sum(!is.na(v2x_libdem)),
    .groups = "drop"
  )

write_csv(v_period, out_path)
cat("Saved: ", out_path, "\n", sep = "")
