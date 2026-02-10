suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please install it with install.packages('countrycode').")
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' is required. Please install it with install.packages('haven').")
  }
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(countrycode)
  library(haven)
})

manifest_path <- "data/raw/unga_ideal_points/manifest.json"
if (!file.exists(manifest_path)) {
  stop("Missing manifest: ", manifest_path)
}

manifest <- jsonlite::fromJSON(manifest_path)
file_path <- manifest$file_path
if (!file.exists(file_path)) {
  stop("Missing UNGA file: ", file_path)
}

# Prefer Stata reader as requested; keep a fallback for robustness.
unga <- tryCatch(
  haven::read_dta(file_path),
  error = function(e) NULL
)
if (is.null(unga)) {
  unga <- readr::read_tsv(file_path, show_col_types = FALSE)
}

# Identify columns (case-insensitive, best-effort).
names_lower <- tolower(names(unga))
get_col <- function(candidates) {
  idx <- which(names_lower %in% candidates)
  if (length(idx) > 0) {
    return(names(unga)[idx[1]])
  }
  NA_character_
}

year_col <- get_col(c("year", "yr"))
if (is.na(year_col)) {
  stop("Year column not found in UNGA data.")
}

iso_col <- get_col(c("iso3c", "iso3", "iso"))
ccode_col <- get_col(c("ccode", "cowcode", "cown"))
country_col <- get_col(c("countryname", "country", "state"))

ideal_col <- get_col(c("idealpointall", "idealpoint", "idealpointfp", "idealpointlegacy"))
if (is.na(ideal_col)) {
  ideal_candidates <- names(unga)[grepl("ideal", names_lower)]
  if (length(ideal_candidates) == 0) {
    stop("Ideal point column not found in UNGA data.")
  }
  ideal_col <- ideal_candidates[1]
}

unga <- unga %>%
  dplyr::mutate(
    year = suppressWarnings(as.integer(.data[[year_col]])),
    ideal = suppressWarnings(as.numeric(.data[[ideal_col]]))
  )

# Output must use column name iso3 (not iso3c).
if (!is.na(iso_col)) {
  unga$iso3 <- as.character(unga[[iso_col]])
} else if (!is.na(ccode_col)) {
  unga$iso3 <- countrycode::countrycode(unga[[ccode_col]], "cown", "iso3c", warn = FALSE)
} else if (!is.na(country_col)) {
  unga$iso3 <- countrycode::countrycode(unga[[country_col]], "country.name", "iso3c", warn = FALSE)
} else {
  stop("No usable country identifier found in UNGA data.")
}

unga$iso3 <- toupper(trimws(as.character(unga$iso3)))

period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)
period_labels <- c(
  "1990-1994",
  "1995-1999",
  "2000-2004",
  "2005-2009",
  "2010-2014",
  "2015-2019",
  "2020-2024"
)

assign_period <- function(year) {
  cut(
    year,
    breaks = period_breaks,
    labels = period_labels,
    right = FALSE,
    include.lowest = TRUE
  )
}

unga <- unga %>%
  dplyr::filter(!is.na(year) & year >= 1990 & year <= 2024) %>%
  dplyr::filter(!is.na(iso3) & !is.na(ideal)) %>%
  dplyr::mutate(period = as.character(assign_period(year))) %>%
  dplyr::filter(!is.na(period))

unga_period <- unga %>%
  dplyr::group_by(iso3, period) %>%
  dplyr::summarise(unga_ideal_point = mean(ideal, na.rm = TRUE), .groups = "drop") %>%
  dplyr::select(iso3, period, unga_ideal_point)

out_path <- "data/processed/unga_ideal_points_period_v8.csv"
readr::write_csv(unga_period, out_path)

cat("UNGA preparation (V8) complete. Output: ", out_path, "\n", sep = "")

