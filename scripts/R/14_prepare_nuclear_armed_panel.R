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
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please install it with install.packages('countrycode').")
  }
  library(dplyr)
  library(readr)
  library(tidyr)
  library(jsonlite)
  library(countrycode)
})

dir_raw <- "data/raw/nuclear_weapons"
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
    id = "sipri_yearbook_2025_ch06",
    title = "SIPRI Yearbook 2025, Chapter 6: World nuclear forces",
    org = "Stockholm International Peace Research Institute (SIPRI)",
    url = "https://www.sipri.org/yearbook/2025/06",
    local_path = file.path(dir_sources, "sipri_yearbook_2025_ch06_world_nuclear_forces.pdf"),
    note = "Used to justify coding Israel as nuclear-armed in research practice (SIPRI lists Israel among the nine nuclear-armed states)."
  ),
  list(
    id = "fas_status_world_nuclear_forces",
    title = "Status of World Nuclear Forces",
    org = "Federation of American Scientists (FAS)",
    url = "https://fas.org/initiative/status-world-nuclear-forces/",
    local_path = file.path(dir_sources, "fas_status_world_nuclear_forces.html"),
    note = "Used as the reference list of the 9 nuclear-armed states."
  ),
  list(
    id = "owid_nuclear_tests",
    title = "Number of nuclear weapons tests",
    org = "Our World in Data (OWID)",
    url = "https://ourworldindata.org/grapher/number-of-nuclear-weapons-tests.csv",
    local_path = file.path(dir_sources, "owid_number_of_nuclear_weapons_tests.csv"),
    note = "Used to compute the first year with >=1 nuclear test (proxy onset year for a time-varying indicator)."
  ),
  list(
    id = "iaea_npt",
    title = "Treaty on the Non-Proliferation of Nuclear Weapons (NPT)",
    org = "International Atomic Energy Agency (IAEA)",
    url = "https://www.iaea.org/newscenter/focus/npt",
    local_path = file.path(dir_sources, "iaea_npt.html"),
    note = "Background definition of NPT 'nuclear-weapon States' (the 5)."
  )
)

download_if_missing <- function(url, dest) {
  if (file.exists(dest)) return(invisible(FALSE))
  utils::download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
  invisible(TRUE)
}

for (s in sources) {
  download_if_missing(s$url, s$local_path)
  if (!file.exists(s$local_path)) {
    stop("Missing source file: ", s$local_path)
  }
}

get_source <- function(id) {
  idx <- which(vapply(sources, function(s) identical(s$id, id), logical(1)))
  if (length(idx) == 0) stop("Source not found: ", id)
  sources[[idx[1]]]
}

manifest <- list(
  access_date = access_date,
  generated_by = "scripts/R/14_prepare_nuclear_armed_panel.R",
  sources = lapply(sources, function(s) {
    s$md5 <- unname(tools::md5sum(s$local_path))
    s
  }),
  definition = list(
    nuclear_armed_states = "Nine nuclear-armed states per FAS (USA, Russia, UK, France, China, India, Pakistan, Israel, North Korea).",
    time_varying_onset = "For all except Israel, onset year = first year with >=1 nuclear test in OWID 'number-of-nuclear-weapons-tests' series. Israel is set to 1 for all years 1990-2022 (no public nuclear test series)."
  )
)
jsonlite::write_json(manifest, file.path(dir_raw, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)

# Reference list (ISO3 codes) for nuclear-armed states.
# This is intentionally explicit to avoid brittle HTML parsing.
nuclear_states <- tibble::tibble(
  iso3 = c("USA", "RUS", "GBR", "FRA", "CHN", "IND", "PAK", "ISR", "PRK"),
  nuclear_armed_fas = 1L,
  npt_nws = as.integer(iso3 %in% c("USA", "RUS", "GBR", "FRA", "CHN"))
)

owid_src <- get_source("owid_nuclear_tests")
tests <- readr::read_csv(owid_src$local_path, show_col_types = FALSE) %>%
  dplyr::rename(
    entity = Entity,
    code = Code,
    year = Year,
    tests = `Number of nuclear weapons tests`
  )

# Some entities in OWID use non-ISO codes like OWID_USS (USSR). For our 1990-2022 control,
# we map USSR test history to Russia (RUS).
tests_mapped <- tests %>%
  dplyr::mutate(
    iso3 = dplyr::case_when(
      code %in% c("OWID_USS") ~ "RUS",
      !is.na(code) & grepl("^[A-Z]{3}$", code) ~ code,
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(iso3))

first_test_year <- tests_mapped %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(
    first_test_year = suppressWarnings(min(year[tests > 0], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    first_test_year = dplyr::if_else(is.infinite(first_test_year), NA_integer_, as.integer(first_test_year))
  )

ref <- nuclear_states %>%
  dplyr::left_join(first_test_year, by = "iso3") %>%
  dplyr::mutate(
    # Israel has no public nuclear test series; treat as nuclear-armed throughout our sample window.
    nuclear_israel_assumed = as.integer(iso3 == "ISR"),
    onset_year = dplyr::if_else(iso3 == "ISR", 1990L, first_test_year)
  )

ref_out <- file.path(dir_extracted, "nuclear_armed_states_reference.csv")
readr::write_csv(ref, ref_out)

iso3_universe <- countrycode::codelist %>%
  dplyr::filter(!is.na(iso3c) & nzchar(iso3c)) %>%
  dplyr::distinct(iso3 = iso3c) %>%
  dplyr::arrange(iso3) %>%
  dplyr::pull(iso3)

years <- 1990:2022

panel <- tidyr::crossing(iso3 = iso3_universe, year = years) %>%
  dplyr::left_join(ref, by = "iso3") %>%
  dplyr::mutate(
    nuclear_armed_fas = dplyr::coalesce(nuclear_armed_fas, 0L),
    npt_nws = dplyr::coalesce(npt_nws, 0L),
    nuclear_israel_assumed = dplyr::coalesce(nuclear_israel_assumed, 0L),
    nuclear_armed = dplyr::if_else(nuclear_armed_fas == 1L & !is.na(onset_year) & year >= onset_year, 1L, 0L)
  )

out_path <- file.path(dir_processed, "nuclear_armed_country_year_1990_2022.csv")
readr::write_csv(panel, out_path)

# Validations
if (anyDuplicated(panel %>% dplyr::select(iso3, year)) > 0) stop("Duplicate iso3-year rows detected.")
if (!identical(sort(unique(panel$year)), years)) stop("Year coverage is not exactly 1990-2022.")
bad <- panel %>%
  dplyr::summarise(
    bad_nuclear_armed = any(!(nuclear_armed %in% c(0L, 1L))),
    bad_fas = any(!(nuclear_armed_fas %in% c(0L, 1L))),
    bad_npt = any(!(npt_nws %in% c(0L, 1L)))
  )
if (any(bad)) stop("Non-binary values found in one or more flags.")

counts <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_nuclear_armed = sum(nuclear_armed),
    n_npt_nws = sum(dplyr::if_else(nuclear_armed == 1L, npt_nws, 0L)),
    .groups = "drop"
  )
readr::write_csv(counts, file.path(dir_validation, "nuclear_armed_counts_by_year.csv"))

cat("Nuclear-armed panel written to: ", out_path, "\n")
cat("Reference list written to: ", ref_out, "\n")
cat("Manifest written to: ", file.path(dir_raw, 'manifest.json'), "\n")
