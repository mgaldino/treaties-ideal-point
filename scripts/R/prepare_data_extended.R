suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Please install it with install.packages('readxl').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please install it with install.packages('countrycode').")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Please install it with install.packages('tibble').")
  }
  library(dplyr)
  library(readr)
  library(readxl)
  library(countrycode)
  library(tibble)
})

# Ensure UTF-8 locale for reading IEADB files with accented characters.
invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE))

output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

logs_dir <- "logs"
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990
year_max <- 2024

period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)  # right boundary exclusive
period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024")

assign_period <- function(year) {
  cut(year, breaks = period_breaks, labels = period_labels, right = FALSE, include.lowest = TRUE)
}

period_index <- function(label) {
  match(label, period_labels) - 1L
}

parse_date_vec <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  x_char <- trimws(as.character(x))
  x_char[x_char == ""] <- NA
  suppressWarnings(as.Date(x_char, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
}

year_from_date <- function(x) {
  x_date <- parse_date_vec(x)
  out <- rep(NA_integer_, length(x_date))
  idx <- !is.na(x_date)
  out[idx] <- as.integer(format(x_date[idx], "%Y"))
  out
}

min_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_integer_)
  }
  suppressWarnings(min(x, na.rm = TRUE))
}

safe_iso3_from_name <- function(names) {
  countrycode::countrycode(names, "country.name", "iso3c", warn = FALSE)
}

standardize_event <- function(df) {
  cols <- c(
    "source",
    "issue_area",
    "iso3",
    "country_name",
    "event_year",
    "event_date",
    "event_type",
    "item_id",
    "item_name",
    "status",
    "treaty_type",
    "source_url"
  )
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df$iso3 <- as.character(df$iso3)
  df$country_name <- as.character(df$country_name)
  df$event_year <- suppressWarnings(as.integer(df$event_year))
  df$event_date <- as.character(df$event_date)
  df$event_type <- as.character(df$event_type)
  df$item_id <- as.character(df$item_id)
  df$item_name <- as.character(df$item_name)
  df$status <- as.character(df$status)
  df$treaty_type <- as.character(df$treaty_type)
  df$source <- as.character(df$source)
  df$issue_area <- as.character(df$issue_area)
  df$source_url <- as.character(df$source_url)
  dplyr::select(df, dplyr::all_of(cols))
}

standardize_item <- function(df) {
  cols <- c(
    "issue_area",
    "item_id",
    "source",
    "item_name",
    "treaty_type",
    "treaty_open_year",
    "source_url"
  )
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df$issue_area <- as.character(df$issue_area)
  df$item_id <- as.character(df$item_id)
  df$source <- as.character(df$source)
  df$item_name <- as.character(df$item_name)
  df$treaty_type <- as.character(df$treaty_type)
  df$treaty_open_year <- suppressWarnings(as.integer(df$treaty_open_year))
  df$source_url <- as.character(df$source_url)
  dplyr::select(df, dplyr::all_of(cols))
}

# --- UNCTAD IIA (investment) ---
build_unctad_iia <- function() {
  path <- "data/raw/unctad_iia/unctad_iia_country_treaties.csv"
  if (!file.exists(path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble(), excluded = tibble::tibble()))
  }

  raw <- readr::read_csv(path, show_col_types = FALSE)
  raw <- dplyr::select(
    raw,
    country_code,
    country_name,
    date_of_signature,
    date_of_entry_into_force,
    status,
    type,
    treaty_url,
    full_title
  )

  raw$iso3 <- countrycode::countrycode(raw$country_code, "iso2c", "iso3c", warn = FALSE)
  excluded <- raw %>%
    dplyr::filter(is.na(iso3)) %>%
    dplyr::distinct(country_code, country_name)

  raw <- raw %>% dplyr::filter(!is.na(iso3))

  raw$signature_year <- year_from_date(raw$date_of_signature)
  raw$eif_year <- year_from_date(raw$date_of_entry_into_force)

  events <- raw %>%
    dplyr::filter(!is.na(eif_year) & eif_year >= year_min & eif_year <= year_max) %>%
    dplyr::mutate(
      source = "UNCTAD IIA",
      issue_area = "investment",
      event_year = eif_year,
      event_date = as.character(parse_date_vec(date_of_entry_into_force)),
      event_type = "entry_into_force",
      item_id = paste0("unctad_", treaty_url),
      item_name = full_title,
      treaty_type = type,
      source_url = treaty_url
    )

  items <- raw %>%
    dplyr::group_by(treaty_url) %>%
    dplyr::summarise(
      item_id = paste0("unctad_", first(treaty_url)),
      item_name = first(full_title),
      treaty_type = first(type),
      treaty_open_year = min_or_na(signature_year),
      source_url = first(treaty_url),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source = "UNCTAD IIA",
      issue_area = "investment"
    )

  list(events = standardize_event(events), items = standardize_item(items), excluded = excluded)
}

# --- IEADB (environment) ---
build_ieadb <- function() {
  members_path <- "data/processed/ieadb/db_members.csv"
  treaties_path <- "data/processed/ieadb/db_treaties.csv"
  if (!file.exists(members_path) || !file.exists(treaties_path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  members <- read.delim(
    members_path,
    sep = ";",
    skip = 3,
    quote = '"',
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  members <- dplyr::select(
    members,
    `Mitch ID`,
    `Treaty Name from Treaty Table`,
    `Country (official name)`,
    `Action Type`,
    Year,
    `Agreement Type - Level 1`
  )

  members$Year <- suppressWarnings(as.integer(members$Year))
  members$iso3 <- safe_iso3_from_name(members$`Country (official name)`)

  members <- members %>%
    dplyr::filter(!is.na(iso3) & !is.na(Year)) %>%
    dplyr::filter(Year >= year_min & Year <= year_max)

  action_lower <- tolower(members$`Action Type`)
  is_proxy <- grepl("ratification", action_lower) |
    grepl("accession", action_lower) |
    grepl("acceptance", action_lower) |
    grepl("approval", action_lower)

  proxy <- members %>% dplyr::filter(is_proxy)

  events <- proxy %>%
    dplyr::group_by(iso3, `Mitch ID`) %>%
    dplyr::summarise(
      event_year = min(Year, na.rm = TRUE),
      item_name = first(`Treaty Name from Treaty Table`),
      treaty_type = first(`Agreement Type - Level 1`),
      country_name = first(`Country (official name)`),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source = "IEADB",
      issue_area = "environment",
      event_date = NA,
      event_type = "entry_into_force",
      item_id = paste0("ieadb_", `Mitch ID`),
      status = "ratification_accession_proxy",
      source_url = NA
    )

  treaties <- read.delim(
    treaties_path,
    sep = ";",
    skip = 3,
    quote = '"',
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  treaties <- dplyr::select(
    treaties,
    `IEA#`,
    `Agreement Name`,
    `Date IEA was concluded`,
    `Date IEA entered into force`,
    `Agreement Type Level 3`,
    `URL to text of IEA`
  )

  treaties$open_year <- year_from_date(treaties$`Date IEA was concluded`)
  eif_year <- year_from_date(treaties$`Date IEA entered into force`)
  treaties$open_year[is.na(treaties$open_year)] <- eif_year[is.na(treaties$open_year)]

  type_lookup <- events %>%
    dplyr::group_by(item_id) %>%
    dplyr::summarise(treaty_type = dplyr::first(treaty_type), .groups = "drop")

  items <- treaties %>%
    dplyr::mutate(
      source = "IEADB",
      issue_area = "environment",
      item_id = paste0("ieadb_", `IEA#`),
      item_name = `Agreement Name`,
      treaty_open_year = open_year,
      source_url = `URL to text of IEA`
    ) %>%
    dplyr::left_join(type_lookup, by = "item_id") %>%
    dplyr::mutate(
      treaty_type = dplyr::coalesce(treaty_type, `Agreement Type Level 3`)
    ) %>%
    dplyr::select(source, issue_area, item_id, item_name, treaty_type, treaty_open_year, source_url)

  list(events = standardize_event(events), items = standardize_item(items))
}

# --- UN Human Rights Treaties (human_rights) ---
build_un_hr_treaties <- function() {
  path <- "data/raw/un_hr_treaties/un_hr_ratifications.csv"
  if (!file.exists(path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  raw <- readr::read_csv(path, show_col_types = FALSE)

  raw$iso3 <- safe_iso3_from_name(raw$country_name)
  raw <- raw %>% dplyr::filter(!is.na(iso3))

  raw$event_year <- year_from_date(raw$action_date)
  raw$sig_year <- year_from_date(raw$signature_date)

  events <- raw %>%
    dplyr::filter(!is.na(event_year) & event_year >= year_min & event_year <= year_max) %>%
    dplyr::mutate(
      source = "UN HR Treaties",
      issue_area = "human_rights",
      event_date = as.character(action_date),
      event_type = tolower(action_type),
      item_id = paste0("unhr_", treaty_id),
      item_name = treaty_name,
      status = action_type,
      treaty_type = "human_rights_treaty",
      source_url = source_url,
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE)
    )

  items <- raw %>%
    dplyr::group_by(treaty_id) %>%
    dplyr::summarise(
      item_name = dplyr::first(treaty_name),
      treaty_open_year = dplyr::first(as.integer(treaty_open_year)),
      source_url = dplyr::first(source_url),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source = "UN HR Treaties",
      issue_area = "human_rights",
      item_id = paste0("unhr_", treaty_id),
      treaty_type = "human_rights_treaty"
    )

  list(events = standardize_event(events), items = standardize_item(items))
}

# --- Arms Control Treaties (arms_control) ---
build_arms_control <- function() {
  path <- "data/raw/arms_control/arms_control_ratifications.csv"
  if (!file.exists(path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  raw <- readr::read_csv(path, show_col_types = FALSE)

  raw$iso3 <- safe_iso3_from_name(raw$country_name)
  raw <- raw %>% dplyr::filter(!is.na(iso3))

  raw$event_year <- year_from_date(raw$action_date)

  events <- raw %>%
    dplyr::filter(!is.na(event_year) & event_year >= year_min & event_year <= year_max) %>%
    dplyr::mutate(
      source = "Arms Control (UN Ch. XXVI)",
      issue_area = "arms_control",
      event_date = as.character(action_date),
      event_type = tolower(action_type),
      item_id = paste0("arms_", treaty_id),
      item_name = treaty_name,
      status = action_type,
      treaty_type = "arms_control_treaty",
      source_url = source_url,
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE)
    )

  items <- raw %>%
    dplyr::group_by(treaty_id) %>%
    dplyr::summarise(
      item_name = dplyr::first(treaty_name),
      treaty_open_year = dplyr::first(as.integer(treaty_open_year)),
      source_url = dplyr::first(source_url),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source = "Arms Control (UN Ch. XXVI)",
      issue_area = "arms_control",
      item_id = paste0("arms_", treaty_id),
      treaty_type = "arms_control_treaty"
    )

  list(events = standardize_event(events), items = standardize_item(items))
}

# --- WIPO Treaties (intellectual_property) ---
build_wipo_treaties <- function() {
  path <- "data/raw/wipo_treaties/wipo_ratifications.csv"
  if (!file.exists(path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  raw <- readr::read_csv(path, show_col_types = FALSE)

  raw$iso3 <- safe_iso3_from_name(raw$country_name)
  raw <- raw %>% dplyr::filter(!is.na(iso3))

  raw$event_year <- year_from_date(raw$action_date)
  eif_year <- year_from_date(raw$entry_into_force_date)
  raw$event_year[is.na(raw$event_year)] <- eif_year[is.na(raw$event_year)]

  raw$best_date <- as.character(raw$action_date)
  missing <- is.na(raw$best_date) | raw$best_date == ""
  raw$best_date[missing] <- as.character(raw$entry_into_force_date)[missing]

  events <- raw %>%
    dplyr::filter(!is.na(event_year) & event_year >= year_min & event_year <= year_max) %>%
    dplyr::mutate(
      source = "WIPO Treaties",
      issue_area = "intellectual_property",
      event_date = as.character(best_date),
      event_type = tolower(action_type),
      item_id = paste0("wipo_", treaty_id),
      item_name = treaty_name,
      status = action_type,
      treaty_type = "ip_treaty",
      source_url = source_url,
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE)
    )

  items <- raw %>%
    dplyr::group_by(treaty_id) %>%
    dplyr::summarise(
      item_name = dplyr::first(treaty_name),
      treaty_open_year = dplyr::first(as.integer(treaty_open_year)),
      source_url = dplyr::first(source_url),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source = "WIPO Treaties",
      issue_area = "intellectual_property",
      item_id = paste0("wipo_", treaty_id),
      treaty_type = "ip_treaty"
    )

  list(events = standardize_event(events), items = standardize_item(items))
}

# --- Build baseline events and items (extended, 5 domains only) ---
message("Building baseline events (extended to 2024, 5 domains)...")

unctad_out <- build_unctad_iia()
ieadb_out <- build_ieadb()
unhr_out <- build_un_hr_treaties()
arms_out <- build_arms_control()
wipo_out <- build_wipo_treaties()

all_events <- dplyr::bind_rows(
  unctad_out$events,
  ieadb_out$events,
  unhr_out$events,
  arms_out$events,
  wipo_out$events
)

all_items <- dplyr::bind_rows(
  unctad_out$items,
  ieadb_out$items,
  unhr_out$items,
  arms_out$items,
  wipo_out$items
)

all_events <- all_events %>%
  dplyr::filter(!is.na(event_year)) %>%
  dplyr::filter(event_year >= year_min & event_year <= year_max)

all_events <- all_events %>%
  dplyr::group_by(issue_area, iso3, item_id) %>%
  dplyr::summarise(
    source = dplyr::first(source),
    country_name = dplyr::first(country_name),
    event_year = min(event_year, na.rm = TRUE),
    event_date = dplyr::first(event_date),
    event_type = dplyr::first(event_type),
    item_name = dplyr::first(item_name),
    status = dplyr::first(status),
    treaty_type = dplyr::first(treaty_type),
    source_url = dplyr::first(source_url),
    .groups = "drop"
  )

all_items <- all_items %>%
  dplyr::group_by(issue_area, item_id) %>%
  dplyr::summarise(
    source = dplyr::first(source),
    item_name = dplyr::first(item_name),
    treaty_type = dplyr::first(treaty_type),
    treaty_open_year = dplyr::first(treaty_open_year),
    source_url = dplyr::first(source_url),
    .groups = "drop"
  )

open_fill <- all_events %>%
  dplyr::group_by(item_id) %>%
  dplyr::summarise(min_event_year = min(event_year, na.rm = TRUE), .groups = "drop")

all_items <- all_items %>%
  dplyr::left_join(open_fill, by = "item_id") %>%
  dplyr::mutate(
    treaty_open_year = dplyr::if_else(is.na(treaty_open_year), min_event_year, treaty_open_year)
  ) %>%
  dplyr::select(issue_area, item_id, source, item_name, treaty_type, treaty_open_year, source_url)

readr::write_csv(all_events, file.path(output_dir, "baseline_events_extended.csv"))
readr::write_csv(all_items, file.path(output_dir, "item_codebook_extended.csv"))

# --- Flow matrix builder (same logic as baseline) ---
build_flow_matrix <- function(events, items, issue_area) {
  ev <- events %>% dplyr::filter(issue_area == !!issue_area)
  it <- items %>% dplyr::filter(issue_area == !!issue_area)

  if (nrow(ev) == 0 || nrow(it) == 0) {
    return(NULL)
  }

  ev <- ev %>%
    dplyr::mutate(event_period = as.character(assign_period(event_year)))

  it <- it %>%
    dplyr::mutate(open_period = as.character(assign_period(treaty_open_year)))

  ev <- ev %>%
    dplyr::group_by(iso3, item_id) %>%
    dplyr::summarise(
      event_period = dplyr::first(event_period),
      event_year = dplyr::first(event_year),
      .groups = "drop"
    )

  start_period <- ev %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarise(start_period = min(event_period), .groups = "drop")

  start_period$start_index <- period_index(start_period$start_period)

  phantom <- expand.grid(
    item_id = it$item_id,
    period_label = period_labels,
    stringsAsFactors = FALSE
  )

  phantom <- phantom %>%
    dplyr::left_join(it, by = "item_id") %>%
    dplyr::mutate(
      period_index = period_index(period_label),
      open_index = period_index(open_period),
      phantom_item_id = paste0(item_id, "__", period_label)
    )

  phantom <- phantom %>%
    dplyr::mutate(open_index = dplyr::if_else(is.na(open_index), 0L, open_index)) %>%
    dplyr::filter(period_index >= open_index)

  ev_join <- ev %>%
    dplyr::mutate(join_index = period_index(event_period)) %>%
    dplyr::select(iso3, item_id, join_index)

  grid <- expand.grid(
    iso3 = unique(ev$iso3),
    phantom_item_id = phantom$phantom_item_id,
    stringsAsFactors = FALSE
  )

  grid <- grid %>%
    dplyr::left_join(phantom %>% dplyr::select(phantom_item_id, item_id, period_index, open_index), by = "phantom_item_id") %>%
    dplyr::left_join(ev_join, by = c("iso3", "item_id")) %>%
    dplyr::left_join(start_period %>% dplyr::select(iso3, start_index), by = "iso3")

  grid$rc <- ifelse(
    grid$period_index < grid$open_index,
    0L,
    ifelse(
      grid$period_index < grid$start_index,
      0L,
      ifelse(
        !is.na(grid$join_index) & grid$period_index == grid$join_index,
        1L,
        ifelse(
          !is.na(grid$join_index) & grid$period_index > grid$join_index,
          0L,
          -1L
        )
      )
    )
  )

  countries <- unique(ev$iso3)
  phantom_items <- phantom$phantom_item_id

  rc_matrix <- matrix(0L, nrow = length(countries), ncol = length(phantom_items))
  row_idx <- match(grid$iso3, countries)
  col_idx <- match(grid$phantom_item_id, phantom_items)
  rc_matrix[cbind(row_idx, col_idx)] <- grid$rc

  row_keep <- rowSums(rc_matrix != 0) > 0
  dropped_countries <- countries[!row_keep]
  rc_matrix <- rc_matrix[row_keep, , drop = FALSE]
  countries <- countries[row_keep]

  col_nonzero <- colSums(rc_matrix != 0) > 0
  col_variation <- apply(rc_matrix, 2, function(x) {
    vals <- unique(x[x != 0])
    length(vals) >= 2
  })
  col_keep <- col_nonzero & col_variation
  dropped_items <- phantom_items[!col_keep]
  rc_matrix <- rc_matrix[, col_keep, drop = FALSE]
  phantom_items <- phantom_items[col_keep]
  phantom <- phantom %>% dplyr::filter(phantom_item_id %in% phantom_items)

  startlegis <- start_period %>%
    dplyr::filter(iso3 %in% countries) %>%
    dplyr::arrange(match(iso3, countries)) %>%
    dplyr::pull(start_index)

  startlegis[is.na(startlegis)] <- 0L
  endlegis <- rep(length(period_labels) - 1L, length(countries))

  bill_session <- phantom %>%
    dplyr::arrange(match(phantom_item_id, phantom_items)) %>%
    dplyr::pull(period_index)

  list(
    rc = rc_matrix,
    startlegis = matrix(startlegis, ncol = 1),
    endlegis = matrix(endlegis, ncol = 1),
    bill.session = matrix(bill_session, ncol = 1),
    T = length(period_labels),
    country_codes = countries,
    item_labels = phantom_items,
    period_labels = period_labels,
    dropped_countries = dropped_countries,
    dropped_items = dropped_items
  )
}

issue_areas <- c("investment", "environment", "human_rights", "arms_control", "intellectual_property")
for (area in issue_areas) {
  flow <- build_flow_matrix(all_events, all_items, area)
  if (is.null(flow)) {
    next
  }
  saveRDS(flow, file.path(output_dir, paste0(area, "_flow_matrix_extended.rds")))

  if (length(flow$dropped_countries) > 0) {
    readr::write_csv(
      tibble::tibble(iso3 = flow$dropped_countries),
      file.path(output_dir, paste0("dropped_countries_", area, "_extended.csv"))
    )
  }

  if (length(flow$dropped_items) > 0) {
    readr::write_csv(
      tibble::tibble(item_id = flow$dropped_items),
      file.path(output_dir, paste0("dropped_items_", area, "_extended.csv"))
    )
  }
}

message("Extended (to 2024) preparation completed.")
