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

output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

logs_dir <- "logs"
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990
year_max <- 2018

period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2019)
period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018")

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

# --- DESTA (trade) ---
build_desta <- function() {
  meta_path <- "data/raw/desta/desta_list_of_treaties_02_03.csv"
  dyads_path <- "data/raw/desta/desta_list_of_treaties_02_03_dyads.csv"
  if (!file.exists(meta_path) || !file.exists(dyads_path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  meta <- readr::read_csv(meta_path, show_col_types = FALSE)
  meta <- dplyr::select(meta, number, name, entry_type, typememb)

  dyads <- readr::read_csv(dyads_path, show_col_types = FALSE)
  dyads <- dplyr::select(dyads, iso1, iso2, number, year, entryforceyear)
  dyads$year <- suppressWarnings(as.integer(dyads$year))
  dyads$entryforceyear <- suppressWarnings(as.integer(dyads$entryforceyear))

  open_year <- dyads %>%
    dplyr::filter(!is.na(number) & !is.na(year)) %>%
    dplyr::group_by(number) %>%
    dplyr::summarise(treaty_open_year = min(year, na.rm = TRUE), .groups = "drop")

  long_eif <- tibble::tibble(
    cow = c(dyads$iso1, dyads$iso2),
    event_year = c(dyads$entryforceyear, dyads$entryforceyear),
    number = c(dyads$number, dyads$number)
  )
  long_eif$iso3 <- countrycode::countrycode(long_eif$cow, "cown", "iso3c", warn = FALSE)

  long_eif <- long_eif %>%
    dplyr::filter(!is.na(iso3) & !is.na(event_year)) %>%
    dplyr::filter(event_year >= year_min & event_year <= year_max)

  long_eif <- long_eif %>%
    dplyr::left_join(meta, by = "number")

  events <- long_eif %>%
    dplyr::mutate(
      source = "DESTA",
      issue_area = "trade",
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE),
      event_date = NA,
      event_type = "entry_into_force",
      item_id = paste0("desta_", number),
      item_name = name,
      status = entry_type,
      treaty_type = typememb,
      source_url = NA
    )

  items <- meta %>%
    dplyr::left_join(open_year, by = "number") %>%
    dplyr::mutate(
      source = "DESTA",
      issue_area = "trade",
      item_id = paste0("desta_", number),
      item_name = name,
      treaty_type = typememb,
      source_url = NA
    )

  list(events = standardize_event(events), items = standardize_item(items))
}

# --- WTO RTA (trade) ---
build_wto_rta <- function() {
  wto_path <- "data/raw/wto_rta/WTO_RTA_AllRTAs.xlsx"
  if (!file.exists(wto_path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble(), excluded = tibble::tibble()))
  }

  wto_raw <- readxl::read_excel(wto_path, sheet = "AllRTAs")
  wto_raw <- as.data.frame(wto_raw)

  wto <- dplyr::select(
    wto_raw,
    `RTA ID`,
    `RTA Name`,
    `Date of Signature (G)`,
    `Date of Signature (S)`,
    `Date of Entry into Force (G)`,
    `Date of Entry into Force (S)`,
    `Current signatories`,
    `Original signatories`,
    Status,
    Type
  )

  wto <- dplyr::rename(
    wto,
    rta_id = `RTA ID`,
    rta_name = `RTA Name`,
    sign_g = `Date of Signature (G)`,
    sign_s = `Date of Signature (S)`,
    eif_g = `Date of Entry into Force (G)`,
    eif_s = `Date of Entry into Force (S)`,
    signatories_current = `Current signatories`,
    signatories_original = `Original signatories`,
    status = Status,
    treaty_type = Type
  )

  sign_g_date <- parse_date_vec(wto$sign_g)
  sign_s_date <- parse_date_vec(wto$sign_s)
  eif_g_date <- parse_date_vec(wto$eif_g)
  eif_s_date <- parse_date_vec(wto$eif_s)

  sign_date <- sign_g_date
  sign_date[is.na(sign_date)] <- sign_s_date[is.na(sign_date)]
  eif_date <- eif_g_date
  eif_date[is.na(eif_date)] <- eif_s_date[is.na(eif_date)]

  wto$open_year <- year_from_date(sign_date)
  wto$eif_year <- year_from_date(eif_date)
  wto$event_date <- as.character(eif_date)

  wto$signatories <- wto$signatories_current
  missing_sig <- is.na(wto$signatories) | wto$signatories == ""
  wto$signatories[missing_sig] <- wto$signatories_original[missing_sig]

  expand_rows <- function(df) {
    records <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
      year_val <- df$eif_year[i]
      if (is.na(year_val) || year_val < year_min || year_val > year_max) {
        next
      }
      sign_str <- df$signatories[i]
      if (is.na(sign_str) || sign_str == "") {
        next
      }
      parts <- unlist(strsplit(sign_str, ";"))
      parts <- trimws(parts)
      parts <- parts[parts != ""]
      if (!length(parts)) {
        next
      }
      records[[i]] <- tibble::tibble(
        signatory = parts,
        event_year = rep(year_val, length(parts)),
        event_date = rep(df$event_date[i], length(parts)),
        rta_id = rep(df$rta_id[i], length(parts)),
        rta_name = rep(df$rta_name[i], length(parts)),
        status = rep(df$status[i], length(parts)),
        treaty_type = rep(df$treaty_type[i], length(parts)),
        open_year = rep(df$open_year[i], length(parts))
      )
    }
    dplyr::bind_rows(records)
  }

  all_rows <- expand_rows(wto)
  if (nrow(all_rows) == 0) {
    return(list(events = tibble::tibble(), items = tibble::tibble(), excluded = tibble::tibble()))
  }

  all_rows$iso3 <- safe_iso3_from_name(all_rows$signatory)
  excluded <- all_rows %>%
    dplyr::filter(is.na(iso3)) %>%
    dplyr::distinct(signatory)

  all_rows <- all_rows %>%
    dplyr::filter(!is.na(iso3))

  events <- all_rows %>%
    dplyr::mutate(
      source = "WTO RTA",
      issue_area = "trade",
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE),
      event_type = "entry_into_force",
      item_id = paste0("wto_rta_", rta_id),
      item_name = rta_name,
      source_url = NA
    )

  items <- wto %>%
    dplyr::mutate(
      source = "WTO RTA",
      issue_area = "trade",
      item_id = paste0("wto_rta_", rta_id),
      item_name = rta_name,
      treaty_open_year = open_year,
      source_url = NA
    ) %>%
    dplyr::select(source, issue_area, item_id, item_name, treaty_type, treaty_open_year, source_url)

  list(events = standardize_event(events), items = standardize_item(items), excluded = excluded)
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

# --- ATOP (security) ---
build_atop <- function() {
  zip_path <- "data/raw/atop/atop_5.1__.csv_.zip"
  if (!file.exists(zip_path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  atop_dir <- "data/processed/atop"
  dir.create(atop_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = atop_dir)

  member_path <- file.path(atop_dir, "ATOP 5.1 (.csv)", "atop5_1m.csv")
  alliance_path <- file.path(atop_dir, "ATOP 5.1 (.csv)", "atop5_1a.csv")

  if (!file.exists(member_path) || !file.exists(alliance_path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble()))
  }

  members <- readr::read_csv(member_path, show_col_types = FALSE)
  members <- dplyr::select(members, atopid, member, yrent)
  members$yrent <- suppressWarnings(as.integer(members$yrent))
  members$iso3 <- countrycode::countrycode(members$member, "cown", "iso3c", warn = FALSE)

  members <- members %>%
    dplyr::filter(!is.na(iso3) & !is.na(yrent)) %>%
    dplyr::filter(yrent >= year_min & yrent <= year_max)

  alliances <- readr::read_csv(alliance_path, show_col_types = FALSE)
  alliances <- dplyr::select(alliances, atopid, begyr, defense, offense, neutral, nonagg, consul)
  alliances$begyr <- suppressWarnings(as.integer(alliances$begyr))

  type_cols <- c("defense", "offense", "neutral", "nonagg", "consul")
  type_df <- tibble::tibble(atopid = integer(), begyr = integer(), type = character())
  for (t in type_cols) {
    sub <- alliances %>%
      dplyr::filter(.data[[t]] == 1) %>%
      dplyr::select(atopid, begyr) %>%
      dplyr::mutate(type = t)
    if (nrow(sub) > 0) {
      type_df <- dplyr::bind_rows(type_df, sub)
    }
  }

  any_rows <- alliances %>%
    dplyr::select(atopid, begyr) %>%
    dplyr::mutate(type = "any")

  type_df <- dplyr::bind_rows(type_df, any_rows)

  joined <- members %>%
    dplyr::inner_join(type_df, by = "atopid")

  events <- joined %>%
    dplyr::mutate(
      source = "ATOP",
      issue_area = "security",
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE),
      event_year = yrent,
      event_date = NA,
      event_type = "entry_into_force",
      item_id = paste0("atop_", atopid, "_", type),
      item_name = paste("ATOP", atopid, type),
      status = NA,
      treaty_type = type,
      source_url = NA
    )

  items <- type_df %>%
    dplyr::mutate(
      source = "ATOP",
      issue_area = "security",
      item_id = paste0("atop_", atopid, "_", type),
      item_name = paste("ATOP", atopid, type),
      treaty_type = type,
      treaty_open_year = begyr,
      source_url = NA
    ) %>%
    dplyr::select(source, issue_area, item_id, item_name, treaty_type, treaty_open_year, source_url)

  list(events = standardize_event(events), items = standardize_item(items))
}

# --- WTO accession (trade) ---
build_wto_accession <- function() {
  path <- "data/raw/wto_accession/wto_members.csv"
  if (!file.exists(path)) {
    return(list(events = tibble::tibble(), items = tibble::tibble(), excluded = tibble::tibble()))
  }

  wto <- readr::read_csv(path, show_col_types = FALSE)
  wto$accession_year <- as.integer(format(as.Date(wto$accession_date), "%Y"))
  wto$iso3 <- safe_iso3_from_name(wto$country_name)

  excluded <- wto %>%
    dplyr::filter(is.na(iso3)) %>%
    dplyr::distinct(country_name)

  wto <- wto %>%
    dplyr::filter(!is.na(iso3) & !is.na(accession_year))

  wto <- wto %>%
    dplyr::filter(accession_year >= year_min & accession_year <= year_max)

  events <- wto %>%
    dplyr::mutate(
      source = "WTO Accession",
      issue_area = "trade",
      event_year = accession_year,
      event_date = accession_date,
      event_type = "entry_into_force",
      item_id = "wto_accession",
      item_name = "WTO accession",
      status = NA,
      treaty_type = "wto_accession",
      source_url = "https://www.wto.org/english/thewto_e/acc_e/acc_status_e.htm"
    )

  items <- tibble::tibble(
    source = "WTO Accession",
    issue_area = "trade",
    item_id = "wto_accession",
    item_name = "WTO accession",
    treaty_type = "wto_accession",
    treaty_open_year = min(events$event_year, na.rm = TRUE),
    source_url = "https://www.wto.org/english/thewto_e/acc_e/acc_status_e.htm"
  )

  list(events = standardize_event(events), items = standardize_item(items), excluded = excluded)
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

  # Use action_date (ratification/accession) as the event
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

  # Use action_date preferentially; fall back to entry_into_force_date
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

# --- Build baseline events and items ---

message("Building baseline events...")

# Trade: DESTA (deduplicated) + WTO RTA + WTO accession
trade_dedup <- NULL
if (file.exists("data/processed/trade_dedup_matches.csv")) {
  trade_dedup <- readr::read_csv("data/processed/trade_dedup_matches.csv", show_col_types = FALSE)
}

trade_drop_ids <- character()
if (!is.null(trade_dedup)) {
  trade_drop_ids <- trade_dedup %>%
    dplyr::filter(match_ok == TRUE) %>%
    dplyr::pull(desta_id)
  trade_drop_ids <- as.character(trade_drop_ids)
}

desta_out <- build_desta()
if (nrow(desta_out$events) > 0 && length(trade_drop_ids) > 0) {
  desta_out$events <- desta_out$events %>%
    dplyr::filter(!gsub("^desta_", "", item_id) %in% trade_drop_ids)
  desta_out$items <- desta_out$items %>%
    dplyr::filter(!gsub("^desta_", "", item_id) %in% trade_drop_ids)
}

wto_rta_out <- build_wto_rta()
wto_acc_out <- build_wto_accession()

unctad_out <- build_unctad_iia()
ieadb_out <- build_ieadb()
atop_out <- build_atop()

# New issue areas (Phase 14)
unhr_out <- build_un_hr_treaties()
arms_out <- build_arms_control()
wipo_out <- build_wipo_treaties()

all_events <- dplyr::bind_rows(
  desta_out$events,
  wto_rta_out$events,
  wto_acc_out$events,
  unctad_out$events,
  ieadb_out$events,
  atop_out$events,
  unhr_out$events,
  arms_out$events,
  wipo_out$events
)

all_items <- dplyr::bind_rows(
  desta_out$items,
  wto_rta_out$items,
  wto_acc_out$items,
  unctad_out$items,
  ieadb_out$items,
  atop_out$items,
  unhr_out$items,
  arms_out$items,
  wipo_out$items
)

# Filter baseline events within window
all_events <- all_events %>%
  dplyr::filter(!is.na(event_year)) %>%
  dplyr::filter(event_year >= year_min & event_year <= year_max)

# Collapse to earliest event per country-item
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

# Item codebook
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

# Fill missing open years with earliest event year
open_fill <- all_events %>%
  dplyr::group_by(item_id) %>%
  dplyr::summarise(min_event_year = min(event_year, na.rm = TRUE), .groups = "drop")

all_items <- all_items %>%
  dplyr::left_join(open_fill, by = "item_id") %>%
  dplyr::mutate(
    treaty_open_year = dplyr::if_else(is.na(treaty_open_year), min_event_year, treaty_open_year)
  ) %>%
  dplyr::select(issue_area, item_id, source, item_name, treaty_type, treaty_open_year, source_url)

# Save baseline events and items
readr::write_csv(all_events, file.path(output_dir, "baseline_events.csv"))
readr::write_csv(all_items, file.path(output_dir, "item_codebook.csv"))

# Save excluded entities (mapping failures)
if (nrow(wto_rta_out$excluded) > 0) {
  readr::write_csv(wto_rta_out$excluded, file.path(output_dir, "excluded_entities_wto_rta_baseline.csv"))
}
if (nrow(unctad_out$excluded) > 0) {
  readr::write_csv(unctad_out$excluded, file.path(output_dir, "excluded_entities_unctad_iia_baseline.csv"))
}
if (nrow(wto_acc_out$excluded) > 0) {
  readr::write_csv(wto_acc_out$excluded, file.path(output_dir, "excluded_entities_wto_accession_baseline.csv"))
}

# Country codebook (first event per country)
country_codebook <- all_events %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(
    country_name = dplyr::first(country_name),
    first_event_year = min(event_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    first_event_period = as.character(assign_period(first_event_year))
  )

readr::write_csv(country_codebook, file.path(output_dir, "country_codebook.csv"))

# --- Flow matrix builder ---
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

  # Country start periods (proxy: first observed event)
  start_period <- ev %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarise(start_period = min(event_period), .groups = "drop")

  start_period$start_index <- period_index(start_period$start_period)

  # Phantom items
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
    dplyr::filter(!is.na(open_index)) %>%
    dplyr::filter(period_index >= open_index)

  # Country-item join period
  ev_join <- ev %>%
    dplyr::mutate(join_index = period_index(event_period)) %>%
    dplyr::select(iso3, item_id, join_index)

  # Grid
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

  # Drop all-zero rows
  row_keep <- rowSums(rc_matrix != 0) > 0
  dropped_countries <- countries[!row_keep]
  rc_matrix <- rc_matrix[row_keep, , drop = FALSE]
  countries <- countries[row_keep]

  # Drop columns with no variation or all-zero
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

  # startlegis/endlegis/bill.session
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

issue_areas <- c("trade", "investment", "security", "environment",
                  "human_rights", "arms_control", "intellectual_property")
for (area in issue_areas) {
  flow <- build_flow_matrix(all_events, all_items, area)
  if (is.null(flow)) {
    next
  }
  saveRDS(flow, file.path(output_dir, paste0(area, "_flow_matrix.rds")))

  if (length(flow$dropped_countries) > 0) {
    readr::write_csv(
      tibble::tibble(iso3 = flow$dropped_countries),
      file.path(output_dir, paste0("dropped_countries_", area, ".csv"))
    )
  }

  if (length(flow$dropped_items) > 0) {
    readr::write_csv(
      tibble::tibble(item_id = flow$dropped_items),
      file.path(output_dir, paste0("dropped_items_", area, ".csv"))
    )
  }
}

message("Phase 1 preparation completed.")
