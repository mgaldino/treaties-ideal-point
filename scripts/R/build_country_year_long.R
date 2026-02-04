suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please install it with install.packages('countrycode').")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Please install it with install.packages('readxl').")
  }
  library(dplyr)
  library(countrycode)
  library(readxl)
})

output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990

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
  suppressWarnings(as.Date(x_char, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y")))
}

year_from_date <- function(x) {
  x_date <- parse_date_vec(x)
  out <- rep(NA_integer_, length(x_date))
  idx <- !is.na(x_date)
  out[idx] <- as.integer(format(x_date[idx], "%Y"))
  out
}

safe_iso3_from_name <- function(names) {
  countrycode::countrycode(names, "country.name", "iso3c", warn = FALSE)
}

standardize_output <- function(df) {
  cols <- c(
    "source",
    "issue_area",
    "iso3",
    "country_name",
    "year",
    "event_type",
    "event_date",
    "org_or_treaty_id",
    "org_or_treaty_name",
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
  df$year <- suppressWarnings(as.integer(df$year))
  df$event_date <- as.character(df$event_date)
  df$status <- as.character(df$status)
  df$treaty_type <- as.character(df$treaty_type)
  df$org_or_treaty_id <- as.character(df$org_or_treaty_id)
  df$org_or_treaty_name <- as.character(df$org_or_treaty_name)
  df$source <- as.character(df$source)
  df$issue_area <- as.character(df$issue_area)
  df$event_type <- as.character(df$event_type)
  df$source_url <- as.character(df$source_url)
  dplyr::select(df, dplyr::all_of(cols))
}

# --- DESTA ---

build_desta <- function() {
  desta_list_path <- "data/raw/desta/desta_list_of_treaties_02_03.csv"
  desta_dyads_path <- "data/raw/desta/desta_list_of_treaties_02_03_dyads.csv"
  if (!file.exists(desta_list_path) || !file.exists(desta_dyads_path)) {
    return(data.frame())
  }

  meta <- read.csv(desta_list_path, stringsAsFactors = FALSE)
  meta <- dplyr::select(meta, number, name, entry_type, wto_listed, regioncon, typememb)

  dyads <- read.csv(desta_dyads_path, stringsAsFactors = FALSE)
  dyads <- dplyr::select(dyads, iso1, iso2, number, year, entryforceyear)
  dyads$year <- suppressWarnings(as.integer(dyads$year))
  dyads$entryforceyear <- suppressWarnings(as.integer(dyads$entryforceyear))

  long_sig <- data.frame(
    iso3 = c(dyads$iso1, dyads$iso2),
    year = c(dyads$year, dyads$year),
    number = c(dyads$number, dyads$number),
    stringsAsFactors = FALSE
  )

  long_eif <- data.frame(
    iso3 = c(dyads$iso1, dyads$iso2),
    year = c(dyads$entryforceyear, dyads$entryforceyear),
    number = c(dyads$number, dyads$number),
    stringsAsFactors = FALSE
  )

  long_sig <- long_sig %>% dplyr::filter(!is.na(iso3) & !is.na(year) & year >= year_min)
  long_eif <- long_eif %>% dplyr::filter(!is.na(iso3) & !is.na(year) & year >= year_min)

  long_sig <- dplyr::left_join(long_sig, meta, by = "number")
  long_eif <- dplyr::left_join(long_eif, meta, by = "number")

  long_sig <- long_sig %>%
    dplyr::mutate(
      source = "DESTA",
      issue_area = "trade",
      event_type = "signature",
      event_date = NA,
      org_or_treaty_id = number,
      org_or_treaty_name = name,
      status = entry_type,
      treaty_type = typememb,
      source_url = NA,
      country_name = NA
    )

  long_eif <- long_eif %>%
    dplyr::mutate(
      source = "DESTA",
      issue_area = "trade",
      event_type = "entry_into_force",
      event_date = NA,
      org_or_treaty_id = number,
      org_or_treaty_name = name,
      status = entry_type,
      treaty_type = typememb,
      source_url = NA,
      country_name = NA
    )

  out <- dplyr::bind_rows(long_sig, long_eif)
  standardize_output(out)
}

# --- ATOP ---

build_atop <- function() {
  atop_zip <- "data/raw/atop/atop_5.1__.csv_.zip"
  atop_dir <- "data/processed/atop"
  if (!file.exists(atop_zip)) {
    return(data.frame())
  }

  dir.create(atop_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(atop_zip, exdir = atop_dir)
  atop_sy_path <- file.path(atop_dir, "ATOP 5.1 (.csv)", "atop5_1sy.csv")
  if (!file.exists(atop_sy_path)) {
    return(data.frame())
  }

  atop <- read.csv(atop_sy_path, stringsAsFactors = FALSE)
  atop <- dplyr::select(atop, state, year, atopally, defense, offense, neutral, nonagg, consul)
  atop$year <- suppressWarnings(as.integer(atop$year))
  atop$iso3 <- countrycode::countrycode(atop$state, "cown", "iso3c", warn = FALSE)
  atop$country_name <- countrycode::countrycode(atop$iso3, "iso3c", "country.name", warn = FALSE)

  types <- c("atopally", "defense", "offense", "neutral", "nonagg", "consul")
  labels <- c(
    atopally = "alliance_any",
    defense = "alliance_defense",
    offense = "alliance_offense",
    neutral = "alliance_neutral",
    nonagg = "alliance_nonaggression",
    consul = "alliance_consultation"
  )

  rows <- list()
  for (type_name in types) {
    subset <- atop %>%
      dplyr::filter(!is.na(year) & year >= year_min & .data[[type_name]] > 0 & !is.na(iso3)) %>%
      dplyr::mutate(
        source = "ATOP",
        issue_area = "security",
        event_type = labels[[type_name]],
        event_date = NA,
        org_or_treaty_id = labels[[type_name]],
        org_or_treaty_name = gsub("_", " ", labels[[type_name]]),
        status = NA,
        treaty_type = labels[[type_name]],
        source_url = NA
      )
    rows[[type_name]] <- subset
  }

  out <- dplyr::bind_rows(rows)
  out <- dplyr::select(
    out,
    source,
    issue_area,
    iso3,
    country_name,
    year,
    event_type,
    event_date,
    org_or_treaty_id,
    org_or_treaty_name,
    status,
    treaty_type,
    source_url
  )
  standardize_output(out)
}

# --- IEADB ---

build_ieadb <- function() {
  ieadb_zip <- "data/raw/ieadb/all-csv-files.zip"
  ieadb_dir <- "data/processed/ieadb"
  if (!file.exists(ieadb_zip)) {
    return(data.frame())
  }

  dir.create(ieadb_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(ieadb_zip, exdir = ieadb_dir)
  ieadb_members_path <- file.path(ieadb_dir, "db_members.csv")
  if (!file.exists(ieadb_members_path)) {
    return(data.frame())
  }

  members <- read.delim(
    ieadb_members_path,
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
  members <- members %>% dplyr::filter(!is.na(iso3) & !is.na(Year) & Year >= year_min)

  action_lower <- tolower(members$`Action Type`)
  members$event_type <- dplyr::case_when(
    grepl("signature", action_lower) ~ "signature",
    grepl("ratification", action_lower) ~ "ratification_accession",
    grepl("accession", action_lower) ~ "ratification_accession",
    grepl("entry into force", action_lower) ~ "entry_into_force",
    TRUE ~ "other"
  )

  out <- members %>%
    dplyr::mutate(
      source = "IEADB",
      issue_area = "environment",
      country_name = `Country (official name)`,
      year = Year,
      event_date = NA,
      org_or_treaty_id = `Mitch ID`,
      org_or_treaty_name = `Treaty Name from Treaty Table`,
      status = `Action Type`,
      treaty_type = `Agreement Type - Level 1`,
      source_url = NA
    )

  out <- dplyr::select(
    out,
    source,
    issue_area,
    iso3,
    country_name,
    year,
    event_type,
    event_date,
    org_or_treaty_id,
    org_or_treaty_name,
    status,
    treaty_type,
    source_url
  )

  standardize_output(out)
}

# --- WTO RTA ---

build_wto_rta <- function() {
  wto_path <- "data/raw/wto_rta/WTO_RTA_AllRTAs.xlsx"
  if (!file.exists(wto_path)) {
    return(list(data = data.frame(), excluded = data.frame()))
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

  wto$year_signature <- year_from_date(sign_date)
  wto$year_eif <- year_from_date(eif_date)
  wto$event_date_signature <- as.character(sign_date)
  wto$event_date_eif <- as.character(eif_date)

  wto$signatories <- wto$signatories_current
  missing_sig <- is.na(wto$signatories) | wto$signatories == ""
  wto$signatories[missing_sig] <- wto$signatories_original[missing_sig]

  expand_rows <- function(df, year_col, date_col, event_type) {
    records <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
      year_val <- df[[year_col]][i]
      if (is.na(year_val) || year_val < year_min) {
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
      records[[i]] <- data.frame(
        signatory = parts,
        year = rep(year_val, length(parts)),
        event_type = event_type,
        event_date = rep(df[[date_col]][i], length(parts)),
        rta_id = rep(df$rta_id[i], length(parts)),
        rta_name = rep(df$rta_name[i], length(parts)),
        status = rep(df$status[i], length(parts)),
        treaty_type = rep(df$treaty_type[i], length(parts)),
        stringsAsFactors = FALSE
      )
    }
    dplyr::bind_rows(records)
  }

  sig_rows <- expand_rows(wto, "year_signature", "event_date_signature", "signature")
  eif_rows <- expand_rows(wto, "year_eif", "event_date_eif", "entry_into_force")

  all_rows <- dplyr::bind_rows(sig_rows, eif_rows)
  all_rows$iso3 <- safe_iso3_from_name(all_rows$signatory)

  excluded <- all_rows %>%
    dplyr::filter(is.na(iso3)) %>%
    dplyr::distinct(signatory)

  all_rows <- all_rows %>% dplyr::filter(!is.na(iso3))
  all_rows$country_name <- countrycode::countrycode(all_rows$iso3, "iso3c", "country.name", warn = FALSE)

  out <- all_rows %>%
    dplyr::mutate(
      source = "WTO RTA",
      issue_area = "trade",
      org_or_treaty_id = rta_id,
      org_or_treaty_name = rta_name,
      source_url = NA
    )

  out <- dplyr::select(
    out,
    source,
    issue_area,
    iso3,
    country_name,
    year,
    event_type,
    event_date,
    org_or_treaty_id,
    org_or_treaty_name,
    status,
    treaty_type,
    source_url
  )

  list(data = standardize_output(out), excluded = excluded)
}

# --- UNCTAD IIA ---

build_unctad_iia <- function() {
  unctad_path <- "data/raw/unctad_iia/unctad_iia_country_treaties.csv"
  if (!file.exists(unctad_path)) {
    return(list(data = data.frame(), excluded = data.frame()))
  }

  raw <- read.csv(unctad_path, stringsAsFactors = FALSE)
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
  raw$event_date_signature <- as.character(parse_date_vec(raw$date_of_signature))
  raw$event_date_eif <- as.character(parse_date_vec(raw$date_of_entry_into_force))

  sig <- raw %>%
    dplyr::filter(!is.na(signature_year) & signature_year >= year_min) %>%
    dplyr::mutate(
      year = signature_year,
      event_type = "signature",
      event_date = event_date_signature
    )

  eif <- raw %>%
    dplyr::filter(!is.na(eif_year) & eif_year >= year_min) %>%
    dplyr::mutate(
      year = eif_year,
      event_type = "entry_into_force",
      event_date = event_date_eif
    )

  combined <- dplyr::bind_rows(sig, eif) %>%
    dplyr::mutate(
      source = "UNCTAD IIA",
      issue_area = "investment",
      org_or_treaty_id = treaty_url,
      org_or_treaty_name = full_title,
      source_url = treaty_url,
      treaty_type = type
    )

  out <- dplyr::select(
    combined,
    source,
    issue_area,
    iso3,
    country_name,
    year,
    event_type,
    event_date,
    org_or_treaty_id,
    org_or_treaty_name,
    status,
    treaty_type,
    source_url
  )

  list(data = standardize_output(out), excluded = excluded)
}

# --- Build all ---

all_rows <- list()
all_rows[["desta"]] <- build_desta()
all_rows[["atop"]] <- build_atop()
all_rows[["ieadb"]] <- build_ieadb()

wto_out <- build_wto_rta()
all_rows[["wto_rta"]] <- wto_out$data

unctad_out <- build_unctad_iia()
all_rows[["unctad_iia"]] <- unctad_out$data

combined <- dplyr::bind_rows(all_rows)
combined <- combined %>%
  dplyr::filter(!is.na(iso3) & !is.na(year)) %>%
  dplyr::arrange(source, iso3, year, event_type)

write.csv(combined, file = file.path(output_dir, "country_year_long.csv"), row.names = FALSE)
saveRDS(combined, file = file.path(output_dir, "country_year_long.rds"))

if (nrow(wto_out$excluded) > 0) {
  write.csv(wto_out$excluded, file = file.path(output_dir, "excluded_entities_wto_rta.csv"), row.names = FALSE)
}

if (nrow(unctad_out$excluded) > 0) {
  write.csv(unctad_out$excluded, file = file.path(output_dir, "excluded_entities_unctad_iia.csv"), row.names = FALSE)
}
