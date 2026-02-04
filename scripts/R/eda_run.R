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

output_dir <- "outputs/eda"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990

escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([%_&$#])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

write_latex_table <- function(df, path, caption, label) {
  df_out <- df
  for (col in names(df_out)) {
    if (is.character(df_out[[col]])) {
      df_out[[col]] <- escape_latex(df_out[[col]])
    }
  }

  header_names <- vapply(names(df_out), escape_latex, character(1))
  header <- paste(header_names, collapse = " & ")
  rows <- apply(df_out, 1, function(row) paste(row, collapse = " & "))
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sprintf("\\caption{%s}", escape_latex(caption)),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{%s}", paste(rep("l", ncol(df_out)), collapse = "")),
    "\\hline",
    paste0(header, " \\\\") ,
    "\\hline",
    paste0(rows, " \\\\") ,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, con = path)
}

map_region_iso3 <- function(iso3) {
  region <- countrycode::countrycode(iso3, "iso3c", "un.region.name", warn = FALSE)
  region[is.na(region)] <- "Other/Unknown"
  region
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
  suppressWarnings(as.Date(x_char, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y")))
}

year_from_date <- function(x) {
  x_date <- parse_date_vec(x)
  out <- rep(NA_integer_, length(x_date))
  idx <- !is.na(x_date)
  out[idx] <- as.integer(format(x_date[idx], "%Y"))
  out
}

map_iso3_from_name <- function(names) {
  countrycode::countrycode(names, "country.name", "iso3c", warn = FALSE)
}

plot_line <- function(df, x_col, y_col, file, title, ylab) {
  png(file, width = 1000, height = 600)
  plot(
    df[[x_col]],
    df[[y_col]],
    type = "l",
    lwd = 2,
    col = "#1b9e77",
    xlab = "Year",
    ylab = ylab,
    main = title
  )
  abline(v = year_min, col = "#999999", lty = 3)
  dev.off()
}

plot_group_lines <- function(df, file, title, ylab) {
  years <- sort(unique(df$year))
  groups <- sort(unique(df$group))
  full <- merge(
    expand.grid(year = years, group = groups, stringsAsFactors = FALSE),
    df,
    by = c("year", "group"),
    all.x = TRUE
  )
  full$value[is.na(full$value)] <- 0
  palette <- grDevices::rainbow(length(groups))
  y_max <- max(full$value, na.rm = TRUE)
  y_min <- min(full$value, na.rm = TRUE)
  if (!is.finite(y_max)) {
    y_max <- 0
  }
  if (!is.finite(y_min)) {
    y_min <- 0
  }
  pad <- max(1, 0.05 * (y_max - y_min))
  ylim <- c(max(0, y_min - pad), y_max + pad)

  png(file, width = 1100, height = 650)
  plot(
    years,
    full$value[full$group == groups[1]],
    type = "l",
    lwd = 2,
    col = palette[1],
    xlab = "Year",
    ylab = ylab,
    main = title,
    ylim = ylim
  )
  if (length(groups) > 1) {
    for (i in seq_along(groups)[-1]) {
      lines(years, full$value[full$group == groups[i]], col = palette[i], lwd = 2)
    }
  }
  legend("topright", legend = groups, col = palette, lty = 1, cex = 0.8)
  abline(v = year_min, col = "#999999", lty = 3)
  dev.off()
}

split_signatories <- function(x) {
  if (is.na(x)) {
    return(character(0))
  }
  parts <- unlist(strsplit(as.character(x), ";"))
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  parts
}

expand_signatories <- function(df, year_col, sign_col) {
  records <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    year_val <- df[[year_col]][i]
    if (is.na(year_val) || year_val < year_min) {
      next
    }
    sign_str <- df[[sign_col]][i]
    parts <- split_signatories(sign_str)
    if (!length(parts)) {
      next
    }
    records[[i]] <- data.frame(
      year = rep(year_val, length(parts)),
      signatory = parts,
      stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(records)
}

summary_rows <- list()

# --- DESTA ---

desta_list_path <- "data/raw/desta/desta_list_of_treaties_02_03.csv"
desta_dyads_path <- "data/raw/desta/desta_list_of_treaties_02_03_dyads.csv"

if (file.exists(desta_list_path)) {
  desta_list <- read.csv(desta_list_path, stringsAsFactors = FALSE)
  desta_list <- dplyr::select(
    desta_list,
    number,
    year,
    entryforceyear,
    wto_listed,
    regioncon,
    typememb
  )
  desta_list$year <- suppressWarnings(as.integer(desta_list$year))
  desta_list$entryforceyear <- suppressWarnings(as.integer(desta_list$entryforceyear))

  desta_year_counts <- desta_list %>%
    dplyr::filter(!is.na(year) & year >= year_min) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(treaties = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(year)

  write.csv(desta_year_counts, file = file.path(output_dir, "desta_year_counts.csv"), row.names = FALSE)
  plot_line(
    desta_year_counts,
    x_col = "year",
    y_col = "treaties",
    file = file.path(output_dir, "fig_desta_treaties_per_year.png"),
    title = "DESTA treaties per year (signature year)",
    ylab = "Number of treaties"
  )

  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    dataset = "DESTA",
    rows = nrow(desta_list),
    unique_countries = NA_integer_,
    year_min = min(desta_list$year, na.rm = TRUE),
    year_max = max(desta_list$year, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

if (file.exists(desta_dyads_path)) {
  desta_dyads <- read.csv(desta_dyads_path, stringsAsFactors = FALSE)
  desta_dyads <- dplyr::select(desta_dyads, iso1, iso2, year, entryforceyear)
  desta_dyads$year <- suppressWarnings(as.integer(desta_dyads$year))
  desta_dyads$entryforceyear <- suppressWarnings(as.integer(desta_dyads$entryforceyear))

  desta_long_sig <- data.frame(
    iso3 = c(desta_dyads$iso1, desta_dyads$iso2),
    year = c(desta_dyads$year, desta_dyads$year),
    stringsAsFactors = FALSE
  )

  desta_long_sig <- desta_long_sig %>%
    dplyr::filter(!is.na(iso3) & !is.na(year) & year >= year_min)

  desta_country_counts_sig <- desta_long_sig %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
    dplyr::arrange(year)

  write.csv(
    desta_country_counts_sig,
    file = file.path(output_dir, "desta_country_counts_signature.csv"),
    row.names = FALSE
  )

  plot_line(
    desta_country_counts_sig,
    x_col = "year",
    y_col = "countries",
    file = file.path(output_dir, "fig_desta_countries_signature.png"),
    title = "DESTA countries per year (signature year)",
    ylab = "Unique countries"
  )

  desta_sig_region <- desta_long_sig %>%
    dplyr::mutate(region = map_region_iso3(iso3)) %>%
    dplyr::group_by(year, region) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

  write.csv(
    desta_sig_region,
    file = file.path(output_dir, "desta_region_counts_signature.csv"),
    row.names = FALSE
  )

  plot_group_lines(
    df = data.frame(
      year = desta_sig_region$year,
      group = desta_sig_region$region,
      value = desta_sig_region$countries,
      stringsAsFactors = FALSE
    ),
    file = file.path(output_dir, "fig_desta_region_signature.png"),
    title = "DESTA countries by region (signature year)",
    ylab = "Unique countries"
  )

  desta_long_eif <- data.frame(
    iso3 = c(desta_dyads$iso1, desta_dyads$iso2),
    year = c(desta_dyads$entryforceyear, desta_dyads$entryforceyear),
    stringsAsFactors = FALSE
  )

  desta_long_eif <- desta_long_eif %>%
    dplyr::filter(!is.na(iso3) & !is.na(year) & year >= year_min)

  desta_country_counts_eif <- desta_long_eif %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
    dplyr::arrange(year)

  write.csv(
    desta_country_counts_eif,
    file = file.path(output_dir, "desta_country_counts_eif.csv"),
    row.names = FALSE
  )

  plot_line(
    desta_country_counts_eif,
    x_col = "year",
    y_col = "countries",
    file = file.path(output_dir, "fig_desta_countries_eif.png"),
    title = "DESTA countries per year (entry into force)",
    ylab = "Unique countries"
  )

  desta_eif_region <- desta_long_eif %>%
    dplyr::mutate(region = map_region_iso3(iso3)) %>%
    dplyr::group_by(year, region) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

  write.csv(
    desta_eif_region,
    file = file.path(output_dir, "desta_region_counts_eif.csv"),
    row.names = FALSE
  )

  plot_group_lines(
    df = data.frame(
      year = desta_eif_region$year,
      group = desta_eif_region$region,
      value = desta_eif_region$countries,
      stringsAsFactors = FALSE
    ),
    file = file.path(output_dir, "fig_desta_region_eif.png"),
    title = "DESTA countries by region (entry into force)",
    ylab = "Unique countries"
  )
}

# --- ATOP ---

atop_zip <- "data/raw/atop/atop_5.1__.csv_.zip"
atop_dir <- "data/processed/atop"
if (file.exists(atop_zip)) {
  dir.create(atop_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(atop_zip, exdir = atop_dir)
  atop_sy_path <- file.path(atop_dir, "ATOP 5.1 (.csv)", "atop5_1sy.csv")

  if (file.exists(atop_sy_path)) {
    atop_sy <- read.csv(atop_sy_path, stringsAsFactors = FALSE)
    atop_sy <- dplyr::select(
      atop_sy,
      state,
      year,
      atopally,
      defense,
      offense,
      neutral,
      nonagg,
      consul
    )
    atop_sy$year <- suppressWarnings(as.integer(atop_sy$year))
    atop_sy$atopally <- suppressWarnings(as.integer(atop_sy$atopally))
    atop_sy$defense <- suppressWarnings(as.integer(atop_sy$defense))
    atop_sy$offense <- suppressWarnings(as.integer(atop_sy$offense))
    atop_sy$neutral <- suppressWarnings(as.integer(atop_sy$neutral))
    atop_sy$nonagg <- suppressWarnings(as.integer(atop_sy$nonagg))
    atop_sy$consul <- suppressWarnings(as.integer(atop_sy$consul))

    atop_sy$iso3 <- countrycode::countrycode(atop_sy$state, "cown", "iso3c", warn = FALSE)
    atop_sy$region <- map_region_iso3(atop_sy$iso3)

    atop_any <- atop_sy %>%
      dplyr::filter(!is.na(year) & year >= year_min & atopally > 0 & !is.na(iso3))

    atop_any_counts <- atop_any %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
      dplyr::arrange(year)

    write.csv(
      atop_any_counts,
      file = file.path(output_dir, "atop_country_counts_any.csv"),
      row.names = FALSE
    )

    plot_line(
      atop_any_counts,
      x_col = "year",
      y_col = "countries",
      file = file.path(output_dir, "fig_atop_countries_any.png"),
      title = "ATOP countries per year (any alliance)",
      ylab = "Unique countries"
    )

    type_cols <- c("defense", "offense", "neutral", "nonagg", "consul")
    type_list <- list()
    for (type_name in type_cols) {
      subset_df <- atop_sy %>%
        dplyr::filter(!is.na(year) & year >= year_min & .data[[type_name]] > 0 & !is.na(iso3))
      type_counts <- subset_df %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")
      type_counts$type <- type_name
      type_list[[type_name]] <- type_counts
    }

    atop_type_counts <- dplyr::bind_rows(type_list) %>%
      dplyr::arrange(type, year)

    write.csv(
      atop_type_counts,
      file = file.path(output_dir, "atop_country_counts_by_type.csv"),
      row.names = FALSE
    )

    plot_group_lines(
      df = data.frame(
        year = atop_type_counts$year,
        group = atop_type_counts$type,
        value = atop_type_counts$countries,
        stringsAsFactors = FALSE
      ),
      file = file.path(output_dir, "fig_atop_countries_by_type.png"),
      title = "ATOP countries per year by alliance type",
      ylab = "Unique countries"
    )

    atop_region_counts <- atop_any %>%
      dplyr::group_by(year, region) %>%
      dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

    write.csv(
      atop_region_counts,
      file = file.path(output_dir, "atop_region_counts_any.csv"),
      row.names = FALSE
    )

    plot_group_lines(
      df = data.frame(
        year = atop_region_counts$year,
        group = atop_region_counts$region,
        value = atop_region_counts$countries,
        stringsAsFactors = FALSE
      ),
      file = file.path(output_dir, "fig_atop_region_any.png"),
      title = "ATOP countries by region (any alliance)",
      ylab = "Unique countries"
    )

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      dataset = "ATOP (state-year)",
      rows = nrow(atop_sy),
      unique_countries = length(unique(atop_sy$iso3[!is.na(atop_sy$iso3)])),
      year_min = min(atop_sy$year, na.rm = TRUE),
      year_max = max(atop_sy$year, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}

# --- IEADB ---

ieadb_zip <- "data/raw/ieadb/all-csv-files.zip"
ieadb_dir <- "data/processed/ieadb"
if (file.exists(ieadb_zip)) {
  dir.create(ieadb_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(ieadb_zip, exdir = ieadb_dir)
  ieadb_members_path <- file.path(ieadb_dir, "db_members.csv")

  if (file.exists(ieadb_members_path)) {
    ieadb_members <- read.delim(
      ieadb_members_path,
      sep = ";",
      skip = 3,
      quote = '"',
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    )

    ieadb_members <- dplyr::select(
      ieadb_members,
      `Mitch ID`,
      `Treaty Name from Treaty Table`,
      `Country (official name)`,
      `Action Type`,
      Year,
      `Signature Date`,
      `Entry into force (EIF) Date`,
      `Agreement Type - Level 1`
    )

    ieadb_members$Year <- suppressWarnings(as.integer(ieadb_members$Year))
    ieadb_members$iso3 <- countrycode::countrycode(
      ieadb_members$`Country (official name)`,
      "country.name",
      "iso3c",
      warn = FALSE
    )
    ieadb_members$region <- map_region_iso3(ieadb_members$iso3)
    ieadb_members$country_id <- ifelse(
      is.na(ieadb_members$iso3),
      ieadb_members$`Country (official name)`,
      ieadb_members$iso3
    )

    action_lower <- tolower(ieadb_members$`Action Type`)
    ieadb_members$action_category <- dplyr::case_when(
      grepl("signature", action_lower) ~ "Signature",
      grepl("ratification", action_lower) ~ "Ratification/Accession",
      grepl("accession", action_lower) ~ "Ratification/Accession",
      grepl("entry into force", action_lower) ~ "Entry into force",
      TRUE ~ "Other"
    )

    ieadb_filtered <- ieadb_members %>%
      dplyr::filter(!is.na(Year) & Year >= year_min)

    ieadb_year_counts <- ieadb_filtered %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(actions = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(Year)

    write.csv(ieadb_year_counts, file = file.path(output_dir, "ieadb_year_counts.csv"), row.names = FALSE)

    png(file.path(output_dir, "fig_ieadb_actions_per_year.png"), width = 1000, height = 600)
    plot(
      ieadb_year_counts$Year,
      ieadb_year_counts$actions,
      type = "l",
      lwd = 2,
      col = "#7570b3",
      xlab = "Year",
      ylab = "Number of actions",
      main = "IEADB actions per year"
    )
    abline(v = year_min, col = "#999999", lty = 3)
    dev.off()

    action_counts <- ieadb_filtered %>%
      dplyr::group_by(`Action Type`) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(n))

    write.csv(action_counts, file = file.path(output_dir, "ieadb_action_type_counts.csv"), row.names = FALSE)
    write_latex_table(
      head(action_counts, 10),
      path = file.path(output_dir, "ieadb_action_type_counts.tex"),
      caption = "IEADB action type counts (top 10, 1990+)",
      label = "tab:ieadb_action_types"
    )

    ieadb_country_counts_any <- ieadb_filtered %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(countries = dplyr::n_distinct(country_id), .groups = "drop") %>%
      dplyr::arrange(Year)

    write.csv(
      ieadb_country_counts_any,
      file = file.path(output_dir, "ieadb_country_counts_any.csv"),
      row.names = FALSE
    )

    plot_line(
      ieadb_country_counts_any,
      x_col = "Year",
      y_col = "countries",
      file = file.path(output_dir, "fig_ieadb_countries_any.png"),
      title = "IEADB countries per year (any action)",
      ylab = "Unique countries"
    )

    ieadb_country_counts_action <- ieadb_filtered %>%
      dplyr::group_by(Year, action_category) %>%
      dplyr::summarise(countries = dplyr::n_distinct(country_id), .groups = "drop")

    write.csv(
      ieadb_country_counts_action,
      file = file.path(output_dir, "ieadb_country_counts_by_action.csv"),
      row.names = FALSE
    )

    plot_group_lines(
      df = data.frame(
        year = ieadb_country_counts_action$Year,
        group = ieadb_country_counts_action$action_category,
        value = ieadb_country_counts_action$countries,
        stringsAsFactors = FALSE
      ),
      file = file.path(output_dir, "fig_ieadb_countries_by_action.png"),
      title = "IEADB countries per year by action category",
      ylab = "Unique countries"
    )

    ieadb_region_counts <- ieadb_filtered %>%
      dplyr::group_by(Year, region) %>%
      dplyr::summarise(countries = dplyr::n_distinct(country_id), .groups = "drop")

    write.csv(
      ieadb_region_counts,
      file = file.path(output_dir, "ieadb_region_counts_any.csv"),
      row.names = FALSE
    )

    plot_group_lines(
      df = data.frame(
        year = ieadb_region_counts$Year,
        group = ieadb_region_counts$region,
        value = ieadb_region_counts$countries,
        stringsAsFactors = FALSE
      ),
      file = file.path(output_dir, "fig_ieadb_region_any.png"),
      title = "IEADB countries by region (any action)",
      ylab = "Unique countries"
    )

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      dataset = "IEADB (members)",
      rows = nrow(ieadb_members),
      unique_countries = length(unique(ieadb_members$country_id)),
      year_min = min(ieadb_members$Year, na.rm = TRUE),
      year_max = max(ieadb_members$Year, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}

# --- WTO RTA ---

wto_path <- "data/raw/wto_rta/WTO_RTA_AllRTAs.xlsx"
if (file.exists(wto_path)) {
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
    Region,
    `RTA Composition`,
    Status
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
    rta_region = Region,
    rta_composition = `RTA Composition`,
    status = Status
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

  wto_agreements_sig <- wto %>%
    dplyr::filter(!is.na(year_signature) & year_signature >= year_min) %>%
    dplyr::group_by(year_signature) %>%
    dplyr::summarise(agreements = dplyr::n_distinct(rta_id), .groups = "drop") %>%
    dplyr::arrange(year_signature)

  write.csv(wto_agreements_sig, file = file.path(output_dir, "wto_rta_agreements_signature.csv"), row.names = FALSE)
  plot_line(
    wto_agreements_sig,
    x_col = "year_signature",
    y_col = "agreements",
    file = file.path(output_dir, "fig_wto_rta_agreements_signature.png"),
    title = "WTO RTA agreements per year (signature)",
    ylab = "Agreements"
  )

  wto_agreements_eif <- wto %>%
    dplyr::filter(!is.na(year_eif) & year_eif >= year_min) %>%
    dplyr::group_by(year_eif) %>%
    dplyr::summarise(agreements = dplyr::n_distinct(rta_id), .groups = "drop") %>%
    dplyr::arrange(year_eif)

  write.csv(wto_agreements_eif, file = file.path(output_dir, "wto_rta_agreements_eif.csv"), row.names = FALSE)
  plot_line(
    wto_agreements_eif,
    x_col = "year_eif",
    y_col = "agreements",
    file = file.path(output_dir, "fig_wto_rta_agreements_eif.png"),
    title = "WTO RTA agreements per year (entry into force)",
    ylab = "Agreements"
  )

  wto$signatories <- wto$signatories_current
  missing_sig <- is.na(wto$signatories) | wto$signatories == ""
  wto$signatories[missing_sig] <- wto$signatories_original[missing_sig]

  wto_sign_sig <- expand_signatories(wto, "year_signature", "signatories")
  wto_sign_eif <- expand_signatories(wto, "year_eif", "signatories")

  wto_sign_sig$iso3 <- map_iso3_from_name(wto_sign_sig$signatory)
  wto_sign_sig$entity_type <- ifelse(is.na(wto_sign_sig$iso3), "Non-country/REIO", "Country")
  wto_sign_sig$region <- map_region_iso3(wto_sign_sig$iso3)

  wto_sign_eif$iso3 <- map_iso3_from_name(wto_sign_eif$signatory)
  wto_sign_eif$entity_type <- ifelse(is.na(wto_sign_eif$iso3), "Non-country/REIO", "Country")
  wto_sign_eif$region <- map_region_iso3(wto_sign_eif$iso3)

  wto_excluded <- dplyr::bind_rows(
    wto_sign_sig %>% dplyr::filter(entity_type != "Country"),
    wto_sign_eif %>% dplyr::filter(entity_type != "Country")
  ) %>%
    dplyr::group_by(signatory) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n))

  write.csv(wto_excluded, file = file.path(output_dir, "wto_rta_excluded_entities.csv"), row.names = FALSE)

  wto_sig_countries <- wto_sign_sig %>%
    dplyr::filter(entity_type == "Country") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
    dplyr::arrange(year)

  write.csv(wto_sig_countries, file = file.path(output_dir, "wto_rta_countries_signature.csv"), row.names = FALSE)
  plot_line(
    wto_sig_countries,
    x_col = "year",
    y_col = "countries",
    file = file.path(output_dir, "fig_wto_rta_countries_signature.png"),
    title = "WTO RTA countries per year (signature)",
    ylab = "Unique countries"
  )

  wto_sig_region <- wto_sign_sig %>%
    dplyr::filter(entity_type == "Country") %>%
    dplyr::group_by(year, region) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

  write.csv(wto_sig_region, file = file.path(output_dir, "wto_rta_region_signature.csv"), row.names = FALSE)
  plot_group_lines(
    df = data.frame(
      year = wto_sig_region$year,
      group = wto_sig_region$region,
      value = wto_sig_region$countries,
      stringsAsFactors = FALSE
    ),
    file = file.path(output_dir, "fig_wto_rta_region_signature.png"),
    title = "WTO RTA countries by region (signature)",
    ylab = "Unique countries"
  )

  wto_eif_countries <- wto_sign_eif %>%
    dplyr::filter(entity_type == "Country") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
    dplyr::arrange(year)

  write.csv(wto_eif_countries, file = file.path(output_dir, "wto_rta_countries_eif.csv"), row.names = FALSE)
  plot_line(
    wto_eif_countries,
    x_col = "year",
    y_col = "countries",
    file = file.path(output_dir, "fig_wto_rta_countries_eif.png"),
    title = "WTO RTA countries per year (entry into force)",
    ylab = "Unique countries"
  )

  wto_eif_region <- wto_sign_eif %>%
    dplyr::filter(entity_type == "Country") %>%
    dplyr::group_by(year, region) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

  write.csv(wto_eif_region, file = file.path(output_dir, "wto_rta_region_eif.csv"), row.names = FALSE)
  plot_group_lines(
    df = data.frame(
      year = wto_eif_region$year,
      group = wto_eif_region$region,
      value = wto_eif_region$countries,
      stringsAsFactors = FALSE
    ),
    file = file.path(output_dir, "fig_wto_rta_region_eif.png"),
    title = "WTO RTA countries by region (entry into force)",
    ylab = "Unique countries"
  )

  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    dataset = "WTO RTA",
    rows = nrow(wto),
    unique_countries = length(unique(wto_sign_sig$iso3[!is.na(wto_sign_sig$iso3)])),
    year_min = min(wto$year_signature, na.rm = TRUE),
    year_max = max(wto$year_signature, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# --- UNCTAD IIA ---

unctad_path <- "data/raw/unctad_iia/unctad_iia_country_treaties.csv"
if (file.exists(unctad_path)) {
  unctad_raw <- read.csv(unctad_path, stringsAsFactors = FALSE)
  unctad <- dplyr::select(
    unctad_raw,
    country_code,
    country_name,
    date_of_signature,
    date_of_entry_into_force,
    termination_date,
    status,
    type,
    table_name,
    treaty_url,
    full_title
  )

  unctad$iso3 <- countrycode::countrycode(unctad$country_code, "iso2c", "iso3c", warn = FALSE)
  unctad$entity_type <- ifelse(is.na(unctad$iso3), "Non-country/REIO", "Country")
  unctad$region <- map_region_iso3(unctad$iso3)

  unctad_excluded <- unctad %>%
    dplyr::filter(entity_type != "Country") %>%
    dplyr::distinct(country_code, country_name, entity_type)

  write.csv(unctad_excluded, file = file.path(output_dir, "unctad_iia_excluded_entities.csv"), row.names = FALSE)

  unctad <- unctad %>%
    dplyr::filter(entity_type == "Country")

  unctad$signature_year <- year_from_date(unctad$date_of_signature)
  unctad$eif_year <- year_from_date(unctad$date_of_entry_into_force)

  unctad_treaties_sig <- unctad %>%
    dplyr::filter(!is.na(signature_year) & signature_year >= year_min) %>%
    dplyr::group_by(signature_year) %>%
    dplyr::summarise(treaties = dplyr::n_distinct(treaty_url), .groups = "drop") %>%
    dplyr::arrange(signature_year)

  write.csv(unctad_treaties_sig, file = file.path(output_dir, "unctad_iia_treaties_signature.csv"), row.names = FALSE)
  plot_line(
    unctad_treaties_sig,
    x_col = "signature_year",
    y_col = "treaties",
    file = file.path(output_dir, "fig_unctad_iia_treaties_signature.png"),
    title = "UNCTAD IIA treaties per year (signature)",
    ylab = "Treaties"
  )

  unctad_treaties_eif <- unctad %>%
    dplyr::filter(!is.na(eif_year) & eif_year >= year_min) %>%
    dplyr::group_by(eif_year) %>%
    dplyr::summarise(treaties = dplyr::n_distinct(treaty_url), .groups = "drop") %>%
    dplyr::arrange(eif_year)

  write.csv(unctad_treaties_eif, file = file.path(output_dir, "unctad_iia_treaties_eif.csv"), row.names = FALSE)
  plot_line(
    unctad_treaties_eif,
    x_col = "eif_year",
    y_col = "treaties",
    file = file.path(output_dir, "fig_unctad_iia_treaties_eif.png"),
    title = "UNCTAD IIA treaties per year (entry into force)",
    ylab = "Treaties"
  )

  unctad_countries_sig <- unctad %>%
    dplyr::filter(!is.na(signature_year) & signature_year >= year_min) %>%
    dplyr::group_by(signature_year) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
    dplyr::arrange(signature_year)

  write.csv(unctad_countries_sig, file = file.path(output_dir, "unctad_iia_countries_signature.csv"), row.names = FALSE)
  plot_line(
    unctad_countries_sig,
    x_col = "signature_year",
    y_col = "countries",
    file = file.path(output_dir, "fig_unctad_iia_countries_signature.png"),
    title = "UNCTAD IIA countries per year (signature)",
    ylab = "Unique countries"
  )

  unctad_countries_eif <- unctad %>%
    dplyr::filter(!is.na(eif_year) & eif_year >= year_min) %>%
    dplyr::group_by(eif_year) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop") %>%
    dplyr::arrange(eif_year)

  write.csv(unctad_countries_eif, file = file.path(output_dir, "unctad_iia_countries_eif.csv"), row.names = FALSE)
  plot_line(
    unctad_countries_eif,
    x_col = "eif_year",
    y_col = "countries",
    file = file.path(output_dir, "fig_unctad_iia_countries_eif.png"),
    title = "UNCTAD IIA countries per year (entry into force)",
    ylab = "Unique countries"
  )

  unctad_region_sig <- unctad %>%
    dplyr::filter(!is.na(signature_year) & signature_year >= year_min) %>%
    dplyr::group_by(signature_year, region) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

  write.csv(unctad_region_sig, file = file.path(output_dir, "unctad_iia_region_signature.csv"), row.names = FALSE)
  plot_group_lines(
    df = data.frame(
      year = unctad_region_sig$signature_year,
      group = unctad_region_sig$region,
      value = unctad_region_sig$countries,
      stringsAsFactors = FALSE
    ),
    file = file.path(output_dir, "fig_unctad_iia_region_signature.png"),
    title = "UNCTAD IIA countries by region (signature)",
    ylab = "Unique countries"
  )

  unctad_region_eif <- unctad %>%
    dplyr::filter(!is.na(eif_year) & eif_year >= year_min) %>%
    dplyr::group_by(eif_year, region) %>%
    dplyr::summarise(countries = dplyr::n_distinct(iso3), .groups = "drop")

  write.csv(unctad_region_eif, file = file.path(output_dir, "unctad_iia_region_eif.csv"), row.names = FALSE)
  plot_group_lines(
    df = data.frame(
      year = unctad_region_eif$eif_year,
      group = unctad_region_eif$region,
      value = unctad_region_eif$countries,
      stringsAsFactors = FALSE
    ),
    file = file.path(output_dir, "fig_unctad_iia_region_eif.png"),
    title = "UNCTAD IIA countries by region (entry into force)",
    ylab = "Unique countries"
  )

  unctad_status <- unctad %>%
    dplyr::filter(!is.na(signature_year) & signature_year >= year_min) %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n))

  write.csv(unctad_status, file = file.path(output_dir, "unctad_iia_status_counts.csv"), row.names = FALSE)
  write_latex_table(
    head(unctad_status, 10),
    path = file.path(output_dir, "unctad_iia_status_counts.tex"),
    caption = "UNCTAD IIA status counts (top 10, 1990+)",
    label = "tab:unctad_status_counts"
  )

  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    dataset = "UNCTAD IIA",
    rows = nrow(unctad),
    unique_countries = length(unique(unctad$iso3)),
    year_min = min(unctad$signature_year, na.rm = TRUE),
    year_max = max(unctad$signature_year, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# --- Summary table ---

if (length(summary_rows) > 0) {
  summary_df <- do.call(rbind, summary_rows)
  write.csv(summary_df, file = file.path(output_dir, "summary_overall.csv"), row.names = FALSE)
  write_latex_table(
    summary_df,
    path = file.path(output_dir, "summary_overall.tex"),
    caption = "Summary of available datasets",
    label = "tab:summary_overall"
  )
}

# --- Excluded entities summary ---

wto_excluded_path <- file.path(output_dir, "wto_rta_excluded_entities.csv")
unctad_excluded_path <- file.path(output_dir, "unctad_iia_excluded_entities.csv")

excluded_summary_rows <- list()

if (file.exists(wto_excluded_path)) {
  wto_excl <- read.csv(wto_excluded_path, stringsAsFactors = FALSE)
  wto_excl <- dplyr::select(wto_excl, signatory, n)
  excluded_summary_rows[[length(excluded_summary_rows) + 1]] <- data.frame(
    dataset = "WTO RTA",
    excluded_entities = nrow(wto_excl),
    excluded_rows = sum(wto_excl$n, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

if (file.exists(unctad_excluded_path)) {
  unctad_excl <- read.csv(unctad_excluded_path, stringsAsFactors = FALSE)
  unctad_excl <- dplyr::select(unctad_excl, country_code, country_name)
  excluded_summary_rows[[length(excluded_summary_rows) + 1]] <- data.frame(
    dataset = "UNCTAD IIA",
    excluded_entities = nrow(unctad_excl),
    excluded_rows = nrow(unctad_excl),
    stringsAsFactors = FALSE
  )
}

if (length(excluded_summary_rows) > 0) {
  excluded_summary <- do.call(rbind, excluded_summary_rows)
  write.csv(excluded_summary, file = file.path(output_dir, "excluded_summary.csv"), row.names = FALSE)
  write_latex_table(
    excluded_summary,
    path = file.path(output_dir, "excluded_summary.tex"),
    caption = "Excluded non-country entities (summary)",
    label = "tab:excluded_entities"
  )
}

# --- Excluded entities list ---

excluded_list_rows <- list()

if (file.exists(wto_excluded_path)) {
  wto_excl <- read.csv(wto_excluded_path, stringsAsFactors = FALSE)
  wto_excl <- dplyr::select(wto_excl, signatory)
  wto_excl <- wto_excl %>% dplyr::distinct(signatory)
  excluded_list_rows[[length(excluded_list_rows) + 1]] <- data.frame(
    dataset = "WTO RTA",
    entity = wto_excl$signatory,
    stringsAsFactors = FALSE
  )
}

if (file.exists(unctad_excluded_path)) {
  unctad_excl <- read.csv(unctad_excluded_path, stringsAsFactors = FALSE)
  unctad_excl <- dplyr::select(unctad_excl, country_name)
  unctad_excl <- unctad_excl %>% dplyr::distinct(country_name)
  excluded_list_rows[[length(excluded_list_rows) + 1]] <- data.frame(
    dataset = "UNCTAD IIA",
    entity = unctad_excl$country_name,
    stringsAsFactors = FALSE
  )
}

if (length(excluded_list_rows) > 0) {
  excluded_list <- do.call(rbind, excluded_list_rows) %>%
    dplyr::arrange(dataset, entity)
  write.csv(excluded_list, file = file.path(output_dir, "excluded_entities_list.csv"), row.names = FALSE)
  write_latex_table(
    excluded_list,
    path = file.path(output_dir, "excluded_entities_list.tex"),
    caption = "Excluded non-country entities (list)",
    label = "tab:excluded_entities_list"
  )
}
