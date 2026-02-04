#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(xml2)
  library(readr)
})

parse_args <- function(args) {
  out <- list(
    reporter_iso3 = NA_character_,
    year = NA_character_,
    raw_root = "data/raw/tariffs/wits_trn",
    out_root = "data/processed/tariffs/wits_trn",
    validation_root = "outputs/validation"
  )
  if (length(args) == 0) {
    return(out)
  }
  if (length(args) %% 2 != 0) {
    stop("Arguments must be provided as --key value pairs.")
  }
  for (i in seq(1, length(args), by = 2)) {
    key <- gsub("^--", "", args[i])
    key <- gsub("-", "_", key)
    out[[key]] <- args[i + 1]
  }
  out
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

parse_country_metadata <- function(path) {
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  nodes <- xml2::xml_find_all(doc, ".//wits:country", ns)
  iso3 <- vapply(
    nodes,
    function(n) xml2::xml_text(xml2::xml_find_first(n, "./wits:iso3Code", ns)),
    character(1)
  )
  name <- vapply(
    nodes,
    function(n) xml2::xml_text(xml2::xml_find_first(n, "./wits:name", ns)),
    character(1)
  )
  tibble(
    countrycode = xml2::xml_attr(nodes, "countrycode"),
    iso3 = iso3,
    name = name,
    isreporter = xml2::xml_attr(nodes, "isreporter"),
    ispartner = xml2::xml_attr(nodes, "ispartner"),
    isgroup = xml2::xml_attr(nodes, "isgroup"),
    grouptype = xml2::xml_attr(nodes, "grouptype")
  )
}

map_attr_value <- function(values, idx) {
  if (is.null(idx) || is.na(idx)) {
    return(NA_character_)
  }
  if (is.null(values) || length(values) == 0) {
    return(as.character(idx))
  }
  pos <- as.integer(idx) + 1
  if (pos <= 0 || pos > length(values)) {
    return(NA_character_)
  }
  values[[pos]]
}

parse_sdmx_tariff_json <- function(path) {
  raw_text <- readr::read_file(path)
  # WITS sometimes encodes missing values as "[," which is invalid JSON.
  raw_text <- gsub("\\[,", "[null,", raw_text)
  obj <- jsonlite::fromJSON(raw_text, simplifyVector = FALSE)
  file_name <- basename(path)
  partner_override <- str_match(file_name, "^tariffs_partner_(.+)\\.json$")[, 2]
  dims <- obj$structure$dimensions$series
  obs_dims <- obj$structure$dimensions$observation
  obs_attrs <- obj$structure$attributes$observation

  dim_names <- vapply(dims, function(d) d$id, character(1))
  dim_values <- lapply(dims, function(d) vapply(d$values, function(v) v$id, character(1)))
  dim_labels <- lapply(dims, function(d) vapply(d$values, function(v) v$name %||% "", character(1)))

  obs_dim_names <- vapply(obs_dims, function(d) d$id, character(1))
  obs_dim_values <- lapply(obs_dims, function(d) vapply(d$values, function(v) v$id, character(1)))

  attr_names <- vapply(obs_attrs, function(a) a$id, character(1))
  attr_values <- lapply(obs_attrs, function(a) {
    if (is.null(a$values)) {
      return(NULL)
    }
    vapply(a$values, function(v) v$id %||% v$name %||% "", character(1))
  })

  series_list <- obj$dataSets[[1]]$series
  if (length(series_list) == 0) {
    return(tibble())
  }

  build_dim_order <- function(dim_values, series_keys) {
    n_dims <- length(dim_values)
    dim_lengths <- vapply(dim_values, length, integer(1))
    idx_mat <- do.call(rbind, lapply(series_keys, function(k) as.integer(strsplit(k, ":", fixed = TRUE)[[1]])))
    max_idx <- apply(idx_mat, 2, max)
    pos_order <- order(max_idx, decreasing = TRUE)
    dim_order <- order(dim_lengths, decreasing = TRUE)
    used <- rep(FALSE, n_dims)
    pos_to_dim <- integer(n_dims)
    for (p in pos_order) {
      chosen <- NA_integer_
      for (d in dim_order) {
        if (used[d]) {
          next
        }
        if ((dim_lengths[d] - 1) >= max_idx[p]) {
          chosen <- d
          break
        }
      }
      if (is.na(chosen)) {
        chosen <- setdiff(seq_len(n_dims), which(used))[1]
      }
      pos_to_dim[p] <- chosen
      used[chosen] <- TRUE
    }
    pos_to_dim
  }

  dim_order_idx <- build_dim_order(dim_values, names(series_list))
  dim_names <- dim_names[dim_order_idx]
  dim_values <- dim_values[dim_order_idx]
  dim_labels <- dim_labels[dim_order_idx]

  rows <- vector("list", length(series_list))
  row_idx <- 1

  safe_dim <- function(vals, idx) {
    if (is.null(idx) || is.na(idx)) {
      return(NA_character_)
    }
    pos <- as.integer(idx) + 1
    if (pos <= 0 || pos > length(vals)) {
      return(NA_character_)
    }
    vals[[pos]]
  }

  for (series_key in names(series_list)) {
    series <- series_list[[series_key]]
    series_idx <- as.integer(strsplit(series_key, ":", fixed = TRUE)[[1]])
    series_vals <- mapply(safe_dim, dim_values, series_idx, SIMPLIFY = FALSE)
    names(series_vals) <- dim_names
    series_labels <- mapply(safe_dim, dim_labels, series_idx, SIMPLIFY = FALSE)
    names(series_labels) <- paste0(dim_names, "_LABEL")

    observations <- series$observations
    if (length(observations) == 0) {
      next
    }
    for (obs_key in names(observations)) {
      obs <- observations[[obs_key]]
      if (is.list(obs)) {
        obs <- lapply(obs, function(x) if (is.null(x)) NA else x)
        obs <- unlist(obs, use.names = FALSE)
      } else {
        obs <- as.vector(obs)
      }
      obs_idx <- as.integer(obs_key)
      time_pos <- which(obs_dim_names == "TIME_PERIOD")
      time_val <- NA_character_
      if (length(time_pos) == 1 && length(obs_dim_values[[time_pos]]) >= obs_idx + 1) {
        time_val <- obs_dim_values[[time_pos]][[obs_idx + 1]]
      }

      value <- obs[[1]]
      attr_map <- vector("list", length(attr_names))
      for (i in seq_along(attr_names)) {
        raw_idx <- if (length(obs) >= i + 1) obs[[i + 1]] else NA
        attr_map[[i]] <- map_attr_value(attr_values[[i]], raw_idx)
      }
      names(attr_map) <- attr_names

      row <- c(series_vals, series_labels, TIME_PERIOD = time_val, VALUE = value, attr_map, PARTNER_OVERRIDE = partner_override)
      rows[[row_idx]] <- row
      row_idx <- row_idx + 1
    }
  }

  rows <- rows[seq_len(row_idx - 1)]
  if (length(rows) == 0) {
    return(tibble())
  }
  bind_rows(lapply(rows, tibble::as_tibble_row))
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  if (is.na(args$reporter_iso3) || is.na(args$year)) {
    stop("Usage: --reporter-iso3 ISO3 --year YYYY")
  }

  reporter_iso3 <- toupper(args$reporter_iso3)
  year <- as.integer(args$year)

  raw_dir <- file.path(args$raw_root, reporter_iso3, year)
  out_dir <- file.path(args$out_root, reporter_iso3, year)
  validation_dir <- args$validation_root
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  json_files <- list.files(raw_dir, pattern = "^tariffs_partner_.*\\.json$", full.names = TRUE)
  if (length(json_files) == 0) {
    stop("No tariff JSON files found in raw directory.")
  }

  meta_path <- file.path(raw_dir, "countries_metadata.xml")
  if (!file.exists(meta_path)) {
    stop("Missing countries_metadata.xml in raw directory.")
  }
  meta <- parse_country_metadata(meta_path)

  data_list <- lapply(json_files, parse_sdmx_tariff_json)
  data <- bind_rows(data_list)

  if (nrow(data) == 0) {
    stop("Parsed data is empty.")
  }

  data <- data %>%
    rename(
      reporter_code = REPORTER,
      partner_code = PARTNER,
      partner_override = PARTNER_OVERRIDE,
      product_code = PRODUCTCODE,
      datatype = DATATYPE,
      freq = FREQ,
      year = TIME_PERIOD,
      tariff_value = VALUE,
      nomencode = NOMENCODE,
      tariff_type = TARIFFTYPE,
      obs_value_measure = OBS_VALUE_MEASURE
    ) %>%
    mutate(
      reporter_code = as.character(reporter_code),
      partner_code = as.character(partner_code),
      partner_override = as.character(partner_override),
      product_code = as.character(product_code),
      hs6_code = product_code,
      hs4_code_raw = substr(product_code, 1, 4),
      year = as.integer(year),
      tariff_value = as.numeric(tariff_value),
      nbr_na_lines = as.numeric(NBR_NA_LINES),
      ad_valorem_only = if_else(!is.na(nbr_na_lines) & nbr_na_lines == 0, TRUE, FALSE),
      partner_code = if_else(is.na(partner_code) | partner_code == "", partner_override, partner_code)
    )

  meta_reporter <- meta %>%
    rename(
      reporter_code = countrycode,
      reporter_iso3 = iso3,
      reporter_name = name,
      reporter_is_group = isgroup,
      reporter_group_type = grouptype
    ) %>%
    dplyr::select(reporter_code, reporter_iso3, reporter_name, reporter_is_group, reporter_group_type)

  meta_partner <- meta %>%
    rename(
      partner_code = countrycode,
      partner_iso3 = iso3,
      partner_name = name,
      partner_is_group = isgroup,
      partner_group_type = grouptype
    ) %>%
    dplyr::select(partner_code, partner_iso3, partner_name, partner_is_group, partner_group_type)

  data <- data %>%
    left_join(meta_reporter, by = "reporter_code") %>%
    left_join(meta_partner, by = "partner_code") %>%
    mutate(
      source = "WITS/TRAINS"
    )

  out_all <- file.path(out_dir, "line_level_all.csv")
  out_adval <- file.path(out_dir, "line_level_ad_valorem.csv")

  readr::write_csv(data, out_all)
  readr::write_csv(data %>% filter(ad_valorem_only), out_adval)

  raw_bytes <- sum(file.info(json_files)$size, na.rm = TRUE)
  summary_tbl <- tibble(
    reporter_iso3 = reporter_iso3,
    year = year,
    partners = n_distinct(data$partner_code),
    partner_groups = n_distinct(data$partner_code[data$partner_is_group == "Yes"]),
    products = n_distinct(data$product_code),
    rows_total = nrow(data),
    rows_ad_valorem = nrow(data %>% filter(ad_valorem_only)),
    share_ad_valorem = if_else(rows_total > 0, rows_ad_valorem / rows_total, NA_real_),
    raw_bytes = raw_bytes,
    raw_megabytes = raw_bytes / (1024^2)
  )

  out_summary <- file.path(validation_dir, sprintf("wits_tariff_pilot_summary_%s_%s.csv", reporter_iso3, year))
  readr::write_csv(summary_tbl, out_summary)

  message("Wrote: ", out_all)
  message("Wrote: ", out_adval)
  message("Wrote: ", out_summary)
}

main()
