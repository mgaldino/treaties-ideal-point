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
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Please install it with install.packages('tibble').")
  }
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})

out_dir <- "outputs/v8_validation"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

safe_cor <- function(x, y) {
  ok <- stats::complete.cases(x, y)
  if (sum(ok) < 4) {
    return(NA_real_)
  }
  r <- suppressWarnings(stats::cor(x[ok], y[ok]))
  if (!is.finite(r)) {
    return(NA_real_)
  }
  r
}

skew_manual <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) {
    return(NA_real_)
  }
  mean(((x - m) / s)^3)
}

compute_skewness <- function(x) {
  if (requireNamespace("e1071", quietly = TRUE)) {
    return(suppressWarnings(e1071::skewness(x, na.rm = TRUE, type = 1)))
  }
  skew_manual(x)
}

normalize_period <- function(period) {
  # Returns a list with period_id (int) and period_label (chr).
  if (is.numeric(period) || is.integer(period)) {
    period_id <- as.integer(period)
    period_label <- dplyr::case_when(
      period_id == 1L ~ "1990-1994",
      period_id == 2L ~ "1995-1999",
      period_id == 3L ~ "2000-2004",
      period_id == 4L ~ "2005-2009",
      period_id == 5L ~ "2010-2014",
      period_id == 6L ~ "2015-2019",
      period_id == 7L ~ "2020-2024",
      TRUE ~ NA_character_
    )
    return(list(period_id = period_id, period_label = period_label))
  }

  p <- as.character(period)
  period_id <- dplyr::case_when(
    p == "1990-1994" ~ 1L,
    p == "1995-1999" ~ 2L,
    p == "2000-2004" ~ 3L,
    p == "2005-2009" ~ 4L,
    p == "2010-2014" ~ 5L,
    p %in% c("2015-2019", "2015-2018") ~ 6L,
    p == "2020-2024" ~ 7L,
    TRUE ~ NA_integer_
  )
  return(list(period_id = period_id, period_label = p))
}

load_unga <- function(path) {
  if (!file.exists(path)) {
    stop("Missing UNGA period file: ", path)
  }
  u <- readr::read_csv(path, show_col_types = FALSE)

  if ("iso3" %in% names(u)) {
    u <- u %>% dplyr::rename(iso3c = iso3)
  } else if ("iso3c" %in% names(u)) {
    u <- u %>% dplyr::rename(iso3c = iso3c)
  } else {
    stop("UNGA file missing iso3/iso3c column: ", path)
  }
  u$iso3c <- toupper(trimws(as.character(u$iso3c)))

  if (!("unga_ideal_point" %in% names(u))) {
    stop("UNGA file missing unga_ideal_point column: ", path)
  }
  if (!("period" %in% names(u))) {
    stop("UNGA file missing period column: ", path)
  }

  per <- normalize_period(u$period)
  u <- u %>%
    dplyr::mutate(
      period = per$period_id,
      period_label = per$period_label
    ) %>%
    dplyr::filter(!is.na(period), !is.na(iso3c)) %>%
    dplyr::select(iso3c, period, unga_ideal_point)

  u
}

extract_country_period <- function(res, domain) {
  if (!is.list(res) || is.null(res$ideal_points)) {
    stop("Unexpected results structure for domain '", domain, "': missing $ideal_points")
  }
  ip <- res$ideal_points
  if (!is.array(ip) || length(dim(ip)) != 3L) {
    stop("Unexpected $ideal_points shape for domain '", domain, "' (expected 3D array).")
  }
  if (is.null(res$country_codes) || is.null(res$period_labels)) {
    stop("Unexpected results structure for domain '", domain, "': missing $country_codes or $period_labels")
  }

  n_c <- dim(ip)[1]
  n_d <- dim(ip)[2]
  n_p <- dim(ip)[3]
  if (n_d < 2) {
    stop("ideal_points has <2 dimensions for domain '", domain, "'; expected dim1 and dim2.")
  }

  countries <- as.character(res$country_codes)
  period_labels <- as.character(res$period_labels)
  if (length(countries) != n_c) {
    stop("Length mismatch: country_codes vs ideal_points[,,] for domain '", domain, "'.")
  }
  if (length(period_labels) != n_p) {
    stop("Length mismatch: period_labels vs ideal_points[,,] for domain '", domain, "'.")
  }

  grid <- tidyr::expand_grid(country_idx = seq_len(n_c), period_idx = seq_len(n_p))
  grid <- grid %>%
    dplyr::mutate(
      domain = domain,
      iso3c = countries[country_idx],
      period_label = period_labels[period_idx]
    )

  per <- normalize_period(grid$period_label)
  grid <- grid %>%
    dplyr::mutate(
      period = per$period_id,
      dim1 = ip[cbind(country_idx, 1L, period_idx)],
      dim2 = ip[cbind(country_idx, 2L, period_idx)]
    ) %>%
    dplyr::filter(!is.na(period), !is.na(iso3c)) %>%
    dplyr::select(domain, iso3c, period, period_label, dim1, dim2)

  grid
}

load_vdem_period_inline <- function(in_path) {
  if (!file.exists(in_path)) {
    return(NULL)
  }
  v <- readRDS(in_path)
  if ("country_text_id" %in% names(v)) {
    v <- v %>% dplyr::rename(iso3c = country_text_id)
  } else if ("iso3c" %in% names(v)) {
    v <- v %>% dplyr::rename(iso3c = iso3c)
  } else if ("iso3" %in% names(v)) {
    v <- v %>% dplyr::rename(iso3c = iso3)
  }

  if (!("iso3c" %in% names(v)) || !("year" %in% names(v)) || !("v2x_libdem" %in% names(v))) {
    return(NULL)
  }

  v$iso3c <- as.character(v$iso3c)

  v %>%
    dplyr::mutate(
      year = as.integer(year),
      period = dplyr::case_when(
        year >= 1990L & year <= 1994L ~ 1L,
        year >= 1995L & year <= 1999L ~ 2L,
        year >= 2000L & year <= 2004L ~ 3L,
        year >= 2005L & year <= 2009L ~ 4L,
        year >= 2010L & year <= 2014L ~ 5L,
        year >= 2015L & year <= 2019L ~ 6L,
        year >= 2020L & year <= 2024L ~ 7L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(period), !is.na(iso3c)) %>%
    dplyr::group_by(iso3c, period) %>%
    dplyr::summarise(
      vdem_libdem = mean(v2x_libdem, na.rm = TRUE),
      n_years = sum(!is.na(v2x_libdem)),
      year_min = min(year, na.rm = TRUE),
      year_max = max(year, na.rm = TRUE),
      .groups = "drop"
    )
}

v8_dir <- "outputs/v8_extended"
if (!dir.exists(v8_dir)) {
  stop("Missing directory: ", v8_dir)
}

v8_domains <- c("arms_control", "environment", "human_rights", "intellectual_property", "investment")
v8_files <- file.path(v8_dir, paste0(v8_domains, "_results.rds"))
missing_v8 <- v8_files[!file.exists(v8_files)]
if (length(missing_v8) > 0) {
  stop("Missing V8 results files: ", paste(missing_v8, collapse = ", "))
}

security_file <- "outputs/v7_country_anchors/security_results.rds"
if (!file.exists(security_file)) {
  stop("Missing security results file: ", security_file)
}

unga <- load_unga("data/processed/unga_ideal_points_period_v8.csv")
vdem_period <- load_vdem_period_inline("data/raw/vdem/vdem_country_year_v14.rds")

all_corr <- list()
all_trends <- list()
issues <- list()

# Process V8 extended domains.
for (i in seq_along(v8_domains)) {
  domain <- v8_domains[i]
  res <- readRDS(v8_files[i])

  ip <- extract_country_period(res, domain)

  trends <- ip %>%
    dplyr::group_by(domain, period, period_label) %>%
    dplyr::summarise(
      mean_dim1 = mean(dim1, na.rm = TRUE),
      sd_dim1 = stats::sd(dim1, na.rm = TRUE),
      skew_dim1 = compute_skewness(dim1),
      mean_dim2 = mean(dim2, na.rm = TRUE),
      sd_dim2 = stats::sd(dim2, na.rm = TRUE),
      .groups = "drop"
    )
  all_trends[[domain]] <- trends

  merged <- ip %>%
    dplyr::inner_join(unga, by = c("iso3c", "period")) %>%
    dplyr::select(domain, iso3c, period, period_label, dim1, dim2, unga_ideal_point)

  if (nrow(merged) == 0) {
    issues[[domain]] <- "No overlap after merging with UNGA (iso3c, period)."
    next
  }

  if (!is.null(vdem_period)) {
    merged <- merged %>%
      dplyr::left_join(vdem_period %>% dplyr::select(iso3c, period, vdem_libdem), by = c("iso3c", "period"))
  } else {
    merged <- merged %>% dplyr::mutate(vdem_libdem = NA_real_)
  }

  corr_by_period <- merged %>%
    dplyr::group_by(domain, period, period_label) %>%
    dplyr::summarise(
      r_unga_dim1 = safe_cor(dim1, unga_ideal_point),
      r_vdem_dim1 = safe_cor(dim1, vdem_libdem),
      r_unga_dim2 = safe_cor(dim2, unga_ideal_point),
      r_vdem_dim2 = safe_cor(dim2, vdem_libdem),
      n = sum(stats::complete.cases(dim1, unga_ideal_point)),
      .groups = "drop"
    )

  corr_overall <- merged %>%
    dplyr::summarise(
      domain = .env$domain,
      period = 0L,
      period_label = "overall",
      r_unga_dim1 = safe_cor(dim1, unga_ideal_point),
      r_vdem_dim1 = safe_cor(dim1, vdem_libdem),
      r_unga_dim2 = safe_cor(dim2, unga_ideal_point),
      r_vdem_dim2 = safe_cor(dim2, vdem_libdem),
      n = sum(stats::complete.cases(dim1, unga_ideal_point))
    )

  all_corr[[domain]] <- dplyr::bind_rows(corr_overall, corr_by_period) %>%
    dplyr::select(domain, period, r_unga_dim1, r_vdem_dim1, r_unga_dim2, r_vdem_dim2, n)
}

# Process security from V7 (T=6; period 6 is 2015-2018 but maps to ID 6).
security_res <- readRDS(security_file)
security_ip <- extract_country_period(security_res, "security")

security_trends <- security_ip %>%
  dplyr::group_by(domain, period, period_label) %>%
  dplyr::summarise(
    mean_dim1 = mean(dim1, na.rm = TRUE),
    sd_dim1 = stats::sd(dim1, na.rm = TRUE),
    skew_dim1 = compute_skewness(dim1),
    mean_dim2 = mean(dim2, na.rm = TRUE),
    sd_dim2 = stats::sd(dim2, na.rm = TRUE),
    .groups = "drop"
  )
all_trends[["security"]] <- security_trends

security_merged <- security_ip %>%
  dplyr::inner_join(unga, by = c("iso3c", "period")) %>%
  dplyr::select(domain, iso3c, period, period_label, dim1, dim2, unga_ideal_point)

if (nrow(security_merged) == 0) {
  issues[["security"]] <- "No overlap after merging security with UNGA (iso3c, period)."
} else {
  if (!is.null(vdem_period)) {
    security_merged <- security_merged %>%
      dplyr::left_join(vdem_period %>% dplyr::select(iso3c, period, vdem_libdem), by = c("iso3c", "period"))
  } else {
    security_merged <- security_merged %>% dplyr::mutate(vdem_libdem = NA_real_)
  }

  security_corr_by_period <- security_merged %>%
    dplyr::group_by(domain, period, period_label) %>%
    dplyr::summarise(
      r_unga_dim1 = safe_cor(dim1, unga_ideal_point),
      r_vdem_dim1 = safe_cor(dim1, vdem_libdem),
      r_unga_dim2 = safe_cor(dim2, unga_ideal_point),
      r_vdem_dim2 = safe_cor(dim2, vdem_libdem),
      n = sum(stats::complete.cases(dim1, unga_ideal_point)),
      .groups = "drop"
    )

  security_corr_overall <- security_merged %>%
    dplyr::summarise(
      domain = "security",
      period = 0L,
      period_label = "overall",
      r_unga_dim1 = safe_cor(dim1, unga_ideal_point),
      r_vdem_dim1 = safe_cor(dim1, vdem_libdem),
      r_unga_dim2 = safe_cor(dim2, unga_ideal_point),
      r_vdem_dim2 = safe_cor(dim2, vdem_libdem),
      n = sum(stats::complete.cases(dim1, unga_ideal_point))
    )

  all_corr[["security"]] <- dplyr::bind_rows(security_corr_overall, security_corr_by_period) %>%
    dplyr::select(domain, period, r_unga_dim1, r_vdem_dim1, r_unga_dim2, r_vdem_dim2, n)
}

corr_table <- dplyr::bind_rows(all_corr) %>%
  dplyr::arrange(domain, period)

trends_table <- dplyr::bind_rows(all_trends) %>%
  dplyr::arrange(domain, period)

readr::write_csv(corr_table, file.path(out_dir, "correlation_table.csv"))
readr::write_csv(trends_table, file.path(out_dir, "aggregate_trends.csv"))

# Build report.
report_lines <- c()
report_lines <- c(report_lines, "V8 Validation Report: UNGA + V-Dem (V8 extended domains + V7 security)")
report_lines <- c(report_lines, paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
report_lines <- c(report_lines, "")
report_lines <- c(report_lines, paste0("Domains (V8): ", paste(v8_domains, collapse = ", ")))
report_lines <- c(report_lines, "Additional domain (V7): security")
report_lines <- c(report_lines, "UNGA source: data/processed/unga_ideal_points_period_v8.csv (years 1990-2024; 7 periods expected)")
report_lines <- c(report_lines, paste0("V-Dem source: data/raw/vdem/vdem_country_year_v14.rds (aggregated inline to 7 periods; period 7 may be partial if V-Dem ends before 2024)"))
report_lines <- c(report_lines, "Period mapping used for joins: 1=1990-1994, 2=1995-1999, 3=2000-2004, 4=2005-2009, 5=2010-2014, 6=2015-2019 (security label may be 2015-2018), 7=2020-2024, 0=overall")
report_lines <- c(report_lines, "")

if (!is.null(vdem_period)) {
  vdem_cov <- vdem_period %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      n_countries = dplyr::n_distinct(iso3c),
      year_min = min(year_min, na.rm = TRUE),
      year_max = max(year_max, na.rm = TRUE),
      n_years_median = stats::median(n_years, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(period)
  report_lines <- c(report_lines, "V-Dem coverage by period (based on available country-years):")
  report_lines <- c(report_lines, utils::capture.output(print(vdem_cov, n = Inf)))
  report_lines <- c(report_lines, "")
} else {
  report_lines <- c(report_lines, "V-Dem not available or missing required columns; correlations with V-Dem are NA.")
  report_lines <- c(report_lines, "")
}

if (nrow(corr_table) == 0) {
  report_lines <- c(report_lines, "ERROR: correlation_table is empty (likely merge failure).")
} else {
  overall <- corr_table %>% dplyr::filter(period == 0L)
  report_lines <- c(report_lines, "Overall correlations (period=0):")
  report_lines <- c(
    report_lines,
    utils::capture.output(
      print(
        overall %>% dplyr::arrange(domain) %>% dplyr::select(domain, r_unga_dim1, r_vdem_dim1, n),
        n = Inf
      )
    )
  )
  report_lines <- c(report_lines, "")

  report_lines <- c(report_lines, "Notes on period coverage:")
  report_lines <- c(report_lines, "- V8 extended domains should have periods 1-7.")
  report_lines <- c(report_lines, "- Security (V7) has periods 1-6 only; its period 6 label is '2015-2018' but maps to period ID 6 for joins.")
  report_lines <- c(report_lines, "")

  report_lines <- c(report_lines, "Data Issues / Diagnostics:")
  if (length(issues) > 0) {
    for (k in names(issues)) {
      report_lines <- c(report_lines, paste0("- ", k, ": ", issues[[k]]))
    }
  } else {
    report_lines <- c(report_lines, "- None detected at merge level (ideal points x UNGA).")
  }
  report_lines <- c(report_lines, "")

  report_lines <- c(report_lines, "Full Correlation Table (domain x period):")
  report_lines <- c(report_lines, utils::capture.output(print(corr_table, n = Inf)))
  report_lines <- c(report_lines, "")
  report_lines <- c(report_lines, "Aggregate Trends (domain x period):")
  report_lines <- c(report_lines, utils::capture.output(print(trends_table, n = Inf)))
}

writeLines(report_lines, file.path(out_dir, "validation_report.txt"), useBytes = TRUE)

cat("V8 validation completed. Outputs written to: ", out_dir, "\n", sep = "")

