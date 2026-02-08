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

out_dir <- "outputs/v7_validation"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

safe_cor <- function(x, y) {
  ok <- stats::complete.cases(x, y)
  if (sum(ok) < 3) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x[ok], y[ok]))
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
      period_id == 6L ~ "2015-2018",
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
    p == "2015-2018" ~ 6L,
    TRUE ~ NA_integer_
  )
  return(list(period_id = period_id, period_label = p))
}

load_unga <- function(path) {
  if (!file.exists(path)) {
    stop("Missing UNGA period file: ", path)
  }
  u <- readr::read_csv(path, show_col_types = FALSE)

  # Standardize columns.
  if ("iso3c" %in% names(u)) {
    u <- u %>% dplyr::rename(iso3c = iso3c)
  } else if ("iso3" %in% names(u)) {
    u <- u %>% dplyr::rename(iso3c = iso3)
  } else {
    stop("UNGA file missing iso3/iso3c column: ", path)
  }

  if ("idealpoint" %in% names(u)) {
    u <- u %>% dplyr::rename(unga_ideal_point = idealpoint)
  } else if ("unga_ideal_point" %in% names(u)) {
    u <- u %>% dplyr::rename(unga_ideal_point = unga_ideal_point)
  } else {
    stop("UNGA file missing idealpoint/unga_ideal_point column: ", path)
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
    # Keep only join keys + value to avoid name conflicts with V7's period_label.
    dplyr::select(iso3c, period, unga_ideal_point)

  u
}

extract_v7_country_period <- function(res) {
  if (!is.list(res) || is.null(res$ideal_points)) {
    stop("Unexpected V7 results structure: missing $ideal_points")
  }
  ip <- res$ideal_points
  if (!is.array(ip) || length(dim(ip)) != 3L) {
    stop("Unexpected $ideal_points shape (expected 3D array).")
  }
  if (is.null(res$country_codes) || is.null(res$period_labels)) {
    stop("Unexpected V7 results structure: missing $country_codes or $period_labels")
  }

  n_c <- dim(ip)[1]
  n_d <- dim(ip)[2]
  n_p <- dim(ip)[3]
  if (n_d < 2) {
    stop("V7 ideal_points has <2 dimensions; expected dim1 and dim2.")
  }

  countries <- res$country_codes
  periods <- seq_len(n_p)
  period_labels <- res$period_labels
  if (length(countries) != n_c) {
    stop("Length mismatch: country_codes vs ideal_points[,,].")
  }
  if (length(period_labels) != n_p) {
    stop("Length mismatch: period_labels vs ideal_points[,,].")
  }

  # Expand grid and index into array.
  grid <- tidyr::expand_grid(country_idx = seq_len(n_c), period = periods)
  grid <- grid %>%
    dplyr::mutate(
      iso3c = countries[country_idx],
      period_label = period_labels[period],
      dim1 = ip[cbind(country_idx, 1L, period)],
      dim2 = ip[cbind(country_idx, 2L, period)]
    ) %>%
    dplyr::select(iso3c, period, period_label, dim1, dim2)

  grid
}

load_vdem_country_year <- function() {
  raw_dir <- "data/raw/vdem"
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  # Try local files first.
  candidates <- list.files(raw_dir, full.names = TRUE)
  if (length(candidates) > 0) {
    # Prefer rds if present.
    rds <- candidates[grepl("\\.rds$", candidates, ignore.case = TRUE)]
    if (length(rds) > 0) {
      v <- readRDS(rds[1])
      return(list(data = v, source = paste0("local_rds:", basename(rds[1]))))
    }
    csv <- candidates[grepl("\\.csv$", candidates, ignore.case = TRUE)]
    if (length(csv) > 0) {
      v <- readr::read_csv(csv[1], show_col_types = FALSE)
      return(list(data = v, source = paste0("local_csv:", basename(csv[1]))))
    }
  }

  # Try vdemdata package (may require internet).
  if (requireNamespace("vdemdata", quietly = TRUE)) {
    v <- NULL
    src <- NULL
    # Best-effort across common function signatures/objects.
    try({
      v <- vdemdata::vdemdata("country_year")
      src <- "vdemdata::vdemdata('country_year')"
    }, silent = TRUE)
    if (is.null(v)) {
      try({
        v <- vdemdata::vdemdata(country_year = TRUE)
        src <- "vdemdata::vdemdata(country_year=TRUE)"
      }, silent = TRUE)
    }
    if (is.null(v) && exists("country_year", where = asNamespace("vdemdata"), inherits = FALSE)) {
      try({
        v <- get("country_year", envir = asNamespace("vdemdata"))
        src <- "vdemdata::country_year"
      }, silent = TRUE)
    }
    if (!is.null(v)) {
      saveRDS(v, file.path(raw_dir, "vdem_country_year_v14.rds"))
      return(list(data = v, source = src))
    }
  }

  # Try direct download (URL may change; failures are handled upstream).
  # Keep this as best-effort only.
  dest_zip <- file.path(raw_dir, "vdem_country_year_v14.zip")
  dest_rds <- file.path(raw_dir, "vdem_country_year_v14.rds")
  if (!file.exists(dest_rds)) {
    url_candidates <- c(
      # Known-ish historical patterns; may fail if the site structure changed.
      "https://v-dem.net/media/datasets/V-Dem-CY-Full+Others-v14.zip",
      "https://v-dem.net/media/datasets/V-Dem-CY-Full+Others-v14.zip?download=1"
    )
    for (u in url_candidates) {
      ok <- FALSE
      try({
        utils::download.file(u, dest_zip, mode = "wb", quiet = TRUE)
        ok <- file.exists(dest_zip) && file.info(dest_zip)$size > 0
      }, silent = TRUE)
      if (ok) {
        break
      }
    }
    if (file.exists(dest_zip) && file.info(dest_zip)$size > 0) {
      tmpdir <- file.path(raw_dir, "tmp_unzip")
      dir.create(tmpdir, showWarnings = FALSE)
      utils::unzip(dest_zip, exdir = tmpdir)
      csvs <- list.files(tmpdir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
      if (length(csvs) > 0) {
        v <- readr::read_csv(csvs[1], show_col_types = FALSE)
        saveRDS(v, dest_rds)
        return(list(data = v, source = paste0("downloaded_zip:", basename(dest_zip))))
      }
    }
  }

  return(list(data = NULL, source = "unavailable"))
}

v7_dir <- "outputs/v7_country_anchors"
if (!dir.exists(v7_dir)) {
  stop("Missing directory: ", v7_dir)
}

v7_files <- list.files(v7_dir, pattern = "_results\\.rds$", full.names = TRUE)
if (length(v7_files) == 0) {
  stop("No V7 results found in: ", v7_dir)
}
domains <- gsub("_results\\.rds$", "", basename(v7_files))

unga <- load_unga("data/processed/unga_ideal_points_period.csv")

vdem_loaded <- load_vdem_country_year()
vdem_source <- vdem_loaded$source
vdem_available <- !is.null(vdem_loaded$data)

vdem_period <- NULL
if (vdem_available) {
  v <- vdem_loaded$data
  # Standardize columns to iso3c/year/v2x_libdem.
  # V-Dem commonly uses: country_text_id, year, v2x_libdem.
  if ("country_text_id" %in% names(v)) {
    v <- v %>% dplyr::rename(iso3c = country_text_id)
  } else if ("iso3c" %in% names(v)) {
    v <- v %>% dplyr::rename(iso3c = iso3c)
  } else if ("iso3" %in% names(v)) {
    v <- v %>% dplyr::rename(iso3c = iso3)
  }
  if (!("iso3c" %in% names(v))) {
    vdem_available <- FALSE
  }
  if (!("year" %in% names(v))) {
    vdem_available <- FALSE
  }
  if (!("v2x_libdem" %in% names(v))) {
    vdem_available <- FALSE
  }

  if (vdem_available) {
    vdem_period <- v %>%
      dplyr::mutate(
        year = as.integer(year),
        period = dplyr::case_when(
          year >= 1990L & year <= 1994L ~ 1L,
          year >= 1995L & year <= 1999L ~ 2L,
          year >= 2000L & year <= 2004L ~ 3L,
          year >= 2005L & year <= 2009L ~ 4L,
          year >= 2010L & year <= 2014L ~ 5L,
          year >= 2015L & year <= 2018L ~ 6L,
          TRUE ~ NA_integer_
        )
      ) %>%
      dplyr::filter(!is.na(period), !is.na(iso3c)) %>%
      dplyr::group_by(iso3c, period) %>%
      dplyr::summarise(
        vdem_libdem = mean(v2x_libdem, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    vdem_period <- NULL
  }
}

all_corr <- list()
all_trends <- list()
issues <- list()

for (i in seq_along(v7_files)) {
  domain <- domains[i]
  res <- readRDS(v7_files[i])
  v7_ip <- extract_v7_country_period(res) %>%
    dplyr::mutate(domain = domain) %>%
    dplyr::select(domain, iso3c, period, period_label, dim1, dim2)

  # Aggregate trends from V7 only.
  trends <- v7_ip %>%
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

  merged <- v7_ip %>%
    dplyr::inner_join(unga, by = c("iso3c", "period")) %>%
    dplyr::select(domain, iso3c, period, period_label, dim1, dim2, unga_ideal_point)

  if (nrow(merged) == 0) {
    issues[[domain]] <- "No overlap after merging V7 with UNGA (iso3c, period)."
    next
  }

  if (!is.null(vdem_period)) {
    merged <- merged %>%
      dplyr::left_join(vdem_period, by = c("iso3c", "period"))
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
      # Use .env to avoid capturing merged$domain (a vector) and accidentally returning many rows.
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

corr_table <- dplyr::bind_rows(all_corr) %>%
  dplyr::arrange(domain, period)

trends_table <- dplyr::bind_rows(all_trends) %>%
  dplyr::arrange(domain, period)

readr::write_csv(corr_table, file.path(out_dir, "correlation_table.csv"))
readr::write_csv(trends_table, file.path(out_dir, "aggregate_trends.csv"))

# Build report.
report_lines <- c()
report_lines <- c(report_lines, "V7 Validation Report: UNGA + V-Dem (All Domains)")
report_lines <- c(report_lines, paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
report_lines <- c(report_lines, "")
report_lines <- c(report_lines, paste0("Domains: ", paste(sort(unique(domains)), collapse = ", ")))
report_lines <- c(report_lines, paste0("UNGA source: data/processed/unga_ideal_points_period.csv"))
report_lines <- c(report_lines, paste0("V-Dem available: ", ifelse(!is.null(vdem_period), "YES", "NO")))
report_lines <- c(report_lines, paste0("V-Dem source attempt: ", vdem_source))
report_lines <- c(report_lines, "Period mapping: 1=1990-1994, 2=1995-1999, 3=2000-2004, 4=2005-2009, 5=2010-2014, 6=2015-2018, 0=overall")
report_lines <- c(report_lines, "")

if (nrow(corr_table) == 0) {
  report_lines <- c(report_lines, "ERROR: correlation_table is empty (likely merge failure).")
} else {
  overall <- corr_table %>% dplyr::filter(period == 0L)
  strong_unga <- overall %>% dplyr::filter(!is.na(r_unga_dim1), abs(r_unga_dim1) > 0.3)
  not_strong_unga <- overall %>%
    dplyr::filter(is.na(r_unga_dim1) | abs(r_unga_dim1) <= 0.3) %>%
    dplyr::pull(domain)
  report_lines <- c(report_lines, "Key Findings")
  report_lines <- c(report_lines, paste0("- dim1 vs UNGA (overall): median r = ", round(stats::median(overall$r_unga_dim1, na.rm = TRUE), 3)))
  if (nrow(strong_unga) > 0) {
    report_lines <- c(report_lines, paste0("- Strong UNGA alignment (|r|>0.3): ", paste(strong_unga$domain, collapse = ", ")))
  } else {
    report_lines <- c(report_lines, "- Strong UNGA alignment (|r|>0.3): none")
  }
  report_lines <- c(report_lines, paste0("- Not strong (|r|<=0.3 or NA): ", paste(not_strong_unga, collapse = ", ")))
  report_lines <- c(report_lines, "")
  report_lines <- c(report_lines, "UNGA Correlations (Overall, Sorted by |r_unga_dim1|)")
  report_lines <- c(
    report_lines,
    utils::capture.output(
      print(
        overall %>%
          dplyr::arrange(dplyr::desc(abs(r_unga_dim1))) %>%
          dplyr::select(domain, r_unga_dim1, r_unga_dim2, n),
        n = Inf
      )
    )
  )

  if (!is.null(vdem_period)) {
    comp <- overall %>%
      dplyr::mutate(
        diff_dim1 = r_unga_dim1 - r_vdem_dim1,
        unga_gt_vdem_dim1 = !is.na(diff_dim1) & diff_dim1 > 0
      )
    n_gt <- sum(comp$unga_gt_vdem_dim1, na.rm = TRUE)
    report_lines <- c(report_lines, paste0("- dim1 correlates more with UNGA than V-Dem in ", n_gt, " / ", nrow(comp), " domains (overall)."))
  } else {
    report_lines <- c(report_lines, "- UNGA vs V-Dem comparison: skipped (V-Dem not available).")
  }

  # Trend summaries from mean_dim1 and sd_dim1.
  trend_summary <- trends_table %>%
    dplyr::filter(period %in% 1:6) %>%
    dplyr::group_by(domain) %>%
    dplyr::summarise(
      delta_mean_dim1 = mean_dim1[period == 6L] - mean_dim1[period == 1L],
      slope_mean_dim1 = tryCatch(stats::coef(stats::lm(mean_dim1 ~ period))[2], error = function(e) NA_real_),
      delta_sd_dim1 = sd_dim1[period == 6L] - sd_dim1[period == 1L],
      slope_sd_dim1 = tryCatch(stats::coef(stats::lm(sd_dim1 ~ period))[2], error = function(e) NA_real_),
      .groups = "drop"
    )

  report_lines <- c(report_lines, "")
  report_lines <- c(report_lines, "Temporal Trends (V7 dim1)")
  report_lines <- c(report_lines, "Interpretation: +delta_mean_dim1 implies increasing average dim1; +delta_sd_dim1 implies increasing dispersion/polarization.")
  report_lines <- c(report_lines, utils::capture.output(print(trend_summary, n = Inf)))
  report_lines <- c(report_lines, "")

  # Data issues.
  report_lines <- c(report_lines, "Data Issues / Diagnostics")
  if (length(issues) > 0) {
    for (k in names(issues)) {
      report_lines <- c(report_lines, paste0("- ", k, ": ", issues[[k]]))
    }
  } else {
    report_lines <- c(report_lines, "- None detected at merge level (V7 x UNGA).")
  }

  low_n <- corr_table %>%
    dplyr::filter(period %in% 1:6) %>%
    dplyr::group_by(domain) %>%
    dplyr::summarise(
      min_n = min(n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(min_n)
  report_lines <- c(report_lines, utils::capture.output(print(low_n, n = Inf)))

  report_lines <- c(report_lines, "")
  report_lines <- c(report_lines, "Full Correlation Table (domain x period)")
  report_lines <- c(report_lines, utils::capture.output(print(corr_table, n = Inf)))
  report_lines <- c(report_lines, "")
  report_lines <- c(report_lines, "Aggregate Trends (domain x period)")
  report_lines <- c(report_lines, utils::capture.output(print(trends_table, n = Inf)))
}

writeLines(report_lines, file.path(out_dir, "validation_report.txt"), useBytes = TRUE)

cat("V7 validation completed. Outputs written to: ", out_dir, "\n", sep = "")
