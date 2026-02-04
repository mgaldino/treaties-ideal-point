suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it with install.packages('ggplot2').")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Please install it with install.packages('tidyr').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please install it with install.packages('countrycode').")
  }
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(countrycode)
})

out_dir <- "outputs/validation"
fig_dir <- file.path(out_dir, "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

unga_path <- "data/processed/unga_ideal_points_period.csv"
if (!file.exists(unga_path)) {
  stop("Missing UNGA period file: ", unga_path)
}

unga <- readr::read_csv(unga_path, show_col_types = FALSE)

areas <- c("investment", "security", "environment", "human_rights", "arms_control", "intellectual_property")

selected_countries <- c("United States", "China", "Russia", "Brazil", "India", "Germany", "Denmark", "Iran", "South Africa", "Japan", "New Zealand", "North Korea", "Israel", "Angola")
selected_iso3 <- countrycode::countrycode(selected_countries, "country.name", "iso3c", warn = FALSE)

corr_overall <- list()

for (area in areas) {
  est_path <- file.path("outputs/estimates", paste0(area, "_ideal_points.csv"))
  if (!file.exists(est_path)) {
    stop("Missing estimates: ", est_path)
  }
  est <- readr::read_csv(est_path, show_col_types = FALSE)

  merged <- est %>%
    dplyr::rename(ideal_point = ideal_point) %>%
    dplyr::inner_join(unga, by = c("country_iso3" = "iso3", "period"))

  if (nrow(merged) == 0) {
    stop("No overlap with UNGA for area: ", area)
  }

  corr_val <- suppressWarnings(cor(merged$ideal_point, merged$unga_ideal_point, use = "complete.obs"))
  corr_overall[[area]] <- tibble::tibble(
    issue_area = area,
    correlation = corr_val,
    n = nrow(merged)
  )

  corr_by_period <- merged %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      correlation = suppressWarnings(cor(ideal_point, unga_ideal_point, use = "complete.obs")),
      n = n(),
      .groups = "drop"
    )

  readr::write_csv(corr_by_period, file.path(out_dir, paste0("unga_correlations_by_period_", area, ".csv")))

  # Time-series plots for selected countries
  series <- merged %>%
    dplyr::filter(country_iso3 %in% selected_iso3) %>%
    dplyr::mutate(
      country_name = countrycode::countrycode(country_iso3, "iso3c", "country.name", warn = FALSE)
    )

  if (nrow(series) > 0) {
    series_long <- series %>%
      dplyr::select(country_name, period, ideal_point, unga_ideal_point) %>%
      tidyr::pivot_longer(cols = c("ideal_point", "unga_ideal_point"), names_to = "series", values_to = "value")

    # z-score within each series (for comparability)
    series_long <- series_long %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(value_z = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) %>%
      dplyr::ungroup()

    p <- ggplot(series_long, aes(x = period, y = value_z, color = series, group = series)) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1.2) +
      facet_wrap(~ country_name, scales = "free_y") +
      labs(
        title = paste("Treaty vs UNGA ideal points (z-score) -", area),
        x = "Period",
        y = "Z-score",
        color = "Series"
      ) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(fig_dir, paste0("fig_timeseries_unga_", area, ".png")), p, width = 10, height = 7, dpi = 200)
  }
}

corr_overall_df <- dplyr::bind_rows(corr_overall)
readr::write_csv(corr_overall_df, file.path(out_dir, "unga_correlations_overall.csv"))

cat("Phase 4 validation completed for all", length(areas), "areas. Outputs in outputs/validation/.\n")
