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
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Please install it with install.packages('tibble').")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Please install it with install.packages('tidyr').")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required. Please install it with install.packages('stringr').")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required. Please install it with install.packages('gridExtra').")
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required. Please install it with install.packages('png').")
  }
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(tidyr)
  library(stringr)
  library(gridExtra)
  library(png)
})

set.seed(123)

output_dir <- "outputs/eda_phase2"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

fig_dir <- file.path(output_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

baseline_path <- "data/processed/baseline_events.csv"
items_path <- "data/processed/item_codebook.csv"

if (!file.exists(baseline_path) || !file.exists(items_path)) {
  stop("Missing baseline outputs. Run scripts/R/01_prepare_data.R first.")
}

events <- readr::read_csv(baseline_path, show_col_types = FALSE)
items <- readr::read_csv(items_path, show_col_types = FALSE)

period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018")
period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2019)

assign_period <- function(year) {
  cut(year, breaks = period_breaks, labels = period_labels, right = FALSE, include.lowest = TRUE)
}

# --- Summaries from flow matrices ---
issue_areas <- c("trade", "investment", "security", "environment",
                  "human_rights", "arms_control", "intellectual_property")

flow_summaries <- list()
for (area in issue_areas) {
  path <- file.path("data/processed", paste0(area, "_flow_matrix.rds"))
  flow <- readRDS(path)
  rc <- flow$rc
  total <- length(rc)
  n_zero <- sum(rc == 0)
  n_one <- sum(rc == 1)
  n_minus <- sum(rc == -1)
  flow_summaries[[area]] <- tibble::tibble(
    issue_area = area,
    countries = nrow(rc),
    items = ncol(rc),
    T = flow$T,
    share_zero = n_zero / total,
    share_one = n_one / total,
    share_minus_one = n_minus / total
  )
}
flow_summary <- dplyr::bind_rows(flow_summaries)
readr::write_csv(flow_summary, file.path(output_dir, "flow_summary.csv"))

# --- PCA per issue area ---
run_pca <- function(area) {
  path <- file.path("data/processed", paste0(area, "_flow_matrix.rds"))
  flow <- readRDS(path)
  rc <- flow$rc

  rc_pca <- rc
  rc_pca[rc_pca == 0] <- NA
  rc_pca[rc_pca == -1] <- 0

  # Impute NAs with column mean
  for (j in seq_len(ncol(rc_pca))) {
    col_mean <- mean(rc_pca[, j], na.rm = TRUE)
    rc_pca[is.na(rc_pca[, j]), j] <- col_mean
  }

  pca <- prcomp(rc_pca, center = TRUE, scale. = TRUE)

  # Scree plot
  var_expl <- pca$sdev^2 / sum(pca$sdev^2)
  scree <- tibble::tibble(
    component = seq_along(var_expl),
    variance_explained = var_expl
  )

  p <- ggplot(scree, aes(x = component, y = variance_explained)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5) +
    labs(
      title = paste("Scree plot -", area),
      x = "Component",
      y = "Proportion of variance"
    ) +
    theme_minimal(base_size = 11)

  fig_path <- file.path(fig_dir, paste0("fig_scree_", area, ".png"))
  ggsave(fig_path, p, width = 6, height = 4, dpi = 200)

  # Loadings by country (PC1)
  country_loadings <- tibble::tibble(
    iso3 = flow$country_codes,
    pc1 = pca$x[, 1]
  ) %>%
    dplyr::arrange(dplyr::desc(pc1))

  readr::write_csv(country_loadings, file.path(output_dir, paste0("pca_country_loadings_", area, ".csv")))

  # Loadings by item (PC1)
  item_loadings <- tibble::tibble(
    item = flow$item_labels,
    pc1 = pca$rotation[, 1]
  ) %>%
    dplyr::arrange(dplyr::desc(pc1))

  readr::write_csv(item_loadings, file.path(output_dir, paste0("pca_item_loadings_", area, ".csv")))

  return(invisible(TRUE))
}

for (area in issue_areas) {
  run_pca(area)
}

# --- Participation time series for selected countries ---
selected_countries <- c("United States", "China", "Russia", "Brazil", "India", "Germany",
                        "Denmark", "Iran", "South Africa", "Japan",
                        "New Zealand", "North Korea", "Saudi Arabia", "Somalia")

ev_ts <- events %>%
  dplyr::filter(country_name %in% selected_countries) %>%
  dplyr::mutate(period = as.character(assign_period(event_year))) %>%
  dplyr::filter(!is.na(period)) %>%
  dplyr::group_by(issue_area, country_name, period) %>%
  dplyr::summarise(new_events = n(), .groups = "drop")

# Ensure all period combinations exist
full_grid <- tidyr::expand_grid(
  issue_area = issue_areas,
  country_name = selected_countries,
  period = period_labels
)

ev_ts <- full_grid %>%
  dplyr::left_join(ev_ts, by = c("issue_area", "country_name", "period")) %>%
  dplyr::mutate(new_events = dplyr::coalesce(new_events, 0L))

for (area in issue_areas) {
  df <- ev_ts %>% dplyr::filter(issue_area == area)
  p <- ggplot(df, aes(x = period, y = new_events, group = country_name, color = country_name)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    labs(
      title = paste("New EIF events by period -", area),
      x = "Period",
      y = "New EIF events",
      color = "Country"
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  fig_path <- file.path(fig_dir, paste0("fig_timeseries_", area, ".png"))
  ggsave(fig_path, p, width = 7, height = 4, dpi = 200)
}

message("Phase 2 EDA completed. Figures and tables written to outputs/eda_phase2/")
