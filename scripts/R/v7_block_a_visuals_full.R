#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggrepel)
})

# ---- Helpers ----
clean_utf8 <- function(x) {
  if (is.null(x)) return(x)
  x <- ifelse(is.na(x), "", x)
  iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
}

safe_cor <- function(x, y) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
}

# Load V7 results into a tidy country-period table
extract_v7_country_period <- function(res, domain, coding) {
  if (is.null(res$ideal_points)) stop("Missing ideal_points")
  if (is.null(res$country_codes)) stop("Missing country_codes")
  if (is.null(res$period_labels)) {
    # fallback: build from T
    T_periods <- dim(res$ideal_points)[3]
    period_labels <- paste0("P", seq_len(T_periods))
  } else {
    period_labels <- as.character(res$period_labels)
  }

  x <- res$ideal_points
  N <- dim(x)[1]
  K <- dim(x)[2]
  T_periods <- dim(x)[3]
  if (K < 2) stop("Expected K>=2")

  df <- expand.grid(
    iso3 = res$country_codes,
    period_index = seq_len(T_periods)
  ) %>%
    dplyr::mutate(
      domain = domain,
      coding = coding,
      period = period_labels[period_index]
    )

  dim1 <- as.vector(x[, 1, ])
  dim2 <- as.vector(x[, 2, ])
  df$dim1 <- dim1
  df$dim2 <- dim2
  df
}

# Load RDS results from directory into table
load_v7_dir <- function(dir_path, coding) {
  files <- list.files(dir_path, pattern = "_results\\.rds$", full.names = TRUE)
  if (length(files) == 0) stop("No V7 results in: ", dir_path)
  domains <- gsub("_results\\.rds$", "", basename(files))
  res_list <- vector("list", length(files))
  for (i in seq_along(files)) {
    res <- readRDS(files[i])
    res_list[[i]] <- extract_v7_country_period(res, domains[i], coding)
  }
  dplyr::bind_rows(res_list)
}

# ---- Paths ----
fig_dir <- "outputs/v7_report/figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ---- Load data ----
setwd("/Users/manoelgaldino/Documents/DCP/Papers/Ideal\ point")
v7_flow <- load_v7_dir("outputs/v7_country_anchors", "flow")
v7_stock <- load_v7_dir("outputs/v7_stock_country_anchors", "stock")

## gráficos Manoel

# scatter por período e domínio, com nomes dos extremos
extremos <- v7_flow %>%
  group_by(domain, period) %>%
  mutate(.row_id = row_number()) %>%  # garante que dá pra "distinct" mesmo com empates
  {
    bind_rows(
      slice_min(., dim1, n = 5, with_ties = FALSE),
      slice_max(., dim1, n = 5, with_ties = FALSE),
      slice_min(., dim2, n = 5, with_ties = FALSE),
      slice_max(., dim2, n = 5, with_ties = FALSE)
    )
  } %>%
  distinct(domain, period, .row_id, .keep_all = TRUE) %>%
  ungroup()

extremos90 <- extremos %>%
  dplyr::filter(period == "1990-1994")

v7_flow %>%
  dplyr::filter(period == "1990-1994") %>%
  ggplot(aes(dim1, dim2)) +
  geom_point() +
  geom_text_repel(
    data = extremos90,
    aes(label = iso3),
    size = 3,
    max.overlaps = Inf
  ) +
  facet_wrap(domain ~ period, scales = "free") +
  ggtitle("Flow coding data") +
  xlab("1st dimension") +
  ylab("2nd dimension")

v7_all <- dplyr::bind_rows(v7_flow, v7_stock)

# UNGA + V-Dem data
unga <- read_csv("data/processed/unga_ideal_points_period.csv", show_col_types = FALSE) %>%
  dplyr::mutate(iso3 = as.character(iso3), period = as.character(period))

# Expect vdem period-level file
vdem_path <- "data/processed/vdem_liberal_democracy_period.csv"
if (!file.exists(vdem_path)) {
  stop("Missing V-Dem period file: ", vdem_path)
}

vdem <- read_csv(vdem_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    iso3 = as.character(iso3c),
    period = as.character(period)
  ) %>%
  dplyr::select(iso3, period, vdem_libdem)

# Country names for labels
country_map <- read_csv("data/processed/country_codebook.csv", show_col_types = FALSE) %>%
  dplyr::transmute(iso3 = as.character(iso3), country_name = as.character(country_name))

# Period order
period_order <- v7_flow %>% dplyr::distinct(period, period_index) %>% dplyr::arrange(period_index) %>% dplyr::pull(period)

# Domains
domains <- sort(unique(v7_flow$domain))

# Key countries
key_countries <- c("USA", "CHN", "RUS", "DEU", "DNK", "IRN", "BRA", "IND", "GBR", "ZAF")

# ---- Merge external benchmarks (flow only for validation visuals) ----
v7_flow_ext <- v7_flow %>%
  dplyr::left_join(unga, by = c("iso3", "period")) %>%
  dplyr::left_join(vdem, by = c("iso3", "period"))

# ---- 1) Small multiples: Dim1 vs UNGA by period (per domain) ----
for (dom in domains) {
  df <- v7_flow_ext %>% dplyr::filter(domain == dom)
  p <- ggplot(df, aes(x = unga_ideal_point, y = dim1)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
    facet_wrap(~ period, ncol = 3) +
    labs(
      title = sprintf("Dim1 vs UNGA by period — %s", dom),
      x = "UNGA ideal point",
      y = "Dim1 (flow)"
    ) +
    theme_minimal(base_size = 10)
  ggsave(file.path(fig_dir, sprintf("fig_dim1_unga_%s_by_period.png", dom)), p, width = 9, height = 6, dpi = 160)
}

# ---- 2) Small multiples: Dim1 vs V-Dem by period (per domain) ----
for (dom in domains) {
  df <- v7_flow_ext %>% dplyr::filter(domain == dom)
  p <- ggplot(df, aes(x = vdem_libdem, y = dim1)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
    facet_wrap(~ period, ncol = 3) +
    labs(
      title = sprintf("Dim1 vs V-Dem by period — %s", dom),
      x = "V-Dem liberal democracy",
      y = "Dim1 (flow)"
    ) +
    theme_minimal(base_size = 10)
  ggsave(file.path(fig_dir, sprintf("fig_dim1_vdem_%s_by_period.png", dom)), p, width = 9, height = 6, dpi = 160)
}

# ---- 3) Correlation heatmaps (domain x period) ----
cor_tbl <- v7_flow_ext %>%
  dplyr::group_by(domain, period) %>%
  dplyr::summarise(
    cor_unga = safe_cor(dim1, unga_ideal_point),
    cor_vdem = safe_cor(dim1, vdem_libdem),
    .groups = "drop"
  )

cor_long <- cor_tbl %>%
  tidyr::pivot_longer(cols = c(cor_unga, cor_vdem), names_to = "benchmark", values_to = "corr") %>%
  dplyr::mutate(
    benchmark = dplyr::recode(benchmark, cor_unga = "UNGA", cor_vdem = "V-Dem"),
    period = factor(period, levels = period_order),
    domain = factor(domain, levels = domains)
  )

p_heat <- ggplot(cor_long, aes(x = period, y = domain, fill = corr)) +
  geom_tile(color = "white") +
  facet_wrap(~ benchmark, ncol = 1) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0) +
  labs(title = "Correlation of Dim1 with external benchmarks", x = "Period", y = "Domain") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig_dim1_corr_heatmap.png"), p_heat, width = 8, height = 6, dpi = 160)

# ---- 4) Trajectories: UNGA vs V-Dem colored by Dim1 (key countries) ----
traj <- v7_flow_ext %>%
  dplyr::filter(iso3 %in% key_countries) %>%
  dplyr::left_join(country_map, by = "iso3") %>%
  dplyr::mutate(country_label = ifelse(is.na(country_name) | country_name == "", iso3, country_name))

p_traj <- ggplot(traj, aes(x = unga_ideal_point, y = vdem_libdem, color = dim1, group = iso3)) +
  geom_path(alpha = 0.7) +
  geom_point(size = 1.8) +
  geom_text(data = traj %>% dplyr::group_by(iso3) %>% dplyr::filter(period_index == max(period_index)),
            aes(label = iso3), hjust = -0.1, vjust = 0.5, size = 3, show.legend = FALSE) +
  scale_color_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0) +
  labs(title = "Trajectories in UNGA–V-Dem space (color = Dim1)", x = "UNGA", y = "V-Dem") +
  theme_minimal(base_size = 10)

ggsave(file.path(fig_dir, "fig_unga_vdem_dim1_trajectories.png"), p_traj, width = 8, height = 6, dpi = 160)

# ---- 5) Flow vs Stock scatter by period (per domain) ----
flow_stock <- v7_flow %>%
  dplyr::select(domain, iso3, period, period_index, dim1, dim2) %>%
  dplyr::rename(dim1_flow = dim1, dim2_flow = dim2) %>%
  dplyr::inner_join(
    v7_stock %>% dplyr::select(domain, iso3, period, period_index, dim1, dim2) %>%
      dplyr::rename(dim1_stock = dim1, dim2_stock = dim2),
    by = c("domain", "iso3", "period", "period_index")
  )

for (dom in domains) {
  df <- flow_stock %>% dplyr::filter(domain == dom)
  p <- ggplot(df, aes(x = dim1_stock, y = dim1_flow)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(~ period, ncol = 3) +
    labs(title = sprintf("Flow vs Stock Dim1 by period — %s", dom), x = "Stock Dim1", y = "Flow Dim1") +
    theme_minimal(base_size = 10)
  ggsave(file.path(fig_dir, sprintf("fig_flow_stock_dim1_%s_by_period.png", dom)), p, width = 9, height = 6, dpi = 160)
}

# ---- 6) Trajectories: Flow vs Stock for key countries ----
traj_fs <- flow_stock %>% dplyr::filter(iso3 %in% key_countries)

p_fs_traj <- ggplot(traj_fs, aes(x = period_index)) +
  geom_line(aes(y = dim1_flow, color = "Flow")) +
  geom_line(aes(y = dim1_stock, color = "Stock"), linetype = "dashed") +
  facet_wrap(~ iso3, scales = "free_y") +
  scale_x_continuous(breaks = seq_along(period_order), labels = period_order) +
  labs(title = "Flow vs Stock trajectories (Dim1) — key countries", x = "Period", y = "Dim1") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig_flow_stock_trajectories_key_countries.png"), p_fs_traj, width = 9, height = 7, dpi = 160)

# ---- 7) Density/ridge plots: Flow vs Stock by period ----
fs_long <- flow_stock %>%
  dplyr::select(domain, period, dim1_flow, dim1_stock) %>%
  tidyr::pivot_longer(cols = c(dim1_flow, dim1_stock), names_to = "coding", values_to = "dim1") %>%
  dplyr::mutate(coding = dplyr::recode(coding, dim1_flow = "Flow", dim1_stock = "Stock"))

for (dom in domains) {
  df <- fs_long %>% dplyr::filter(domain == dom)
  p <- ggplot(df, aes(x = dim1, fill = coding)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ period, ncol = 3) +
    labs(title = sprintf("Dim1 density: Flow vs Stock — %s", dom), x = "Dim1", y = "Density") +
    theme_minimal(base_size = 10)
  ggsave(file.path(fig_dir, sprintf("fig_dim1_density_flow_stock_%s.png", dom)), p, width = 9, height = 6, dpi = 160)
}

# ---- 7.5) Dim1 vs Dim2 scatter with extremes labeled (per period) ----
extremos <- v7_flow %>%
  dplyr::group_by(domain, period) %>%
  dplyr::mutate(.row_id = dplyr::row_number()) %>%
  {
    dplyr::bind_rows(
      dplyr::slice_min(., dim1, n = 5, with_ties = FALSE),
      dplyr::slice_max(., dim1, n = 5, with_ties = FALSE),
      dplyr::slice_min(., dim2, n = 5, with_ties = FALSE),
      dplyr::slice_max(., dim2, n = 5, with_ties = FALSE)
    )
  } %>%
  dplyr::distinct(domain, period, .row_id, .keep_all = TRUE) %>%
  dplyr::ungroup()

for (p_lab in period_order) {
  df_p <- v7_flow %>% dplyr::filter(period == p_lab)
  ext_p <- extremos %>% dplyr::filter(period == p_lab)

  p_sc <- ggplot(df_p, aes(dim1, dim2)) +
    geom_point(alpha = 0.6, size = 1) +
    ggrepel::geom_text_repel(
      data = ext_p,
      aes(label = iso3),
      size = 3,
      max.overlaps = Inf
    ) +
    facet_wrap(~ domain, scales = "free") +
    labs(
      title = sprintf("Flow coding: Dim1 vs Dim2 with extremes labeled (%s)", p_lab),
      x = "1st dimension",
      y = "2nd dimension"
    ) +
    theme_minimal(base_size = 9)

  safe_p <- gsub("[^0-9A-Za-z]+", "_", p_lab)
  ggsave(
    file.path(fig_dir, sprintf("fig_dim12_extremes_%s.png", safe_p)),
    p_sc, width = 9, height = 6.5, dpi = 160
  )
}

# ---- 8) Dispersion over time (SD) for Flow vs Stock ----
fs_disp <- flow_stock %>%
  dplyr::group_by(domain, period, coding = "Flow") %>%
  dplyr::summarise(sd_dim1 = sd(dim1_flow, na.rm = TRUE), .groups = "drop") %>%
  dplyr::bind_rows(
    flow_stock %>% dplyr::group_by(domain, period, coding = "Stock") %>%
      dplyr::summarise(sd_dim1 = sd(dim1_stock, na.rm = TRUE), .groups = "drop")
  ) %>%
  dplyr::mutate(period = factor(period, levels = period_order))

p_disp <- ggplot(fs_disp, aes(x = period, y = sd_dim1, color = coding, group = coding)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ domain, scales = "free_y") +
  labs(title = "Dispersion over time (SD) — Flow vs Stock", x = "Period", y = "SD of Dim1") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig_dispersion_flow_stock.png"), p_disp, width = 10, height = 7, dpi = 160)

# ---- 9) Domain trends with uncertainty (mean ± SE) ----
trend_flow <- v7_flow %>%
  dplyr::group_by(domain, period, period_index) %>%
  dplyr::summarise(mean_dim1 = mean(dim1, na.rm = TRUE),
                   se_dim1 = sd(dim1, na.rm = TRUE) / sqrt(sum(!is.na(dim1))),
                   .groups = "drop")

p_trend <- ggplot(trend_flow, aes(x = period_index, y = mean_dim1)) +
  geom_ribbon(aes(ymin = mean_dim1 - se_dim1, ymax = mean_dim1 + se_dim1), fill = "grey80") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ domain, scales = "free_y") +
  scale_x_continuous(breaks = seq_along(period_order), labels = period_order) +
  labs(title = "Dim1 trends by domain (mean ± SE, flow)", x = "Period", y = "Mean Dim1") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig_trends_domain_uncertainty.png"), p_trend, width = 10, height = 7, dpi = 160)

# ---- 10) System-level aggregate trend ----
# Equal-weight aggregation across domains
agg_flow <- trend_flow %>%
  dplyr::group_by(period, period_index) %>%
  dplyr::summarise(mean_dim1 = mean(mean_dim1, na.rm = TRUE), .groups = "drop")

p_agg <- ggplot() +
  geom_line(data = trend_flow, aes(x = period_index, y = mean_dim1, group = domain), color = "grey70") +
  geom_line(data = agg_flow, aes(x = period_index, y = mean_dim1), color = "black", linewidth = 1) +
  geom_point(data = agg_flow, aes(x = period_index, y = mean_dim1), color = "black", size = 2) +
  scale_x_continuous(breaks = seq_along(period_order), labels = period_order) +
  labs(title = "Aggregate trend (equal weights) vs domain trends", x = "Period", y = "Mean Dim1") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig_aggregate_trend.png"), p_agg, width = 8, height = 5, dpi = 160)

# ---- 11) Slope comparison (Flow vs Stock) ----
trend_stock <- v7_stock %>%
  dplyr::group_by(domain, period, period_index) %>%
  dplyr::summarise(mean_dim1 = mean(dim1, na.rm = TRUE), .groups = "drop")

slope_flow <- trend_flow %>%
  dplyr::group_by(domain) %>%
  dplyr::summarise(slope = coef(lm(mean_dim1 ~ period_index))[2], .groups = "drop") %>%
  dplyr::mutate(coding = "Flow")

slope_stock <- trend_stock %>%
  dplyr::group_by(domain) %>%
  dplyr::summarise(slope = coef(lm(mean_dim1 ~ period_index))[2], .groups = "drop") %>%
  dplyr::mutate(coding = "Stock")

slope_tbl <- dplyr::bind_rows(slope_flow, slope_stock)

p_slope <- ggplot(slope_tbl, aes(x = domain, y = slope, fill = coding)) +
  geom_col(position = "dodge") +
  labs(title = "Slope comparison (Flow vs Stock)", x = "Domain", y = "Slope of mean Dim1") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig_slope_comparison.png"), p_slope, width = 8, height = 5, dpi = 160)

# ---- 12) Human Rights: polarity check (Dim1 vs V-Dem) ----
hr <- v7_flow_ext %>% dplyr::filter(domain == "human_rights")

p_hr_pol <- ggplot(hr, aes(x = vdem_libdem, y = dim1)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  labs(title = "Human rights: Dim1 vs V-Dem (original)", x = "V-Dem", y = "Dim1") +
  theme_minimal(base_size = 10)

ggsave(file.path(fig_dir, "fig_hr_dim1_vdem.png"), p_hr_pol, width = 6, height = 4, dpi = 160)

p_hr_pol_flip <- ggplot(hr, aes(x = vdem_libdem, y = -dim1)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  labs(title = "Human rights: Dim1 vs V-Dem (polarity flipped)", x = "V-Dem", y = "-Dim1") +
  theme_minimal(base_size = 10)

ggsave(file.path(fig_dir, "fig_hr_dim1_vdem_flipped.png"), p_hr_pol_flip, width = 6, height = 4, dpi = 160)

# ---- 13) Human Rights: leverage diagnostics (label extremes) ----
hr_ext <- hr %>%
  dplyr::filter(!is.na(vdem_libdem) & !is.na(dim1)) %>%
  dplyr::mutate(abs_resid = abs(dim1 - (coef(lm(dim1 ~ vdem_libdem))[1] + coef(lm(dim1 ~ vdem_libdem))[2] * vdem_libdem))) %>%
  dplyr::arrange(desc(abs_resid)) %>%
  dplyr::slice_head(n = 10)

p_hr_lev <- ggplot(hr, aes(x = vdem_libdem, y = dim1)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_point(data = hr_ext, color = "red", size = 2) +
  geom_text(data = hr_ext, aes(label = iso3), vjust = -0.7, size = 3) +
  labs(title = "Human rights: outliers vs V-Dem", x = "V-Dem", y = "Dim1") +
  theme_minimal(base_size = 10)

ggsave(file.path(fig_dir, "fig_hr_outliers_vdem.png"), p_hr_lev, width = 6, height = 4, dpi = 160)

# ---- 14) Item-level diagnostic: discrimination vs treaty year (Human Rights) ----
# Try to load item codebook for treaty years
item_codebook <- read_csv("data/processed/item_codebook.csv", show_col_types = FALSE) %>%
  dplyr::mutate(item_id = as.character(item_id), issue_area = as.character(issue_area))

hr_res <- readRDS("outputs/v7_country_anchors/human_rights_results.rds")
if (!is.null(hr_res$beta) && !is.null(hr_res$item_labels)) {
  betas <- as.data.frame(hr_res$beta)
  betas$item_label <- hr_res$item_labels
  betas$item_id <- hr_res$item_labels
  betas$item_id_clean <- gsub("__\\d{4}-\\d{4}$", "", betas$item_id)
  betas$beta_norm <- sqrt(betas$V1^2 + betas$V2^2)

  hr_items <- item_codebook %>% dplyr::filter(issue_area == "human_rights") %>%
    dplyr::select(item_id, item_name, treaty_open_year)

  betas <- betas %>% dplyr::left_join(hr_items, by = c("item_id_clean" = "item_id"))

  p_hr_item <- ggplot(betas, aes(x = treaty_open_year, y = beta_norm)) +
    geom_point(alpha = 0.5) +
    labs(title = "Human rights: item discrimination vs treaty year", x = "Treaty open year", y = "||beta||") +
    theme_minimal(base_size = 10)

  ggsave(file.path(fig_dir, "fig_hr_item_beta_year.png"), p_hr_item, width = 7, height = 4.5, dpi = 160)
}

cat("All V7 Block A figures generated in: ", fig_dir, "\n", sep = "")
