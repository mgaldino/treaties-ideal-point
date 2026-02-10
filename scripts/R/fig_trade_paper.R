## ============================================================
## Figures for trade dimension (paper)
## ============================================================
library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

theme_set(theme_minimal(base_size = 11))

outdir <- here("outputs", "paper_figures")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# === Load data ===
yearly <- read_csv(here("data", "processed", "trade_panel_yearly.csv"),
                   show_col_types = FALSE)
panel  <- read_csv(here("data", "processed", "trade_panel.csv"),
                   show_col_types = FALSE)

# Key countries for highlighting
key_countries <- c("USA", "CHN", "IND", "BRA", "GBR", "JPN",
                   "RUS", "KOR", "DEU", "TUR", "MEX", "AUS")

# ============================================================
# Figure 7a: Time series of cross-country MFN mean + ribbon
# ============================================================
agg_year <- yearly %>%
  filter(year >= 1990, year <= 2022) %>%
  group_by(year) %>%
  summarise(
    mean_mfn = mean(mfn_mean, na.rm = TRUE),
    sd_mfn = sd(mfn_mean, na.rm = TRUE),
    mean_margin = mean(margin_mean, na.rm = TRUE),
    sd_margin = sd(margin_mean, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_mfn = sd_mfn / sqrt(n),
    se_margin = sd_margin / sqrt(n)
  )

p7a <- ggplot(agg_year, aes(x = year)) +
  geom_ribbon(aes(ymin = mean_mfn - sd_mfn, ymax = mean_mfn + sd_mfn),
              fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = mean_mfn), color = "steelblue", linewidth = 1) +
  geom_ribbon(aes(ymin = mean_margin - sd_margin,
                  ymax = mean_margin + sd_margin),
              fill = "coral", alpha = 0.2) +
  geom_line(aes(y = mean_margin), color = "coral", linewidth = 1) +
  annotate("text", x = 2018, y = agg_year$mean_mfn[agg_year$year == 2018] + 2,
           label = "MFN", color = "steelblue", fontface = "bold", size = 3.5) +
  annotate("text", x = 2018,
           y = agg_year$mean_margin[agg_year$year == 2018] + 2,
           label = "Pref. margin", color = "coral", fontface = "bold", size = 3.5) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2016.5, y = max(agg_year$mean_mfn + agg_year$sd_mfn, na.rm = TRUE),
           label = "2016", color = "grey40", size = 3, hjust = 0) +
  labs(
    title = "Trade Openness and Preferential Depth, 1990-2022",
    subtitle = "Cross-country mean (line) +/- 1 SD (ribbon). Dashed line = 2016 (Brexit/Trump).",
    x = "Year", y = "Tariff rate (%)"
  ) +
  scale_x_continuous(breaks = seq(1990, 2022, 4)) +
  coord_cartesian(ylim = c(-5, 35))

ggsave(file.path(outdir, "fig_trade_timeseries.png"), p7a,
       width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, "fig_trade_timeseries.pdf"), p7a,
       width = 8, height = 5)
cat("Saved fig_trade_timeseries\n")

# ============================================================
# Figure 7b: Scatter MFN vs Margin (latest period, 2019-2022)
# ============================================================
latest <- panel %>% filter(period == 7)
latest_key <- latest %>% filter(reporter_iso3 %in% key_countries)

# Median lines for quadrant classification
mfn_med <- median(latest$mfn_mean, na.rm = TRUE)
margin_med <- median(latest$margin_mean, na.rm = TRUE)

p7b <- ggplot(latest, aes(x = mfn_mean, y = margin_mean)) +
  geom_hline(yintercept = margin_med, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = mfn_med, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey60", alpha = 0.5, size = 2) +
  geom_point(data = latest_key, color = "steelblue", size = 3) +
  ggrepel::geom_text_repel(
    data = latest_key,
    aes(label = reporter_iso3),
    size = 3, color = "steelblue", max.overlaps = 20
  ) +
  # Quadrant labels
  annotate("text", x = mfn_med / 3, y = max(latest$margin_mean, na.rm = TRUE) * 0.95,
           label = "Universally\nopen + high margin",
           color = "grey40", size = 2.8, fontface = "italic") +
  annotate("text", x = max(latest$mfn_mean, na.rm = TRUE) * 0.8,
           y = max(latest$margin_mean, na.rm = TRUE) * 0.95,
           label = "Club",
           color = "coral", size = 3.5, fontface = "bold") +
  annotate("text", x = mfn_med / 3, y = 0.5,
           label = "Universally\nopen",
           color = "steelblue", size = 3.5, fontface = "bold") +
  annotate("text", x = max(latest$mfn_mean, na.rm = TRUE) * 0.8, y = 0.5,
           label = "Closed",
           color = "firebrick", size = 3.5, fontface = "bold") +
  labs(
    title = "Trade Typology: MFN Tariff vs. Preferential Margin (2019-2022)",
    subtitle = "Dashed lines = median. Key countries labeled.",
    x = "Mean MFN tariff (%)",
    y = "Mean preferential margin (%)"
  )

ggsave(file.path(outdir, "fig_trade_scatter.png"), p7b,
       width = 8, height = 6, dpi = 300)
ggsave(file.path(outdir, "fig_trade_scatter.pdf"), p7b,
       width = 8, height = 6)
cat("Saved fig_trade_scatter\n")

# ============================================================
# Figure 7c: Key country trajectories (MFN, year by year)
# ============================================================
yearly_key <- yearly %>%
  filter(reporter_iso3 %in% key_countries, year >= 1990, year <= 2022)

p7c <- ggplot(yearly_key, aes(x = year, y = mfn_mean,
                               color = reporter_iso3,
                               group = reporter_iso3)) +
  geom_line(linewidth = 0.7) +
  ggrepel::geom_text_repel(
    data = yearly_key %>% group_by(reporter_iso3) %>%
      filter(year == max(year)),
    aes(label = reporter_iso3),
    size = 3, direction = "y", nudge_x = 0.5, segment.size = 0.3,
    max.overlaps = 20
  ) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "grey40") +
  labs(
    title = "MFN Tariff Trajectories, Selected Countries (1990-2022)",
    subtitle = "Dashed line = 2016",
    x = "Year", y = "Mean MFN tariff (%)"
  ) +
  scale_x_continuous(breaks = seq(1990, 2022, 4)) +
  theme(legend.position = "none")

ggsave(file.path(outdir, "fig_trade_trajectories_mfn.png"), p7c,
       width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, "fig_trade_trajectories_mfn.pdf"), p7c,
       width = 8, height = 5)
cat("Saved fig_trade_trajectories_mfn\n")

# ============================================================
# Figure 7d: Key country trajectories (Margin, year by year)
# ============================================================
p7d <- ggplot(yearly_key %>% filter(!is.na(margin_mean)),
              aes(x = year, y = margin_mean,
                  color = reporter_iso3,
                  group = reporter_iso3)) +
  geom_line(linewidth = 0.7) +
  ggrepel::geom_text_repel(
    data = yearly_key %>% filter(!is.na(margin_mean)) %>%
      group_by(reporter_iso3) %>% filter(year == max(year)),
    aes(label = reporter_iso3),
    size = 3, direction = "y", nudge_x = 0.5, segment.size = 0.3,
    max.overlaps = 20
  ) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "grey40") +
  labs(
    title = "Preferential Margin Trajectories, Selected Countries (1990-2022)",
    subtitle = "Dashed line = 2016. Margin = MFN - best preferential tariff.",
    x = "Year", y = "Mean preferential margin (%)"
  ) +
  scale_x_continuous(breaks = seq(1990, 2022, 4)) +
  theme(legend.position = "none")

ggsave(file.path(outdir, "fig_trade_trajectories_margin.png"), p7d,
       width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, "fig_trade_trajectories_margin.pdf"), p7d,
       width = 8, height = 5)
cat("Saved fig_trade_trajectories_margin\n")

cat("\n=== ALL FIGURES DONE ===\n")
