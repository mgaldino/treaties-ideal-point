#!/usr/bin/env Rscript
# V7 Block A — Report Plots
# Generates key figures for the Block A summary report

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
})

out_dir <- "outputs/v7_report"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom"
  ))

domain_labels <- c(
  arms_control = "Arms Control",
  environment = "Environment",
  human_rights = "Human Rights",
  intellectual_property = "Intell. Property",
  investment = "Investment",
  security = "Security"
)

# ── Figure 1: UNGA vs V-Dem correlations (dot plot) ──────────────────────

corr <- read_csv("outputs/v7_validation/correlation_table.csv", show_col_types = FALSE)
overall <- corr %>% filter(period == 0)

fig1_data <- overall %>%
  select(domain, r_unga_dim1, r_vdem_dim1) %>%
  pivot_longer(cols = c(r_unga_dim1, r_vdem_dim1),
               names_to = "measure", values_to = "correlation") %>%
  mutate(
    measure = ifelse(measure == "r_unga_dim1", "UNGA Voting", "V-Dem Liberal Democracy"),
    domain_label = domain_labels[domain]
  ) %>%
  arrange(desc(correlation))

# Order domains by V-Dem correlation
domain_order <- overall %>% arrange(r_vdem_dim1) %>% pull(domain)
fig1_data$domain_label <- factor(fig1_data$domain_label,
  levels = domain_labels[domain_order])

p1 <- ggplot(fig1_data, aes(x = correlation, y = domain_label, color = measure, shape = measure)) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5) +
  geom_vline(xintercept = c(-0.3, 0.3), color = "grey85", linetype = "dashed", linewidth = 0.4) +
  geom_point(size = 4) +
  scale_color_manual(values = c("UNGA Voting" = "#2166ac", "V-Dem Liberal Democracy" = "#b2182b")) +
  scale_shape_manual(values = c("UNGA Voting" = 16, "V-Dem Liberal Democracy" = 17)) +
  labs(
    title = "Validation: Dim1 Correlation with External Benchmarks",
    subtitle = "V-Dem outperforms UNGA in 3 of 6 domains; IP is the exception",
    x = "Pearson Correlation with Dim1 Ideal Points",
    y = NULL,
    color = NULL, shape = NULL
  ) +
  xlim(-0.5, 0.8) +
  annotate("text", x = 0.3, y = 0.6, label = "r = 0.3", color = "grey60", size = 3, hjust = -0.1)

ggsave(file.path(out_dir, "fig1_validation_unga_vdem.png"), p1, width = 8, height = 4.5, dpi = 200)
cat("Saved fig1\n")

# ── Figure 2: Flow vs Stock temporal trends ──────────────────────────────

comparison <- read_csv("outputs/v7_flow_vs_stock/comparison_table.csv", show_col_types = FALSE)

fig2_data <- comparison %>%
  select(domain, period, mean_dim1_flow, mean_dim1_stock) %>%
  pivot_longer(cols = c(mean_dim1_flow, mean_dim1_stock),
               names_to = "coding", values_to = "mean_dim1") %>%
  mutate(
    coding = ifelse(coding == "mean_dim1_flow", "Flow", "Stock (Procrustes-aligned)"),
    domain_label = domain_labels[domain],
    period_num = as.integer(factor(period))
  )

p2 <- ggplot(fig2_data, aes(x = period, y = mean_dim1, color = coding, group = coding)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ domain_label, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("Flow" = "#2166ac", "Stock (Procrustes-aligned)" = "#b2182b")) +
  labs(
    title = "Temporal Trends: Flow vs Stock Coding",
    subtitle = "Stock coding reverses trend direction in 4 of 6 domains",
    x = NULL,
    y = "Mean Dim1 Ideal Point",
    color = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "fig2_flow_vs_stock_trends.png"), p2, width = 10, height = 6, dpi = 200)
cat("Saved fig2\n")

# ── Figure 3: Per-period flow-stock correlation ──────────────────────────

fig3_data <- comparison %>%
  mutate(domain_label = domain_labels[domain]) %>%
  select(domain_label, period, r_dim1)

p3 <- ggplot(fig3_data, aes(x = period, y = r_dim1, color = domain_label, group = domain_label)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Flow-Stock Agreement: Per-Period Correlation of Dim1",
    subtitle = "Only Environment and Investment are robust (r > 0.9); Arms Control and HR are inverted",
    x = NULL,
    y = "Pearson r (Flow vs Stock, Dim1)",
    color = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  annotate("text", x = "2015-2018", y = 0.92, label = "r = 0.9", color = "grey50", size = 3, hjust = 1.1)

ggsave(file.path(out_dir, "fig3_flow_stock_correlation.png"), p3, width = 8, height = 5, dpi = 200)
cat("Saved fig3\n")

# ── Figure 4: Aggregate trends (flow only) with SD ribbon ───────────────

trends <- read_csv("outputs/v7_validation/aggregate_trends.csv", show_col_types = FALSE)

fig4_data <- trends %>%
  mutate(
    domain_label = domain_labels[domain],
    upper = mean_dim1 + sd_dim1,
    lower = mean_dim1 - sd_dim1
  )

p4 <- ggplot(fig4_data, aes(x = period_label, y = mean_dim1, group = 1)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#2166ac", alpha = 0.15) +
  geom_line(color = "#2166ac", linewidth = 1) +
  geom_point(color = "#2166ac", size = 2.5) +
  facet_wrap(~ domain_label, scales = "free_y", ncol = 3) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  labs(
    title = "Dim1 Temporal Trends by Domain (Flow Coding)",
    subtitle = "Ribbon = +/- 1 SD. Investment shows clear growth; Environment declines with rising dispersion",
    x = NULL,
    y = "Mean Dim1 +/- 1 SD"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "fig4_temporal_trends.png"), p4, width = 10, height = 6, dpi = 200)
cat("Saved fig4\n")

cat("All figures saved to", out_dir, "\n")
