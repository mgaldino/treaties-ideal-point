library(here)
library(purrr)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Configuration -----------------------------------------------------------

spec <- tribble(
  ~domain,                ~domain_label,              ~file,
  "investment",           "Investment",               "investment_results.rds",
  "arms_control",         "Arms Control",             "arms_control_results.rds",
  "intellectual_property", "Intellectual Property",    "intellectual_property_results.rds",
  "human_rights",         "Human Rights",             "human_rights_results.rds",
  "environment",          "Environment",              "environment_results.rds",
  "security",             "Security",                 "security_results.rds"
)

# V7 anchors per domain: dim1_pos, dim1_neg, dim2
anchors <- tribble(
  ~domain,                ~anchor_iso3,
  "investment",           c("DNK", "IRN", "CHN"),
  "arms_control",         c("NZL", "ISR", "IND"),
  "intellectual_property", c("DNK", "AGO", "BRA"),
  "human_rights",         c("DNK", "PRK", "USA"),
  "environment",          c("DNK", "SAU", "AUS"),
  "security",             c("DNK", "IRN", "UKR")
)

# Tax havens / offshore financial centers (highlight for investment & IP)
tax_havens <- c("BHS", "BMU", "VGB", "CYM", "CHE", "SGP", "HKG", "LUX",
                "IRL", "NLD", "PAN", "MUS", "BRB", "BLZ", "LIE", "MCO",
                "AND", "MLT", "CYP", "JEY", "GGY", "IMN", "ABW", "CUW",
                "VUT", "WSM", "COK", "MHL", "BHR", "MAC", "GIB", "SMR")

# Nuclear powers (highlight for arms_control & security)
nuclear_powers <- c("USA", "RUS", "GBR", "FRA", "CHN", "IND", "PAK", "ISR", "PRK")

# --- Data wrangling -----------------------------------------------------------

results_dir <- here("outputs", "v7_country_anchors")
fig_dir     <- here("outputs", "paper_figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

ideal_point <- spec %>%
  mutate(
    obj = map(file, ~ readRDS(file.path(results_dir, .x))),
    data = map(obj, ~{
      arr  <- .x$ideal_points
      labs <- .x$period_labels
      iso3 <- .x$country_codes

      stopifnot(dim(arr)[3] == length(labs))
      stopifnot(length(iso3) == dim(arr)[1])

      map_dfr(seq_len(dim(arr)[2]), function(j) {
        mat <- arr[, j, ]

        as_tibble(mat, .name_repair = ~ as.character(labs)) %>%
          mutate(
            unit_id = row_number(),
            iso3 = iso3,
            .before = 1
          ) %>%
          pivot_longer(
            cols = -c(unit_id, iso3),
            names_to = "period",
            values_to = "ideal_point"
          ) %>%
          mutate(dim = paste0("dim", j), .before = 1)
      })
    })
  ) %>%
  select(domain, domain_label, data) %>%
  unnest(data) %>%
  mutate(type = "flow") %>%
  pivot_wider(
    names_from  = dim,
    values_from = ideal_point
  )

# Merge anchor flags
anchor_lookup <- anchors %>%
  unnest_longer(anchor_iso3) %>%
  rename(iso3 = anchor_iso3) %>%
  mutate(is_anchor = TRUE)

ideal_point <- ideal_point %>%
  left_join(anchor_lookup, by = c("domain", "iso3")) %>%
  mutate(is_anchor = coalesce(is_anchor, FALSE))

# Add special group flags
ideal_point <- ideal_point %>%
  mutate(
    is_tax_haven    = iso3 %in% tax_havens,
    is_nuclear      = iso3 %in% nuclear_powers
  )

# --- Theme --------------------------------------------------------------------

theme_ideal <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle    = element_text(size = 10, color = "grey40", hjust = 0),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9),
    legend.text      = element_text(size = 8),
    plot.margin      = margin(10, 15, 10, 10)
  )

# --- Plotting function --------------------------------------------------------

plot_domain <- function(df, domain_key, domain_label, highlight_group = "none",
                        subtitle_extra = NULL) {

  dd <- df %>% filter(domain == domain_key)

  # Determine highlight column
  dd <- dd %>%
    mutate(
      highlight = case_when(
        is_anchor ~ "Anchor",
        highlight_group == "tax_haven" & is_tax_haven ~ "Tax haven",
        highlight_group == "nuclear" & is_nuclear ~ "Nuclear power",
        TRUE ~ "Other"
      ),
      highlight = factor(highlight, levels = c("Other", "Anchor",
                                               "Tax haven", "Nuclear power"))
    )

  # Label only anchors + highlighted countries
  dd <- dd %>%
    mutate(
      label = case_when(
        is_anchor ~ iso3,
        highlight_group == "tax_haven" & is_tax_haven ~ iso3,
        highlight_group == "nuclear" & is_nuclear ~ iso3,
        TRUE ~ ""
      )
    )

  # Color palette
  pal <- c("Other" = "grey70", "Anchor" = "#D62828",
           "Tax haven" = "#F77F00", "Nuclear power" = "#023E8A")
  size_map  <- c("Other" = 1.5, "Anchor" = 2.5, "Tax haven" = 2.2, "Nuclear power" = 2.2)
  alpha_map <- c("Other" = 0.25, "Anchor" = 0.9, "Tax haven" = 0.8, "Nuclear power" = 0.8)

  subtitle <- paste0("2D dynamic IRT (flow coding, 5-year periods)")
  if (!is.null(subtitle_extra)) subtitle <- paste0(subtitle, " | ", subtitle_extra)

  p <- ggplot(dd, aes(x = dim1, y = dim2, color = highlight, size = highlight, alpha = highlight)) +
    geom_point() +
    geom_text(
      aes(label = label),
      size = 2.8,
      fontface = "bold",
      vjust = -0.8,
      show.legend = FALSE
    ) +
    scale_color_manual(values = pal, name = NULL, drop = FALSE,
                       breaks = intersect(c("Anchor", "Tax haven", "Nuclear power"),
                                          levels(dd$highlight)[levels(dd$highlight) %in% unique(dd$highlight)])) +
    scale_size_manual(values = size_map, guide = "none") +
    scale_alpha_manual(values = alpha_map, guide = "none") +
    facet_wrap(~ period, nrow = 2) +
    labs(
      title    = domain_label,
      subtitle = subtitle,
      x        = "Dimension 1 (ILO support)",
      y        = "Dimension 2"
    ) +
    theme_ideal +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

  p
}

# --- Generate all plots -------------------------------------------------------

# Investment: highlight tax havens
p_inv <- plot_domain(ideal_point, "investment", "Investment",
                     highlight_group = "tax_haven",
                     subtitle_extra = "Anchors: DNK (+), IRN (\u2212), CHN (dim2)")
ggsave(file.path(fig_dir, "fig_investment_2d.png"), p_inv,
       width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "fig_investment_2d.pdf"), p_inv,
       width = 10, height = 7, bg = "white")

# Security: highlight nuclear powers
p_sec <- plot_domain(ideal_point, "security", "Security",
                     highlight_group = "nuclear",
                     subtitle_extra = "Anchors: DNK (+), IRN (\u2212), UKR (dim2)")
ggsave(file.path(fig_dir, "fig_security_2d.png"), p_sec,
       width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "fig_security_2d.pdf"), p_sec,
       width = 10, height = 7, bg = "white")

# Environment: no special highlight
p_env <- plot_domain(ideal_point, "environment", "Environment",
                     subtitle_extra = "Anchors: DNK (+), SAU (\u2212), AUS (dim2)")
ggsave(file.path(fig_dir, "fig_environment_2d.png"), p_env,
       width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "fig_environment_2d.pdf"), p_env,
       width = 10, height = 7, bg = "white")

# Human Rights: no special highlight
p_hr <- plot_domain(ideal_point, "human_rights", "Human Rights",
                    subtitle_extra = "Anchors: DNK (+), PRK (\u2212), USA (dim2)")
ggsave(file.path(fig_dir, "fig_human_rights_2d.png"), p_hr,
       width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "fig_human_rights_2d.pdf"), p_hr,
       width = 10, height = 7, bg = "white")

# Arms Control: highlight nuclear powers
p_ac <- plot_domain(ideal_point, "arms_control", "Arms Control",
                    highlight_group = "nuclear",
                    subtitle_extra = "Anchors: NZL (+), ISR (\u2212), IND (dim2)")
ggsave(file.path(fig_dir, "fig_arms_control_2d.png"), p_ac,
       width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "fig_arms_control_2d.pdf"), p_ac,
       width = 10, height = 7, bg = "white")

# Intellectual Property: highlight tax havens
p_ip <- plot_domain(ideal_point, "intellectual_property", "Intellectual Property",
                    highlight_group = "tax_haven",
                    subtitle_extra = "Anchors: DNK (+), AGO (\u2212), BRA (dim2)")
ggsave(file.path(fig_dir, "fig_intellectual_property_2d.png"), p_ip,
       width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "fig_intellectual_property_2d.pdf"), p_ip,
       width = 10, height = 7, bg = "white")

cat("All figures saved to:", fig_dir, "\n")
