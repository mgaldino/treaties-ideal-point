library(here)
library(dplyr)
library(tidyr)
library(readr)

# === Treaty size distribution (STOCK: all-time unique countries per treaty) ===
# For arms_control, human_rights, intellectual_property: use raw CSVs (all-time)
# For investment, security, trade, environment: use baseline_events (bilateral = stock)

# Arms control (raw, all-time)
ac <- read_csv(here("data", "raw", "arms_control", "arms_control_ratifications.csv"),
               show_col_types = FALSE) %>%
  filter(!is.na(action_date)) %>%
  group_by(treaty_id) %>%
  summarise(n_countries = n_distinct(country_name), .groups = "drop") %>%
  mutate(issue_area = "arms_control") %>%
  rename(base_treaty = treaty_id)

# Human rights (raw, all-time)
hr <- read_csv(here("data", "raw", "un_hr_treaties", "un_hr_ratifications.csv"),
               show_col_types = FALSE) %>%
  filter(!is.na(action_date)) %>%
  group_by(treaty_id) %>%
  summarise(n_countries = n_distinct(country_name), .groups = "drop") %>%
  mutate(issue_area = "human_rights") %>%
  rename(base_treaty = treaty_id)

# Intellectual property (raw, all-time)
ip <- read_csv(here("data", "raw", "wipo_treaties", "wipo_ratifications.csv"),
               show_col_types = FALSE) %>%
  filter(!is.na(action_date) | !is.na(entry_into_force_date)) %>%
  group_by(treaty_id) %>%
  summarise(n_countries = n_distinct(country_name), .groups = "drop") %>%
  mutate(issue_area = "intellectual_property") %>%
  rename(base_treaty = treaty_id)

# Investment, security, trade, environment from baseline_events
ev <- read_csv(here("data", "processed", "baseline_events.csv"),
               show_col_types = FALSE) %>%
  filter(issue_area %in% c("investment", "security", "trade", "environment")) %>%
  mutate(base_treaty = sub("__.*$", "", item_id)) %>%
  group_by(issue_area, base_treaty) %>%
  summarise(n_countries = n_distinct(iso3), .groups = "drop")

# Combine
all_data <- bind_rows(ac, hr, ip, ev)

# Classify treaty size
all_data <- all_data %>%
  mutate(
    size_class = case_when(
      n_countries == 1   ~ "1 country",
      n_countries == 2   ~ "Bilateral",
      n_countries == 3   ~ "Trilateral",
      n_countries <= 10  ~ "Mini-lateral (4-10)",
      n_countries <= 50  ~ "Medium (11-50)",
      n_countries > 50   ~ "Large (51+)"
    ),
    size_class = factor(size_class, levels = c(
      "1 country", "Bilateral", "Trilateral",
      "Mini-lateral (4-10)", "Medium (11-50)", "Large (51+)"
    ))
  )

# Summary table
summary_tbl <- all_data %>%
  group_by(issue_area, size_class, .drop = FALSE) %>%
  summarise(n_treaties = n(), .groups = "drop") %>%
  group_by(issue_area) %>%
  mutate(pct = sprintf("%.1f%%", 100 * n_treaties / sum(n_treaties))) %>%
  ungroup()

display <- summary_tbl %>%
  mutate(cell = paste0(n_treaties, " (", pct, ")")) %>%
  select(issue_area, size_class, cell) %>%
  pivot_wider(names_from = size_class, values_from = cell, values_fill = "0 (0.0%)")

totals <- all_data %>% count(issue_area, name = "Total")
display <- display %>% left_join(totals, by = "issue_area")

cat("\n=== Treaty Size Distribution (STOCK: all-time unique countries) ===\n\n")
print(as.data.frame(display), row.names = FALSE, right = FALSE)

# Detail per domain
for (area in c("arms_control", "human_rights", "intellectual_property")) {
  cat(sprintf("\n=== %s ===\n", area))
  sub <- all_data %>% filter(issue_area == area) %>% arrange(desc(n_countries))
  for (i in seq_len(nrow(sub))) {
    cat(sprintf("  %s: %d countries\n", sub$base_treaty[i], sub$n_countries[i]))
  }
}
