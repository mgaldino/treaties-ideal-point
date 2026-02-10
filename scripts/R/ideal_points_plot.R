library(here)
library(purrr)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)

list.files(here("data", "processed", "ideal points"))

spec <- tribble(
  ~domain,            ~file,
  "investment",       "investment_results.rds",
  "arm control",      "arms_control_results.rds",
  "IP",               "intellectual_property_results.rds",
  "human_rights",     "human_rights_results.rds",
  "environment",      "environment_results.rds",
  "security",         "security_results.rds"
)


ideal_point <- spec %>%
  mutate(
    obj = map(file, ~ readRDS(here("data", "processed", "ideal points", .x))),
    data = map(obj, ~{
      arr  <- .x$ideal_points
      labs <- .x$period_labels
      iso3 <- .x$country_codes
      
      stopifnot(dim(arr)[3] == length(labs))
      stopifnot(length(iso3) == dim(arr)[1])
      
      map_dfr(seq_len(dim(arr)[2]), function(j) {
        mat <- arr[, j, ]  # (n_units x n_periods)
        
        as_tibble(mat, .name_repair = ~ as.character(labs)) %>%
          mutate(
            unit_id = row_number(),
            iso3 = iso3,
            .before = 1
          ) %>%
          pivot_longer(
            cols = -c(unit_id, iso3),
            names_to = "period_labels",
            values_to = "ideal_point"
          ) %>%
          mutate(dim = paste0("dim", j), .before = 1)
      })
    })
  ) %>%
  select(domain, data) %>%
  unnest(data) %>%
  mutate(type = "flow") %>%
  pivot_wider(
    names_from  = dim,
    values_from = ideal_point
  )

# investment
ideal_point %>%
  filter(domain == "investment") %>%
  ggplot(aes(dim1, dim2, label = iso3)) + geom_text(position = position_jitter(width = 0.03, height = 0.03),
                                                    alpha = 0.6,
                                                    size = 3) +
  facet_wrap(~ period_labels)

# hr
ideal_point %>%
  filter(domain == "human_rights") %>%
  ggplot(aes(dim1, dim2, label = iso3)) + geom_text(position = position_jitter(width = 0.03, height = 0.03),
                                                    alpha = 0.6,
                                                    size = 3) +
  facet_wrap(~ period_labels)

# arm control
ideal_point %>%
  filter(domain == "arm control") %>%
  ggplot(aes(dim1, dim2, label = iso3)) + geom_text(position = position_jitter(width = 0.03, height = 0.03),
                                                    alpha = 0.6,
                                                    size = 3) +
  facet_wrap(~ period_labels)

# IP
ideal_point %>%
  filter(domain == "IP") %>%
  ggplot(aes(dim1, dim2, label = iso3)) + geom_text(position = position_jitter(width = 0.03, height = 0.03),
                                                    alpha = 0.6,
                                                    size = 3) +
  facet_wrap(~ period_labels)

# environment
ideal_point %>%
  filter(domain == "environment") %>%
  ggplot(aes(dim1, dim2, label = iso3)) + geom_text(position = position_jitter(width = 0.03, height = 0.03),
                                                    alpha = 0.6,
                                                    size = 3) +
  facet_wrap(~ period_labels)

# security
ideal_point %>%
  filter(domain == "security") %>%
  ggplot(aes(dim1, dim2, label = iso3)) + geom_text(position = position_jitter(width = 0.03, height = 0.03),
                                                    alpha = 0.6,
                                                    size = 3) +
  facet_wrap(~ period_labels)


