suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required. Please install it with install.packages('stringr').")
  }
  library(dplyr)
  library(readr)
  library(stringr)
})

out_dir <- "outputs/eda_phase2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

issue_areas <- c("trade", "investment", "security", "environment")

anchor_countries <- c("DNK", "IRN")

country_rows <- list()
for (area in issue_areas) {
  path <- file.path(out_dir, paste0("pca_country_loadings_", area, ".csv"))
  if (!file.exists(path)) {
    next
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  df <- df %>% dplyr::arrange(dplyr::desc(pc1))
  df$rank_desc <- seq_len(nrow(df))
  n <- nrow(df)
  for (iso in anchor_countries) {
    row <- df %>% dplyr::filter(iso3 == iso)
    if (nrow(row) == 0) {
      country_rows[[length(country_rows) + 1]] <- tibble::tibble(
        issue_area = area,
        iso3 = iso,
        pc1 = NA_real_,
        rank_desc = NA_integer_,
        n_countries = n
      )
    } else {
      country_rows[[length(country_rows) + 1]] <- tibble::tibble(
        issue_area = area,
        iso3 = iso,
        pc1 = row$pc1[1],
        rank_desc = row$rank_desc[1],
        n_countries = n
      )
    }
  }
}

country_summary <- dplyr::bind_rows(country_rows) %>%
  dplyr::mutate(
    percentile_from_top = ifelse(!is.na(rank_desc) & n_countries > 1,
                                 1 - (rank_desc - 1) / (n_countries - 1),
                                 NA_real_),
    sign = dplyr::case_when(
      is.na(pc1) ~ NA_character_,
      pc1 > 0 ~ "+",
      pc1 < 0 ~ "-",
      TRUE ~ "0"
    )
  )

readr::write_csv(country_summary, file.path(out_dir, "anchor_check_countries.csv"))

# Item anchors
item_codebook_path <- "data/processed/item_codebook.csv"
item_loadings_path <- file.path(out_dir, "pca_item_loadings_trade.csv")
item_rows <- list()

if (file.exists(item_codebook_path) && file.exists(item_loadings_path)) {
  items <- readr::read_csv(item_codebook_path, show_col_types = FALSE)
  trade_items <- items %>% dplyr::filter(issue_area == "trade")
  loadings_trade <- readr::read_csv(item_loadings_path, show_col_types = FALSE)

  # WTO accession
  wto_phantom <- loadings_trade %>%
    dplyr::filter(str_detect(item, "^wto_accession__"))
  if (nrow(wto_phantom) > 0) {
    item_rows[[length(item_rows) + 1]] <- tibble::tibble(
      issue_area = "trade",
      anchor_item = "wto_accession",
      n_phantom = nrow(wto_phantom),
      pc1_mean = mean(wto_phantom$pc1, na.rm = TRUE),
      pc1_min = min(wto_phantom$pc1, na.rm = TRUE),
      pc1_max = max(wto_phantom$pc1, na.rm = TRUE)
    )
  }
}

# Paris Agreement in environment
env_loadings_path <- file.path(out_dir, "pca_item_loadings_environment.csv")
if (file.exists(item_codebook_path) && file.exists(env_loadings_path)) {
  items <- readr::read_csv(item_codebook_path, show_col_types = FALSE)
  env_items <- items %>% dplyr::filter(issue_area == "environment")
  paris_candidates <- env_items %>%
    dplyr::filter(str_detect(tolower(item_name), "paris"))

  env_loadings <- readr::read_csv(env_loadings_path, show_col_types = FALSE)

  if (nrow(paris_candidates) > 0) {
    for (i in seq_len(nrow(paris_candidates))) {
      item_id <- paris_candidates$item_id[i]
      phantom <- env_loadings %>%
        dplyr::filter(str_detect(item, paste0("^", item_id, "__")))
      if (nrow(phantom) > 0) {
        item_rows[[length(item_rows) + 1]] <- tibble::tibble(
          issue_area = "environment",
          anchor_item = paris_candidates$item_name[i],
          n_phantom = nrow(phantom),
          pc1_mean = mean(phantom$pc1, na.rm = TRUE),
          pc1_min = min(phantom$pc1, na.rm = TRUE),
          pc1_max = max(phantom$pc1, na.rm = TRUE)
        )
      }
    }
  } else {
    item_rows[[length(item_rows) + 1]] <- tibble::tibble(
      issue_area = "environment",
      anchor_item = "PARIS_NOT_FOUND",
      n_phantom = NA_integer_,
      pc1_mean = NA_real_,
      pc1_min = NA_real_,
      pc1_max = NA_real_
    )
  }
}

item_summary <- dplyr::bind_rows(item_rows)
if (nrow(item_summary) > 0) {
  readr::write_csv(item_summary, file.path(out_dir, "anchor_check_items.csv"))
}

cat("Anchor checks written to outputs/eda_phase2/anchor_check_countries.csv and anchor_check_items.csv\n")
