suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  library(dplyr)
  library(readr)
})

out_dir <- "outputs/eda_phase2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018")

# Helper to rebuild PCA from flow matrices (same as in 02_eda.R)
run_pca_from_flow <- function(area) {
  path <- file.path("data/processed", paste0(area, "_flow_matrix.rds"))
  flow <- readRDS(path)
  rc <- flow$rc

  rc_pca <- rc
  rc_pca[rc_pca == 0] <- NA
  rc_pca[rc_pca == -1] <- 0

  for (j in seq_len(ncol(rc_pca))) {
    col_mean <- mean(rc_pca[, j], na.rm = TRUE)
    rc_pca[is.na(rc_pca[, j]), j] <- col_mean
  }

  pca <- prcomp(rc_pca, center = TRUE, scale. = TRUE)
  list(pca = pca, countries = flow$country_codes, items = flow$item_labels)
}

# Environment: DNK + SAU on PC1
env <- run_pca_from_flow("environment")
env_pc1 <- env$pca$x[, 1]

env_df <- tibble::tibble(
  iso3 = env$countries,
  pc1 = env_pc1
) %>%
  dplyr::arrange(dplyr::desc(pc1))

env_df$rank_desc <- seq_len(nrow(env_df))

env_focus <- env_df %>%
  dplyr::filter(iso3 %in% c("DNK", "SAU")) %>%
  dplyr::mutate(
    n_countries = nrow(env_df),
    percentile_from_top = 1 - (rank_desc - 1) / (n_countries - 1)
  )

readr::write_csv(env_focus, file.path(out_dir, "anchor_check_environment_dnk_sau.csv"))

# Trade: PC2 top/bottom 10, plus DNK/IRN positions
trade <- run_pca_from_flow("trade")
trade_pc2 <- trade$pca$x[, 2]

trade_df <- tibble::tibble(
  iso3 = trade$countries,
  pc2 = trade_pc2
) %>%
  dplyr::arrange(dplyr::desc(pc2))

trade_df$rank_desc <- seq_len(nrow(trade_df))

trade_top10 <- trade_df %>% dplyr::slice_head(n = 10)
trade_bottom10 <- trade_df %>% dplyr::slice_tail(n = 10)
trade_focus <- trade_df %>%
  dplyr::filter(iso3 %in% c("DNK", "IRN")) %>%
  dplyr::mutate(
    n_countries = nrow(trade_df),
    percentile_from_top = 1 - (rank_desc - 1) / (n_countries - 1)
  )

readr::write_csv(trade_top10, file.path(out_dir, "trade_pc2_top10.csv"))
readr::write_csv(trade_bottom10, file.path(out_dir, "trade_pc2_bottom10.csv"))
readr::write_csv(trade_focus, file.path(out_dir, "trade_pc2_dnk_irn.csv"))

cat("Anchor updates written to outputs/eda_phase2/ (environment DNK/SAU; trade PC2 top/bottom + DNK/IRN).\n")
