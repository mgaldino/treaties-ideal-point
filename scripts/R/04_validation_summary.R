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

out_dir <- "outputs/validation"
areas <- c("investment", "security", "environment")

summaries <- list()

for (area in areas) {
  path <- file.path(out_dir, paste0("unga_correlations_by_period_", area, ".csv"))
  if (!file.exists(path)) {
    stop("Missing file: ", path)
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  df <- df %>% dplyr::filter(!is.na(correlation))
  summaries[[area]] <- tibble::tibble(
    issue_area = area,
    periods = nrow(df),
    corr_mean = mean(df$correlation),
    corr_min = min(df$correlation),
    corr_max = max(df$correlation),
    n_positive = sum(df$correlation > 0),
    n_negative = sum(df$correlation < 0)
  )
}

summary_df <- dplyr::bind_rows(summaries)
readr::write_csv(summary_df, file.path(out_dir, "unga_correlations_summary.csv"))

cat("Summary written to outputs/validation/unga_correlations_summary.csv\n")
