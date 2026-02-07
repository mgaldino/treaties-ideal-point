#!/usr/bin/env Rscript
# Build data for the V5 convergence report (investment domain).
# Usage: Rscript scripts/R/v5_convergence_report_data.R [domain] [summary_csv] [country_diag_csv] [output_rds]

args <- commandArgs(trailingOnly = TRUE)

domain <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "investment"
summary_csv <- if (length(args) >= 2 && nzchar(args[2])) {
  args[2]
} else {
  "outputs/v5_per_domain_2d_diag/diagnostic_summary.csv"
}
country_diag_csv <- if (length(args) >= 3 && nzchar(args[3])) {
  args[3]
} else {
  file.path("outputs/v5_per_domain_2d_diag",
            paste0(domain, "_country_diagnostics_4500_to_5000.csv"))
}
output_rds <- if (length(args) >= 4 && nzchar(args[4])) {
  args[4]
} else {
  file.path("outputs/v5_per_domain_2d_diag",
            paste0(domain, "_convergence_report_data.rds"))
}

if (!file.exists(summary_csv)) stop("Missing: ", summary_csv)
if (!file.exists(country_diag_csv)) stop("Missing: ", country_diag_csv)

summary_df <- read.csv(summary_csv, stringsAsFactors = FALSE)
summary_df <- summary_df[summary_df$domain == domain, , drop = FALSE]
if (nrow(summary_df) == 0) stop("No rows for domain: ", domain)

summary_df$iter_prev <- as.integer(summary_df$iter_prev)
summary_df$iter_curr <- as.integer(summary_df$iter_curr)
summary_df$loglik_delta <- as.numeric(summary_df$loglik_delta)
summary_df$p95_abs_mean <- as.numeric(summary_df$p95_abs_mean)
summary_df$max_abs_mean <- as.numeric(summary_df$max_abs_mean)
summary_df$cor_dim1 <- as.numeric(summary_df$cor_dim1)
summary_df$cor_dim2 <- as.numeric(summary_df$cor_dim2)
summary_df$cor_mean_dim1 <- as.numeric(summary_df$cor_mean_dim1)
summary_df$cor_mean_dim2 <- as.numeric(summary_df$cor_mean_dim2)

summary_df <- summary_df[order(summary_df$iter_prev), , drop = FALSE]
summary_df$cor_min <- pmin(summary_df$cor_dim1, summary_df$cor_dim2, na.rm = TRUE)
summary_df$cor_mean_min <- pmin(summary_df$cor_mean_dim1, summary_df$cor_mean_dim2, na.rm = TRUE)

summary_table <- summary_df[, c(
  "iter_prev", "iter_curr", "loglik_delta",
  "p95_abs_mean", "max_abs_mean", "cor_mean_min",
  "stable_both"
), drop = FALSE]

latest <- summary_df[nrow(summary_df), , drop = FALSE]

country_df <- read.csv(country_diag_csv, stringsAsFactors = FALSE)
country_df$pos_share <- with(country_df, ifelse((n_pos + n_neg) > 0, n_pos / (n_pos + n_neg), NA_real_))
country_df <- country_df[order(country_df$delta_l2, decreasing = TRUE), , drop = FALSE]
top10 <- head(country_df, 10)

report_data <- list(
  domain = domain,
  summary_table = summary_table,
  latest = latest,
  top10 = top10,
  summary_csv = summary_csv,
  country_diag_csv = country_diag_csv,
  date_access = as.character(Sys.Date())
)

saveRDS(report_data, output_rds)
cat(sprintf("Saved report data: %s\n", output_rds))
