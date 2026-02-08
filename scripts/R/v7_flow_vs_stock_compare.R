#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

flow_dir <- "outputs/v7_country_anchors"
stock_dir <- "outputs/v7_stock_country_anchors"
out_dir <- "outputs/v7_flow_vs_stock"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

cor_safe <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok]))
}

procrustes_rotation <- function(X, Y) {
  # Orthogonal Procrustes: find R minimizing ||X R - Y||_F with R'R = I.
  # Uses SVD of t(X)Y. Returns 2x2 rotation/reflection matrix.
  ok <- stats::complete.cases(X, Y)
  Xo <- X[ok, , drop = FALSE]
  Yo <- Y[ok, , drop = FALSE]
  if (nrow(Xo) < 3) return(diag(ncol(X)))
  sv <- svd(t(Xo) %*% Yo)
  sv$u %*% t(sv$v)
}

get_domain_from_filename <- function(path) {
  sub("_results\\.rds$", "", basename(path))
}

compute_trend_flags <- function(flow_means, stock_means) {
  # Compare direction of adjacent period changes and overall first-to-last direction.
  d_flow <- diff(flow_means)
  d_stock <- diff(stock_means)

  sign_mismatch <- (sign(d_flow) != sign(d_stock)) & (sign(d_flow) != 0) & (sign(d_stock) != 0)
  any_mismatch <- any(sign_mismatch, na.rm = TRUE)

  overall_flow <- sign(tail(flow_means, 1) - head(flow_means, 1))
  overall_stock <- sign(tail(stock_means, 1) - head(stock_means, 1))
  overall_diff_dir <- (overall_flow != overall_stock) & (overall_flow != 0) & (overall_stock != 0)

  list(
    trend_cor = cor_safe(flow_means, stock_means),
    any_adjacent_direction_mismatch = any_mismatch,
    overall_direction_mismatch = overall_diff_dir
  )
}

extract_aligned <- function(flow, stock) {
  common_countries <- intersect(flow$country_codes, stock$country_codes)
  common_periods <- intersect(flow$period_labels, stock$period_labels)

  if (length(common_countries) == 0 || length(common_periods) == 0) {
    return(NULL)
  }

  i_flow_c <- match(common_countries, flow$country_codes)
  i_stock_c <- match(common_countries, stock$country_codes)
  i_flow_p <- match(common_periods, flow$period_labels)
  i_stock_p <- match(common_periods, stock$period_labels)

  list(
    countries = common_countries,
    periods = common_periods,
    i_flow_c = i_flow_c,
    i_stock_c = i_stock_c,
    i_flow_p = i_flow_p,
    i_stock_p = i_stock_p
  )
}

compute_domain <- function(domain, flow_path, stock_path) {
  flow <- safe_read_rds(flow_path)
  stock <- safe_read_rds(stock_path)

  if (is.null(flow) || is.null(stock)) {
    return(list(
      domain = domain,
      skipped = TRUE,
      reason = sprintf("Missing file(s): flow=%s stock=%s", file.exists(flow_path), file.exists(stock_path))
    ))
  }

  aligned <- extract_aligned(flow, stock)
  if (is.null(aligned)) {
    return(list(
      domain = domain,
      skipped = TRUE,
      reason = "No overlapping country_codes and/or period_labels between flow and stock objects"
    ))
  }

  # Per-period correlations + per-period mean(dim1) comparison
  rows <- vector("list", length(aligned$periods))
  flow_dim1_mat <- flow$ideal_points[aligned$i_flow_c, 1, aligned$i_flow_p, drop = FALSE]
  flow_dim2_mat <- flow$ideal_points[aligned$i_flow_c, 2, aligned$i_flow_p, drop = FALSE]
  stock_dim1_mat <- stock$ideal_points[aligned$i_stock_c, 1, aligned$i_stock_p, drop = FALSE]
  stock_dim2_mat <- stock$ideal_points[aligned$i_stock_c, 2, aligned$i_stock_p, drop = FALSE]

  # Convert to 2D matrices (countries x periods)
  flow_dim1_mat <- matrix(flow_dim1_mat, nrow = length(aligned$countries), ncol = length(aligned$periods))
  flow_dim2_mat <- matrix(flow_dim2_mat, nrow = length(aligned$countries), ncol = length(aligned$periods))
  stock_dim1_mat <- matrix(stock_dim1_mat, nrow = length(aligned$countries), ncol = length(aligned$periods))
  stock_dim2_mat <- matrix(stock_dim2_mat, nrow = length(aligned$countries), ncol = length(aligned$periods))

  # Align stock dims to flow dims (rotation/reflection) using mean ideal points.
  flow_mean <- flow$ideal_points_mean[aligned$i_flow_c, , drop = FALSE]
  stock_mean <- stock$ideal_points_mean[aligned$i_stock_c, , drop = FALSE]
  R <- procrustes_rotation(stock_mean, flow_mean)

  # Apply same alignment to each period.
  stock_aligned_dim1_mat <- stock_dim1_mat * NA_real_
  stock_aligned_dim2_mat <- stock_dim2_mat * NA_real_
  for (j in seq_along(aligned$periods)) {
    Xp <- cbind(stock_dim1_mat[, j], stock_dim2_mat[, j])
    Xp_aligned <- Xp %*% R
    stock_aligned_dim1_mat[, j] <- Xp_aligned[, 1]
    stock_aligned_dim2_mat[, j] <- Xp_aligned[, 2]
  }

  stock_mean_aligned <- stock_mean %*% R

  flow_mean_dim1_by_period <- colMeans(flow_dim1_mat, na.rm = TRUE)
  stock_mean_dim1_by_period <- colMeans(stock_aligned_dim1_mat, na.rm = TRUE)

  for (j in seq_along(aligned$periods)) {
    p <- aligned$periods[j]
    r1 <- cor_safe(flow_dim1_mat[, j], stock_aligned_dim1_mat[, j])
    r2 <- cor_safe(flow_dim2_mat[, j], stock_aligned_dim2_mat[, j])

    m_flow <- flow_mean_dim1_by_period[j]
    m_stock <- stock_mean_dim1_by_period[j]

    rows[[j]] <- data.frame(
      domain = domain,
      period = p,
      r_dim1 = r1,
      r_dim2 = r2,
      mean_dim1_flow = m_flow,
      mean_dim1_stock = m_stock,
      delta_mean = m_flow - m_stock
    )
  }

  comparison_table <- do.call(rbind, rows)

  # Overall correlations of mean ideal points (across countries), dim1 and dim2
  overall_r_dim1_mean <- cor_safe(flow_mean[, 1], stock_mean_aligned[, 1])
  overall_r_dim2_mean <- cor_safe(flow_mean[, 2], stock_mean_aligned[, 2])

  # Trend similarity of mean(dim1) series across periods
  trend <- compute_trend_flags(flow_mean_dim1_by_period, stock_mean_dim1_by_period)

  # Top discrepant countries (mean dim1)
  delta_country <- data.frame(
    domain = domain,
    country_code = aligned$countries,
    dim1_flow_mean = flow_mean[, 1],
    dim1_stock_mean = stock_mean_aligned[, 1]
  )
  delta_country$delta_dim1_mean <- delta_country$dim1_flow_mean - delta_country$dim1_stock_mean
  delta_country$abs_delta_dim1_mean <- abs(delta_country$delta_dim1_mean)
  delta_country <- delta_country[order(-delta_country$abs_delta_dim1_mean), , drop = FALSE]

  list(
    domain = domain,
    skipped = FALSE,
    comparison_table = comparison_table,
    overall_r_dim1_mean = overall_r_dim1_mean,
    overall_r_dim2_mean = overall_r_dim2_mean,
    trend = trend,
    discrepant = utils::head(delta_country, 10),
    discrepant_full = delta_country
  )
}

flow_files <- list.files(flow_dir, pattern = "_results\\.rds$", full.names = TRUE)
domains <- sort(unique(vapply(flow_files, get_domain_from_filename, character(1))))

results <- vector("list", length(domains))
names(results) <- domains

for (d in domains) {
  flow_path <- file.path(flow_dir, sprintf("%s_results.rds", d))
  stock_path <- file.path(stock_dir, sprintf("%s_results.rds", d))
  results[[d]] <- compute_domain(d, flow_path, stock_path)
}

all_tables <- do.call(
  rbind,
  lapply(results, function(x) if (!isTRUE(x$skipped)) x$comparison_table else NULL)
)

if (is.null(all_tables) || nrow(all_tables) == 0) {
  stop("No domains processed successfully (all skipped).")
}

comparison_csv <- file.path(out_dir, "comparison_table.csv")
utils::write.csv(all_tables, comparison_csv, row.names = FALSE, fileEncoding = "UTF-8")

top_discrepant <- do.call(
  rbind,
  lapply(results, function(x) if (!isTRUE(x$skipped)) x$discrepant else NULL)
)

discrepant_csv <- file.path(out_dir, "top_discrepant_countries.csv")
utils::write.csv(top_discrepant, discrepant_csv, row.names = FALSE, fileEncoding = "UTF-8")

report_path <- file.path(out_dir, "comparison_report.txt")
con <- file(report_path, open = "wt", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

writeLines("Flow vs Stock Coding Comparison (v7)", con = con)
writeLines(sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con = con)
writeLines("", con = con)
writeLines("Note on identification/alignment:", con = con)
writeLines("- Ideal-point dimensions can flip sign or rotate across independent estimations.", con = con)
writeLines("- For each domain, I aligned STOCK ideal points to FLOW using an orthogonal Procrustes rotation on country mean 2D ideal points.", con = con)
writeLines("- All correlations, trend checks, and discrepancy rankings below use the aligned STOCK coordinates.", con = con)
writeLines("", con = con)

skipped <- vapply(results, function(x) isTRUE(x$skipped), logical(1))
if (any(skipped)) {
  writeLines("Omissions (skipped domains due to missing inputs):", con = con)
  for (d in names(results)[skipped]) {
    writeLines(sprintf("- %s: %s", d, results[[d]]$reason), con = con)
  }
  writeLines("", con = con)
}

writeLines("Full Comparison Table (domain x period):", con = con)
writeLines("", con = con)

fmt_table <- all_tables
fmt_table$r_dim1 <- round(fmt_table$r_dim1, 3)
fmt_table$r_dim2 <- round(fmt_table$r_dim2, 3)
fmt_table$mean_dim1_flow <- round(fmt_table$mean_dim1_flow, 3)
fmt_table$mean_dim1_stock <- round(fmt_table$mean_dim1_stock, 3)
fmt_table$delta_mean <- round(fmt_table$delta_mean, 3)

capture.output(fmt_table, file = con)
writeLines("", con = con)

domain_summaries <- lapply(results[!skipped], function(x) {
  tab <- x$comparison_table
  list(
    domain = x$domain,
    min_r_dim1 = min(tab$r_dim1, na.rm = TRUE),
    mean_r_dim1 = mean(tab$r_dim1, na.rm = TRUE),
    overall_r_dim1_mean = x$overall_r_dim1_mean,
    overall_r_dim2_mean = x$overall_r_dim2_mean,
    trend_cor = x$trend$trend_cor,
    any_adjacent_direction_mismatch = x$trend$any_adjacent_direction_mismatch,
    overall_direction_mismatch = x$trend$overall_direction_mismatch
  )
})
domain_summary_df <- do.call(rbind, lapply(domain_summaries, as.data.frame))

writeLines("Domain-Level Diagnostics:", con = con)
writeLines("", con = con)
domain_summary_fmt <- domain_summary_df
domain_summary_fmt$min_r_dim1 <- round(domain_summary_fmt$min_r_dim1, 3)
domain_summary_fmt$mean_r_dim1 <- round(domain_summary_fmt$mean_r_dim1, 3)
domain_summary_fmt$overall_r_dim1_mean <- round(domain_summary_fmt$overall_r_dim1_mean, 3)
domain_summary_fmt$overall_r_dim2_mean <- round(domain_summary_fmt$overall_r_dim2_mean, 3)
domain_summary_fmt$trend_cor <- round(domain_summary_fmt$trend_cor, 3)
capture.output(domain_summary_fmt, file = con)
writeLines("", con = con)

robust_domains <- domain_summary_df$domain[domain_summary_df$min_r_dim1 > 0.9]
sensitive_domains <- domain_summary_df$domain[domain_summary_df$min_r_dim1 < 0.8 | domain_summary_df$overall_r_dim1_mean < 0.8]

writeLines("Interpretation (per guide):", con = con)
writeLines("", con = con)
writeLines("- If r > 0.9: coding choice has minimal impact.", con = con)
writeLines("- If r < 0.8: flag domain and inspect which countries/periods diverge.", con = con)
writeLines("- If temporal trends differ in direction: CRITICAL (erosion finding may depend on coding choice).", con = con)
writeLines("", con = con)

writeLines(sprintf("Robust domains (min per-period r_dim1 > 0.9): %s",
                   if (length(robust_domains) == 0) "<none>" else paste(robust_domains, collapse = ", ")),
           con = con)
writeLines(sprintf("Sensitive domains (any per-period r_dim1 < 0.8 OR overall mean r_dim1 < 0.8): %s",
                   if (length(sensitive_domains) == 0) "<none>" else paste(sensitive_domains, collapse = ", ")),
           con = con)
writeLines("", con = con)

writeLines("Temporal Trend Agreement (mean dim1 by period):", con = con)
for (d in names(results)[!skipped]) {
  x <- results[[d]]
  writeLines(sprintf("- %s: trend_cor=%.3f, adjacent_dir_mismatch=%s, overall_dir_mismatch=%s",
                     d,
                     round(x$trend$trend_cor, 3),
                     ifelse(isTRUE(x$trend$any_adjacent_direction_mismatch), "YES", "no"),
                     ifelse(isTRUE(x$trend$overall_direction_mismatch), "YES", "no")),
             con = con)
}
writeLines("", con = con)

writeLines("Top Discrepant Countries (Top 5 by |flow-stock| mean dim1) and likely interpretation:", con = con)
writeLines("", con = con)
for (d in names(results)[!skipped]) {
  disc <- results[[d]]$discrepant_full
  top5 <- utils::head(disc, 5)
  writeLines(sprintf("Domain: %s", d), con = con)
  for (i in seq_len(nrow(top5))) {
    cc <- top5$country_code[i]
    df <- top5$dim1_flow_mean[i]
    ds <- top5$dim1_stock_mean[i]
    dd <- top5$delta_dim1_mean[i]
    writeLines(sprintf("  %d. %s: flow=%.3f stock=%.3f delta(flow-stock)=%.3f",
                       i, cc, df, ds, dd),
               con = con)
  }
  writeLines("  Why (hypothesis): large discrepancies are consistent with activity bias; stock coding can overweight cumulative participation, shifting early joiners relative to late joiners.", con = con)
  writeLines("", con = con)
}

message("Wrote: ", comparison_csv)
message("Wrote: ", report_path)
message("Wrote: ", discrepant_csv)
