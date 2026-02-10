#!/usr/bin/env Rscript

# R2 Robustness Check: Alternative country anchor sensitivity (comparison step)
# Compares each Alt anchor run with V7 baseline:
#   - cross-sectional per-period correlations on dim1 and dim2
#   - overall correlation (flattened N x T) on dim1 and dim2
#   - trend correlation on mean dim1 by period
#
# Outputs:
#   outputs/r2_alt_anchors/comparison_table.csv
#   outputs/r2_alt_anchors/comparison_report.txt
#   outputs/r2_alt_anchors/R2_findings.md

options(stringsAsFactors = FALSE)

domains <- c(
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)

dir.create("outputs/r2_alt_anchors", recursive = TRUE, showWarnings = FALSE)

load_run <- function(path) {
  if (!file.exists(path)) stop(sprintf("Missing results file: %s", path))
  readRDS(path)
}

align_common <- function(base, alt) {
  base_codes <- as.character(base$country_codes)
  alt_codes <- as.character(alt$country_codes)
  common <- intersect(base_codes, alt_codes)
  if (length(common) < 5L) stop("Too few common countries to compare (unexpected)")
  b_idx <- match(common, base_codes)
  a_idx <- match(common, alt_codes)
  list(common = common, b_idx = b_idx, a_idx = a_idx)
}

cor_safe <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0 || sd(y, na.rm = TRUE) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
}

compare_pair <- function(base, alt) {
  al <- align_common(base, alt)
  xb <- base$ideal_points[al$b_idx, , , drop = FALSE]
  xa <- alt$ideal_points[al$a_idx, , , drop = FALSE]

  Tb <- dim(xb)[3]
  Ta <- dim(xa)[3]
  if (Tb != Ta) {
    stop(sprintf("Period mismatch: baseline T=%d vs alt T=%d", Tb, Ta))
  }
  T_periods <- Tb

  # Per-period cross-sectional correlations
  per_dim1 <- rep(NA_real_, T_periods)
  per_dim2 <- rep(NA_real_, T_periods)
  for (t in seq_len(T_periods)) {
    per_dim1[t] <- cor_safe(xb[, 1, t], xa[, 1, t])
    per_dim2[t] <- cor_safe(xb[, 2, t], xa[, 2, t])
  }

  # Overall correlations (flatten across countries and time)
  overall_dim1 <- cor_safe(as.vector(xb[, 1, ]), as.vector(xa[, 1, ]))
  overall_dim2 <- cor_safe(as.vector(xb[, 2, ]), as.vector(xa[, 2, ]))

  # Trend correlation: mean dim1 per period
  trend_b <- sapply(seq_len(T_periods), function(t) mean(xb[, 1, t], na.rm = TRUE))
  trend_a <- sapply(seq_len(T_periods), function(t) mean(xa[, 1, t], na.rm = TRUE))
  trend_cor_dim1 <- cor_safe(trend_b, trend_a)

  list(
    overall_cor_dim1 = overall_dim1,
    overall_cor_dim2 = overall_dim2,
    min_period_cor_dim1 = if (all(is.na(per_dim1))) NA_real_ else suppressWarnings(min(per_dim1, na.rm = TRUE)),
    mean_period_cor_dim1 = if (all(is.na(per_dim1))) NA_real_ else mean(per_dim1, na.rm = TRUE),
    trend_cor_dim1 = trend_cor_dim1,
    per_period_dim1 = per_dim1,
    per_period_dim2 = per_dim2
  )
}

cat("R2 Phase 3: Comparing alternative-anchor runs with V7 baseline\n\n")

rows <- list()
report_lines <- c()

for (domain in domains) {
  base_path <- sprintf("outputs/v7_country_anchors/%s_results.rds", domain)
  base <- load_run(base_path)

  for (alt_set in c("Alt1", "Alt2")) {
    alt_path <- sprintf("outputs/r2_alt_anchors/%s_%s_results.rds", domain, alt_set)
    alt <- load_run(alt_path)

    cmp <- compare_pair(base, alt)

    rows[[length(rows) + 1L]] <- data.frame(
      domain = domain,
      alt_set = alt_set,
      overall_cor_dim1 = cmp$overall_cor_dim1,
      overall_cor_dim2 = cmp$overall_cor_dim2,
      min_period_cor_dim1 = cmp$min_period_cor_dim1,
      mean_period_cor_dim1 = cmp$mean_period_cor_dim1,
      trend_cor_dim1 = cmp$trend_cor_dim1,
      stringsAsFactors = FALSE
    )

    report_lines <- c(
      report_lines,
      sprintf("Domain: %s | Alt set: %s", domain, alt_set),
      sprintf("  overall_cor_dim1 = %.4f", cmp$overall_cor_dim1),
      sprintf("  overall_cor_dim2 = %.4f", cmp$overall_cor_dim2),
      sprintf("  min_period_cor_dim1 = %.4f", cmp$min_period_cor_dim1),
      sprintf("  mean_period_cor_dim1 = %.4f", cmp$mean_period_cor_dim1),
      sprintf("  trend_cor_dim1 = %.4f", cmp$trend_cor_dim1),
      ""
    )

    cat(sprintf("  %s x %s: overall dim1=%.3f dim2=%.3f | mean period dim1=%.3f | trend dim1=%.3f\n",
                domain, alt_set,
                cmp$overall_cor_dim1, cmp$overall_cor_dim2,
                cmp$mean_period_cor_dim1, cmp$trend_cor_dim1))
  }
  cat("\n")
}

tab <- do.call(rbind, rows)
tab <- tab[order(tab$domain, tab$alt_set), , drop = FALSE]

out_csv <- "outputs/r2_alt_anchors/comparison_table.csv"
write.csv(tab, out_csv, row.names = FALSE)
cat(sprintf("Saved: %s\n", out_csv))

out_txt <- "outputs/r2_alt_anchors/comparison_report.txt"
writeLines(report_lines, out_txt, useBytes = TRUE)
cat(sprintf("Saved: %s\n", out_txt))

# Findings markdown
fmt_num <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))
md <- c()
md <- c(md, "# R2 Robustness Check: Alternative Country Anchor Sensitivity")
md <- c(md, "")
md <- c(md, "This robustness check evaluates sensitivity of the 2D dynamic IRT estimates to the *choice of anchor countries* (with tight priors, sigma=0.01), relative to the V7 baseline (3 country anchors per domain).")
md <- c(md, "")
md <- c(md, "## Summary Table")
md <- c(md, "")
md <- c(md, "| domain | alt_set | overall_cor_dim1 | overall_cor_dim2 | min_period_cor_dim1 | mean_period_cor_dim1 | trend_cor_dim1 |")
md <- c(md, "|---|---:|---:|---:|---:|---:|---:|")
for (i in seq_len(nrow(tab))) {
  md <- c(md, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                      tab$domain[i],
                      tab$alt_set[i],
                      fmt_num(tab$overall_cor_dim1[i]),
                      fmt_num(tab$overall_cor_dim2[i]),
                      fmt_num(tab$min_period_cor_dim1[i]),
                      fmt_num(tab$mean_period_cor_dim1[i]),
                      fmt_num(tab$trend_cor_dim1[i])))
}
md <- c(md, "")

# Heuristic interpretation
classify <- function(overall, mean_period, trend) {
  if (is.na(overall) || is.na(mean_period) || is.na(trend)) return("insufficient")
  if (overall >= 0.95 && mean_period >= 0.90 && trend >= 0.95) return("robust")
  if (overall >= 0.90 && mean_period >= 0.80 && trend >= 0.90) return("mostly_robust")
  "sensitive"
}

tab$classification <- mapply(classify, tab$overall_cor_dim1, tab$mean_period_cor_dim1, tab$trend_cor_dim1)

md <- c(md, "## Key Findings")
md <- c(md, "")

for (domain in domains) {
  sub <- tab[tab$domain == domain, , drop = FALSE]
  if (nrow(sub) == 0) next
  # Worst-case across Alt1/Alt2 on dim1 overall
  worst <- sub[which.min(sub$overall_cor_dim1), , drop = FALSE]
  best <- sub[which.max(sub$overall_cor_dim1), , drop = FALSE]
  md <- c(md, sprintf("- **%s**: dim1 overall correlation ranges from %s (set=%s) to %s (set=%s). Classification: Alt1=%s, Alt2=%s.",
                      domain,
                      fmt_num(worst$overall_cor_dim1[1]), worst$alt_set[1],
                      fmt_num(best$overall_cor_dim1[1]), best$alt_set[1],
                      sub$classification[sub$alt_set == "Alt1"][1],
                      sub$classification[sub$alt_set == "Alt2"][1]))
}

md <- c(md, "")
md <- c(md, "## Interpretation")
md <- c(md, "")
md <- c(md, "High correlations indicate that cross-sectional ordering and aggregate trends are stable under alternative anchoring choices. Lower correlations suggest that the latent scale is more sensitive to identification choices (particularly on dim2, where anchoring is often harder).")
md <- c(md, "")
md <- c(md, "Artifacts produced by this script:")
md <- c(md, "- `outputs/r2_alt_anchors/comparison_table.csv`")
md <- c(md, "- `outputs/r2_alt_anchors/comparison_report.txt`")
md <- c(md, "- `outputs/r2_alt_anchors/R2_findings.md`")

out_md <- "outputs/r2_alt_anchors/R2_findings.md"
writeLines(md, out_md, useBytes = TRUE)
cat(sprintf("Saved: %s\n", out_md))
