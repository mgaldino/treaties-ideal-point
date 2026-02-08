#!/usr/bin/env Rscript

res_A <- readRDS("outputs/v7_country_anchors/investment_results.rds")
res_B <- readRDS("outputs/v7_item_anchors/investment_results.rds")

dir.create("outputs/v7_comparison", recursive = TRUE, showWarnings = FALSE)

x_A <- res_A$ideal_points_mean
x_B <- res_B$ideal_points_mean
valid <- complete.cases(x_A) & complete.cases(x_B)

cor_try_sign <- function(a, b) {
  r_pos <- suppressWarnings(cor(a, b))
  r_neg <- suppressWarnings(cor(a, -b))
  list(r_pos = r_pos, r_neg = r_neg, r_abs = max(abs(r_pos), abs(r_neg)))
}

dim1 <- cor_try_sign(x_A[valid, 1], x_B[valid, 1])
dim2 <- cor_try_sign(x_A[valid, 2], x_B[valid, 2])

# Cross-dimension correlations (rotation check)
cross_12 <- suppressWarnings(cor(x_A[valid, 1], x_B[valid, 2]))
cross_21 <- suppressWarnings(cor(x_A[valid, 2], x_B[valid, 1]))

# Item parameter correlations
r_alpha <- suppressWarnings(cor(res_A$alpha, res_B$alpha))
r_beta1 <- suppressWarnings(cor(res_A$beta[, 1], res_B$beta[, 1]))
r_beta2 <- suppressWarnings(cor(res_A$beta[, 2], res_B$beta[, 2]))

# Top/bottom countries in country-anchor version
top_bottom <- function(x, n = 5L) {
  o <- order(x, decreasing = TRUE)
  list(
    top = data.frame(iso = rownames(x_A)[o[seq_len(n)]], value = x[o[seq_len(n)]]),
    bottom = data.frame(iso = rownames(x_A)[o[(length(o) - n + 1L):length(o)]], value = x[o[(length(o) - n + 1L):length(o)]])
  )
}

tb1 <- top_bottom(x_A[, 1], n = 5L)
tb2 <- top_bottom(x_A[, 2], n = 5L)

report_lines <- c(
  "=== V7 Comparison: investment ===",
  "",
  "Inputs:",
  sprintf("  A: outputs/v7_country_anchors/investment_results.rds (strategy=%s)", res_A$strategy),
  sprintf("  B: outputs/v7_item_anchors/investment_results.rds (strategy=%s)", res_B$strategy),
  "",
  "Ideal Point Correlations (A vs B) with sign-flip check:",
  sprintf("  Dim1: r_pos=%.4f, r_neg=%.4f, max|r|=%.4f", dim1$r_pos, dim1$r_neg, dim1$r_abs),
  sprintf("  Dim2: r_pos=%.4f, r_neg=%.4f, max|r|=%.4f", dim2$r_pos, dim2$r_neg, dim2$r_abs),
  "",
  "Rotation diagnostics (cross-dimension correlations, raw):",
  sprintf("  cor(dim1_A, dim2_B) = %.4f", cross_12),
  sprintf("  cor(dim2_A, dim1_B) = %.4f", cross_21),
  "",
  "Item Parameter Correlations (raw):",
  sprintf("  Alpha: r = %.4f", r_alpha),
  sprintf("  Beta1: r = %.4f", r_beta1),
  sprintf("  Beta2: r = %.4f", r_beta2),
  "",
  "Runtime:",
  sprintf("  A: %.1fs (%d iters, conv=%d)", res_A$runtime$seconds, res_A$runtime$iters, res_A$runtime$conv),
  sprintf("  B: %.1fs (%d iters, conv=%d)", res_B$runtime$seconds, res_B$runtime$iters, res_B$runtime$conv),
  "",
  sprintf("N valid countries: %d / %d", sum(valid), nrow(x_A)),
  "",
  "Top-5 countries on Dim1 (A):",
  paste0("  ", apply(tb1$top, 1, function(r) sprintf("%s: %.4f", r[["iso"]], as.numeric(r[["value"]])))),
  "Bottom-5 countries on Dim1 (A):",
  paste0("  ", apply(tb1$bottom, 1, function(r) sprintf("%s: %.4f", r[["iso"]], as.numeric(r[["value"]])))),
  "",
  "Top-5 countries on Dim2 (A):",
  paste0("  ", apply(tb2$top, 1, function(r) sprintf("%s: %.4f", r[["iso"]], as.numeric(r[["value"]])))),
  "Bottom-5 countries on Dim2 (A):",
  paste0("  ", apply(tb2$bottom, 1, function(r) sprintf("%s: %.4f", r[["iso"]], as.numeric(r[["value"]]))))
)

report <- paste(report_lines, collapse = "\n")
cat(report, "\n")
writeLines(report, "outputs/v7_comparison/investment_report.txt")
cat("Saved: outputs/v7_comparison/investment_report.txt\n")

