#!/usr/bin/env Rscript
options(width = 120)

domain <- "environment"

a_path <- sprintf("outputs/v7_country_anchors/%s_results.rds", domain)
b_path <- sprintf("outputs/v7_item_anchors/%s_results.rds", domain)

res_A <- readRDS(a_path)
res_B <- readRDS(b_path)

xA <- res_A$ideal_points_mean
xB <- res_B$ideal_points_mean

common <- intersect(rownames(xA), rownames(xB))
if (length(common) < 10) stop("Too few common countries between A and B to compare")
xA <- xA[common, , drop = FALSE]
xB <- xB[common, , drop = FALSE]

cor_safe <- function(u, v) {
  if (all(is.na(u)) || all(is.na(v))) return(NA_real_)
  suppressWarnings(cor(u, v, use = "pairwise.complete.obs"))
}

r11 <- cor_safe(xA[, 1], xB[, 1])
r22 <- cor_safe(xA[, 2], xB[, 2])
r12 <- cor_safe(xA[, 1], xB[, 2])
r21 <- cor_safe(xA[, 2], xB[, 1])

alpha_r <- cor_safe(res_A$alpha, res_B$alpha)
beta1_r <- cor_safe(res_A$beta[, 1], res_B$beta[, 1])
beta2_r <- cor_safe(res_A$beta[, 2], res_B$beta[, 2])

top_bottom <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(list(top = character(), bottom = character()))
  o <- order(x, decreasing = TRUE)
  top <- names(x)[o][seq_len(min(n, length(o)))]
  bottom <- names(x)[rev(o)][seq_len(min(n, length(o)))]
  list(top = top, bottom = bottom)
}

tb1 <- top_bottom(setNames(res_A$ideal_points_mean[, 1], rownames(res_A$ideal_points_mean)), n = 5)
tb2 <- top_bottom(setNames(res_A$ideal_points_mean[, 2], rownames(res_A$ideal_points_mean)), n = 5)

out_dir <- "outputs/v7_comparison"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
report_path <- file.path(out_dir, sprintf("%s_report.txt", domain))

lines <- c(
  sprintf("V7 Comparison Report: %s", domain),
  sprintf("Generated: %s", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "Inputs:",
  sprintf("A (country anchors): %s", a_path),
  sprintf("B (item anchors):   %s", b_path),
  "",
  "Part A runtime:",
  sprintf("iters=%s conv=%s seconds=%.1f", res_A$runtime$iters, res_A$runtime$conv, res_A$runtime$seconds),
  "",
  "Part B runtime:",
  sprintf("iters=%s conv=%s seconds=%.1f", res_B$runtime$iters, res_B$runtime$conv, res_B$runtime$seconds),
  "",
  "Part B anchor items (selected from Part A betas):",
  sprintf("items: %s", paste(res_B$anchor_items, collapse = ", ")),
  sprintf("labels: %s", paste(res_B$anchor_item_labels, collapse = " | ")),
  "fixed alpha:",
  paste(capture.output(print(res_B$fixed_alpha)), collapse = "\n"),
  "fixed betas:",
  paste(capture.output(print(res_B$fixed_betas)), collapse = "\n"),
  "",
  sprintf("Ideal point correlations (A vs B) over %d common countries:", length(common)),
  sprintf("dim1 vs dim1: r=%.4f | sign-invariant=%.4f", r11, abs(r11)),
  sprintf("dim2 vs dim2: r=%.4f | sign-invariant=%.4f", r22, abs(r22)),
  "",
  "Cross-dimension correlations (rotation check):",
  sprintf("dim1_A vs dim2_B: r=%.4f", r12),
  sprintf("dim2_A vs dim1_B: r=%.4f", r21),
  "",
  "Item parameter correlations:",
  sprintf("alpha: r=%.4f", alpha_r),
  sprintf("beta_dim1: r=%.4f", beta1_r),
  sprintf("beta_dim2: r=%.4f", beta2_r),
  "",
  "Top-5 / Bottom-5 countries in Part A (country anchors):",
  sprintf("dim1 top: %s", paste(tb1$top, collapse = ", ")),
  sprintf("dim1 bottom: %s", paste(tb1$bottom, collapse = ", ")),
  sprintf("dim2 top: %s", paste(tb2$top, collapse = ", ")),
  sprintf("dim2 bottom: %s", paste(tb2$bottom, collapse = ", ")),
  "",
  "Aggregate trends (Part A):",
  paste(capture.output(print(res_A$aggregate)), collapse = "\n"),
  "",
  "Aggregate trends (Part B):",
  paste(capture.output(print(res_B$aggregate)), collapse = "\n")
)

writeLines(lines, report_path, useBytes = TRUE)
cat(sprintf("Saved: %s\n", report_path))

