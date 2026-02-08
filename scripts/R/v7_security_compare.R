#!/usr/bin/env Rscript

domain <- "security"

res_A <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
res_B <- readRDS(sprintf("outputs/v7_item_anchors/%s_results.rds", domain))

dir.create("outputs/v7_comparison", recursive = TRUE, showWarnings = FALSE)

x_A <- res_A$ideal_points_mean
x_B <- res_B$ideal_points_mean

valid <- complete.cases(x_A) & complete.cases(x_B)
nv <- sum(valid)
Ntotal <- nrow(x_A)

cor_safe <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(a[ok], b[ok]))
}

# Sign alignment for each dimension (choose sign making correlation positive)
r1_raw <- cor_safe(x_A[valid, 1], x_B[valid, 1])
s1 <- if (is.na(r1_raw) || r1_raw >= 0) 1 else -1
r1 <- cor_safe(x_A[valid, 1], s1 * x_B[valid, 1])

r2_raw <- cor_safe(x_A[valid, 2], x_B[valid, 2])
s2 <- if (is.na(r2_raw) || r2_raw >= 0) 1 else -1
r2 <- cor_safe(x_A[valid, 2], s2 * x_B[valid, 2])

# Cross-dimension correlations (rotation checks)
r12 <- cor_safe(x_A[valid, 1], s2 * x_B[valid, 2])
r21 <- cor_safe(x_A[valid, 2], s1 * x_B[valid, 1])

# Item parameter correlations
r_alpha <- cor_safe(res_A$alpha, res_B$alpha)
r_beta1_raw <- cor_safe(res_A$beta[, 1], res_B$beta[, 1])
r_beta2_raw <- cor_safe(res_A$beta[, 2], res_B$beta[, 2])
r_beta1 <- cor_safe(res_A$beta[, 1], s1 * res_B$beta[, 1])
r_beta2 <- cor_safe(res_A$beta[, 2], s2 * res_B$beta[, 2])

betaA_vec <- as.numeric(res_A$beta)
betaB_adj_vec <- as.numeric(cbind(s1 * res_B$beta[, 1], s2 * res_B$beta[, 2]))
r_beta_all <- cor_safe(betaA_vec, betaB_adj_vec)

# Top/bottom countries (country-anchor version)
cc <- res_A$country_codes
dim1 <- x_A[, 1]
dim2 <- x_A[, 2]

top_bottom <- function(vals, codes, k = 5) {
  ok <- is.finite(vals)
  ord <- order(vals[ok], decreasing = TRUE)
  v_ok <- vals[ok]
  c_ok <- codes[ok]
  top_i <- ord[seq_len(min(k, length(ord)))]
  bot_i <- ord[seq.int(max(1, length(ord) - k + 1), length(ord))]
  list(
    top = data.frame(country = c_ok[top_i], value = v_ok[top_i]),
    bottom = data.frame(country = c_ok[bot_i], value = v_ok[bot_i])
  )
}

tb1 <- top_bottom(dim1, cc, 5)
tb2 <- top_bottom(dim2, cc, 5)

anchor_items_line <- if (!is.null(res_B$anchor_item_labels)) {
  paste(res_B$anchor_item_labels, collapse = ", ")
} else {
  "(missing anchor labels)"
}

report <- paste0(
  "=== V7 Comparison: security ===\n",
  "Strategy A: Country anchors (DNK/IRN/UKR)\n",
  "Strategy B: Item anchors (", anchor_items_line, ")\n\n",

  "Ideal Point Correlations (A vs B):\n",
  sprintf("  Dim1: r_raw = %.4f | sign(B1) = %+d | r_aligned = %.4f\n", r1_raw, s1, r1),
  sprintf("  Dim2: r_raw = %.4f | sign(B2) = %+d | r_aligned = %.4f\n\n", r2_raw, s2, r2),

  "Cross-Dimension Correlations (rotation checks; B aligned by signs above):\n",
  sprintf("  cor(dim1_A, dim2_B) = %.4f\n", r12),
  sprintf("  cor(dim2_A, dim1_B) = %.4f\n\n", r21),

  "Item Parameter Correlations:\n",
  sprintf("  Alpha: r = %.4f\n", r_alpha),
  sprintf("  Beta1: r_raw = %.4f | r_aligned = %.4f\n", r_beta1_raw, r_beta1),
  sprintf("  Beta2: r_raw = %.4f | r_aligned = %.4f\n", r_beta2_raw, r_beta2),
  sprintf("  Beta(all dims, aligned): r = %.4f\n\n", r_beta_all),

  "Runtime:\n",
  sprintf("  A: %.1fs (%d iters, conv=%d)\n",
          res_A$runtime$seconds, res_A$runtime$iters, res_A$runtime$conv),
  sprintf("  B: %.1fs (%d iters, conv=%d)\n\n",
          res_B$runtime$seconds, res_B$runtime$iters, res_B$runtime$conv),

  sprintf("N valid countries (complete cases in x_mean): %d / %d\n\n", nv, Ntotal),

  "Top/Bottom Countries (Strategy A, mean ideal points):\n",
  "  Dim1 top-5:\n",
  paste(sprintf("    %s\t%.4f", tb1$top$country, tb1$top$value), collapse = "\n"), "\n",
  "  Dim1 bottom-5:\n",
  paste(sprintf("    %s\t%.4f", tb1$bottom$country, tb1$bottom$value), collapse = "\n"), "\n",
  "  Dim2 top-5:\n",
  paste(sprintf("    %s\t%.4f", tb2$top$country, tb2$top$value), collapse = "\n"), "\n",
  "  Dim2 bottom-5:\n",
  paste(sprintf("    %s\t%.4f", tb2$bottom$country, tb2$bottom$value), collapse = "\n"), "\n"
)

cat(report)
writeLines(report, "outputs/v7_comparison/security_report.txt")

