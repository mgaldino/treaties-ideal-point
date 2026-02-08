#!/usr/bin/env Rscript

domain <- "arms_control"

path_A <- sprintf("outputs/v7_country_anchors/%s_results.rds", domain)
path_B <- sprintf("outputs/v7_item_anchors/%s_results.rds", domain)
if (!file.exists(path_A)) stop(sprintf("Missing Part A results: %s", path_A))
if (!file.exists(path_B)) stop(sprintf("Missing Part B results: %s", path_B))

A <- readRDS(path_A)
B <- readRDS(path_B)

dir.create("outputs/v7_comparison", recursive = TRUE, showWarnings = FALSE)

xA <- A$ideal_points_mean
xB <- B$ideal_points_mean
if (is.null(xA) || is.null(xB)) stop("Missing ideal_points_mean in A or B.")
if (ncol(xA) < 2 || ncol(xB) < 2) stop("Need K=2 ideal points in both results.")

common <- intersect(rownames(xA), rownames(xB))
if (length(common) < 5) stop("Too few overlapping countries to compare.")
xA <- xA[common, , drop = FALSE]
xB <- xB[common, , drop = FALSE]

cor_safe <- function(a, b) {
  suppressWarnings(cor(a, b, use = "pairwise.complete.obs"))
}

r11 <- cor_safe(xA[, 1], xB[, 1])
r22 <- cor_safe(xA[, 2], xB[, 2])
r12 <- cor_safe(xA[, 1], xB[, 2])
r21 <- cor_safe(xA[, 2], xB[, 1])

abs_r11 <- abs(r11)
abs_r22 <- abs(r22)

alphaA <- as.numeric(A$alpha)
alphaB <- as.numeric(B$alpha)
betaA <- as.matrix(A$beta)
betaB <- as.matrix(B$beta)

cor_alpha <- cor_safe(alphaA, alphaB)
cor_beta1 <- cor_safe(betaA[, 1], betaB[, 1])
cor_beta2 <- cor_safe(betaA[, 2], betaB[, 2])
cor_beta12 <- cor_safe(as.numeric(betaA), as.numeric(betaB))

# Top/bottom countries from country-anchor version (Part A)
top_bottom <- function(vec, codes, n = 5L) {
  ok <- is.finite(vec)
  vv <- vec[ok]
  cc <- codes[ok]
  ord <- order(vv, decreasing = TRUE)
  top <- data.frame(iso = cc[ord][seq_len(min(n, length(ord)))], value = vv[ord][seq_len(min(n, length(ord)))])
  ord2 <- order(vv, decreasing = FALSE)
  bot <- data.frame(iso = cc[ord2][seq_len(min(n, length(ord2)))], value = vv[ord2][seq_len(min(n, length(ord2)))])
  list(top = top, bottom = bot)
}

tb1 <- top_bottom(A$ideal_points_mean[, 1], rownames(A$ideal_points_mean), 5L)
tb2 <- top_bottom(A$ideal_points_mean[, 2], rownames(A$ideal_points_mean), 5L)

report_path <- sprintf("outputs/v7_comparison/%s_report.txt", domain)
con <- file(report_path, open = "wt")
on.exit(close(con), add = TRUE)

writeLines(sprintf("V7 Comparison Report | %s", domain), con)
writeLines(sprintf("Generated (local time): %s", format(Sys.time())), con)
writeLines("", con)

writeLines("Inputs:", con)
writeLines(sprintf("  A (country anchors): %s", path_A), con)
writeLines(sprintf("  B (item anchors):    %s", path_B), con)
writeLines("", con)

writeLines("Ideal point correlations (means across active periods):", con)
writeLines(sprintf("  cor(dim1_A, dim1_B) = %.4f | abs (sign-flip invariant) = %.4f", r11, abs_r11), con)
writeLines(sprintf("  cor(dim2_A, dim2_B) = %.4f | abs (sign-flip invariant) = %.4f", r22, abs_r22), con)
writeLines("", con)

writeLines("Cross-dimension correlations (rotation check):", con)
writeLines(sprintf("  cor(dim1_A, dim2_B) = %.4f", r12), con)
writeLines(sprintf("  cor(dim2_A, dim1_B) = %.4f", r21), con)
writeLines("", con)

writeLines("Item parameter correlations:", con)
writeLines(sprintf("  cor(alpha_A, alpha_B) = %.4f", cor_alpha), con)
writeLines(sprintf("  cor(beta1_A, beta1_B) = %.4f", cor_beta1), con)
writeLines(sprintf("  cor(beta2_A, beta2_B) = %.4f", cor_beta2), con)
writeLines(sprintf("  cor(c(beta_A), c(beta_B)) = %.4f", cor_beta12), con)
writeLines("", con)

if (!is.null(B$anchors) && !is.null(B$anchors$items)) {
  it <- B$anchors$items
  writeLines("Part B anchor items (fixed to Part A values):", con)
  for (k in seq_along(it$j)) {
    j <- it$j[k]
    lbl <- it$labels[k]
    aa <- it$alpha[k]
    bb <- it$beta[k, ]
    writeLines(sprintf("  j=%d | label=%s | alpha=%.4f | beta=(%.4f, %.4f)", j, lbl, aa, bb[1], bb[2]), con)
  }
  writeLines("", con)
}

writeLines("Top/bottom countries (Part A / country anchors):", con)
writeLines("  Dim 1 top 5:", con)
write.table(tb1$top, con, row.names = FALSE, col.names = TRUE, quote = FALSE)
writeLines("  Dim 1 bottom 5:", con)
write.table(tb1$bottom, con, row.names = FALSE, col.names = TRUE, quote = FALSE)
writeLines("", con)
writeLines("  Dim 2 top 5:", con)
write.table(tb2$top, con, row.names = FALSE, col.names = TRUE, quote = FALSE)
writeLines("  Dim 2 bottom 5:", con)
write.table(tb2$bottom, con, row.names = FALSE, col.names = TRUE, quote = FALSE)
writeLines("", con)

writeLines("Runtime summary:", con)
if (!is.null(A$runtime)) {
  writeLines(sprintf("  Part A: conv=%s, iters=%s, seconds=%s",
                     A$runtime$conv, A$runtime$iters, A$runtime$seconds), con)
}
if (!is.null(B$runtime)) {
  writeLines(sprintf("  Part B: conv=%s, iters=%s, seconds=%s",
                     B$runtime$conv, B$runtime$iters, B$runtime$seconds), con)
}

writeLines(sprintf("\nSaved: %s", report_path), con)

cat(sprintf("Wrote: %s\n", report_path))

