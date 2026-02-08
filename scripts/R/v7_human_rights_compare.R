#!/usr/bin/env Rscript

domain <- "human_rights"
K <- 2L

res_A <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
res_B <- readRDS(sprintf("outputs/v7_item_anchors/%s_results.rds", domain))

xA <- res_A$ideal_points_mean
xB <- res_B$ideal_points_mean

# Align countries (defensive; should already match)
common <- intersect(rownames(xA), rownames(xB))
xA <- xA[common, , drop = FALSE]
xB <- xB[common, , drop = FALSE]

cor_safe <- function(a, b) {
  if (all(is.na(a)) || all(is.na(b))) return(NA_real_)
  suppressWarnings(cor(a, b, use = "pairwise.complete.obs"))
}

# Dimension-wise correlations (handle sign flips via abs)
r_d1 <- cor_safe(xA[, 1], xB[, 1])
r_d2 <- cor_safe(xA[, 2], xB[, 2])
r_d1_abs <- max(abs(r_d1), abs(cor_safe(xA[, 1], -xB[, 1])))
r_d2_abs <- max(abs(r_d2), abs(cor_safe(xA[, 2], -xB[, 2])))

# Cross-dimension correlations (rotation check)
r_A1_B2 <- cor_safe(xA[, 1], xB[, 2])
r_A2_B1 <- cor_safe(xA[, 2], xB[, 1])

# Alpha / beta correlations
alpha_r <- cor_safe(res_A$alpha, res_B$alpha)

beta_r_dim1 <- cor_safe(res_A$beta[, 1], res_B$beta[, 1])
beta_r_dim2 <- cor_safe(res_A$beta[, 2], res_B$beta[, 2])
beta_r_all <- cor_safe(as.numeric(res_A$beta), as.numeric(res_B$beta))

# Top/bottom countries (country-anchor version)
top_bottom <- function(x, n = 5L) {
  x <- x[is.finite(x)]
  x <- sort(x, decreasing = TRUE)
  list(top = head(x, n), bottom = tail(x, n))
}
tb1 <- top_bottom(res_A$ideal_points_mean[, 1], 5L)
tb2 <- top_bottom(res_A$ideal_points_mean[, 2], 5L)

# Report
dir.create("outputs/v7_comparison", recursive = TRUE, showWarnings = FALSE)
out_file <- sprintf("outputs/v7_comparison/%s_report.txt", domain)
con <- file(out_file, open = "wt", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

writeLines(sprintf("V7 Comparison Report: %s", domain), con)
writeLines(sprintf("Generated: %s (UTC)", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")), con)
writeLines("", con)

writeLines("== Part A (Country anchors) ==", con)
writeLines(sprintf("Converged: %d | Iters: %d | Seconds: %.1f",
                   res_A$runtime$conv, res_A$runtime$iters, res_A$runtime$seconds), con)
writeLines(sprintf("Anchors: %s", paste(res_A$anchors$iso, collapse = ", ")), con)
writeLines("", con)

writeLines("== Part B (Item anchors) ==", con)
writeLines(sprintf("Converged: %d | Iters: %d | Seconds: %.1f",
                   res_B$runtime$conv, res_B$runtime$iters, res_B$runtime$seconds), con)
writeLines(sprintf("Anchor items (idx): %s", paste(res_B$anchor_items, collapse = ", ")), con)
writeLines(sprintf("Anchor items (labels): %s", paste(res_B$anchor_item_labels, collapse = ", ")), con)
writeLines("Anchor item fixed betas (from Part A):", con)
bt <- cbind(
  item = res_B$anchor_item_labels,
  beta1 = round(res_B$fixed_betas[, 1], 4),
  beta2 = round(res_B$fixed_betas[, 2], 4),
  alpha = round(res_B$fixed_alpha, 4)
)
write.table(bt, con, row.names = FALSE, quote = FALSE, sep = "\t")
writeLines("", con)

writeLines("== Ideal Points Correlations (A vs B) ==", con)
writeLines(sprintf("dim1: r = %.4f | max sign-flip |r| = %.4f", r_d1, r_d1_abs), con)
writeLines(sprintf("dim2: r = %.4f | max sign-flip |r| = %.4f", r_d2, r_d2_abs), con)
writeLines(sprintf("cross (dim1_A vs dim2_B): r = %.4f", r_A1_B2), con)
writeLines(sprintf("cross (dim2_A vs dim1_B): r = %.4f", r_A2_B1), con)
writeLines("", con)

writeLines("== Item Parameter Correlations (A vs B) ==", con)
writeLines(sprintf("alpha: r = %.4f", alpha_r), con)
writeLines(sprintf("beta dim1: r = %.4f", beta_r_dim1), con)
writeLines(sprintf("beta dim2: r = %.4f", beta_r_dim2), con)
writeLines(sprintf("beta (all entries): r = %.4f", beta_r_all), con)
writeLines("", con)

writeLines("== Country-Anchor Ranking (Part A) ==", con)
writeLines("-- Dim1 top-5 --", con)
write.table(tb1$top, con, quote = FALSE, sep = "\t", col.names = FALSE)
writeLines("-- Dim1 bottom-5 --", con)
write.table(tb1$bottom, con, quote = FALSE, sep = "\t", col.names = FALSE)
writeLines("", con)
writeLines("-- Dim2 top-5 --", con)
write.table(tb2$top, con, quote = FALSE, sep = "\t", col.names = FALSE)
writeLines("-- Dim2 bottom-5 --", con)
write.table(tb2$bottom, con, quote = FALSE, sep = "\t", col.names = FALSE)

cat(sprintf("Saved: %s\n", out_file))

