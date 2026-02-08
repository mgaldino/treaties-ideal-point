#!/usr/bin/env Rscript
res_A <- readRDS("outputs/v6_country_anchors/investment_results.rds")
res_B <- readRDS("outputs/v6_item_anchors/investment_results.rds")

dir.create("outputs/v6_comparison", recursive=TRUE, showWarnings=FALSE)

x_A <- res_A$ideal_points_mean
x_B <- res_B$ideal_points_mean
valid <- complete.cases(x_A) & complete.cases(x_B)

# Correlations per dimension (try both signs for rotation)
r1_pos <- cor(x_A[valid,1], x_B[valid,1])
r1_neg <- cor(x_A[valid,1], -x_B[valid,1])
r2_pos <- cor(x_A[valid,2], x_B[valid,2])
r2_neg <- cor(x_A[valid,2], -x_B[valid,2])

r1 <- max(abs(r1_pos), abs(r1_neg))
r2 <- max(abs(r2_pos), abs(r2_neg))

# Alpha and beta correlations
r_alpha <- cor(res_A$alpha, res_B$alpha)
r_beta1 <- cor(res_A$beta[,1], res_B$beta[,1])
r_beta2 <- cor(res_A$beta[,2], res_B$beta[,2])

report <- sprintf("
=== V6 Comparison: investment ===
Strategy A: Country anchors (DNK/IRN/ESP)
Strategy B: Item anchors (%s)

Ideal Point Correlations (A vs B):
  Dim1: r = %.4f
  Dim2: r = %.4f

Item Parameter Correlations:
  Alpha: r = %.4f
  Beta1: r = %.4f
  Beta2: r = %.4f

Runtime:
  A: %.1fs (%d iters, conv=%d)
  B: %.1fs (%d iters, conv=%d)

N valid countries: %d / %d
",
paste(res_B$anchor_item_labels, collapse=", "),
r1, r2, r_alpha, r_beta1, r_beta2,
res_A$runtime$seconds, res_A$runtime$iters, res_A$runtime$conv,
res_B$runtime$seconds, res_B$runtime$iters, res_B$runtime$conv,
sum(valid), nrow(x_A))

cat(report)
writeLines(report, "outputs/v6_comparison/investment_report.txt")
cat("Saved: outputs/v6_comparison/investment_report.txt\n")
