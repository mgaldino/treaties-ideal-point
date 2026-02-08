#!/usr/bin/env Rscript
res_A <- readRDS("outputs/v6_country_anchors/security_results.rds")
res_B <- readRDS("outputs/v6_item_anchors/security_results.rds")
dir.create("outputs/v6_comparison", recursive=TRUE, showWarnings=FALSE)

x_A <- res_A$ideal_points_mean; x_B <- res_B$ideal_points_mean
valid <- complete.cases(x_A) & complete.cases(x_B)
r1 <- max(abs(cor(x_A[valid,1], x_B[valid,1])), abs(cor(x_A[valid,1], -x_B[valid,1])))
r2 <- max(abs(cor(x_A[valid,2], x_B[valid,2])), abs(cor(x_A[valid,2], -x_B[valid,2])))
r_alpha <- cor(res_A$alpha, res_B$alpha)
r_beta1 <- cor(res_A$beta[,1], res_B$beta[,1])
r_beta2 <- cor(res_A$beta[,2], res_B$beta[,2])

report <- sprintf("
=== V6 Comparison: security ===
Strategy A: Country anchors (DNK/IRN/UKR)
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
writeLines(report, "outputs/v6_comparison/security_report.txt")
