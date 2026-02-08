#!/usr/bin/env Rscript
domain <- "intellectual_property"

# Null-coalesce helper (keep script standalone)
`%||%` <- function(x, y) if (is.null(x)) y else x

path_A <- sprintf("outputs/v7_country_anchors/%s_results.rds", domain)
path_B <- sprintf("outputs/v7_item_anchors/%s_results.rds", domain)
if (!file.exists(path_A)) stop("Missing Part A results: ", path_A)
if (!file.exists(path_B)) stop("Missing Part B results: ", path_B)

A <- readRDS(path_A)
B <- readRDS(path_B)

xA <- A$ideal_points_mean
xB <- B$ideal_points_mean
if (is.null(xA) || is.null(xB)) stop("Missing ideal_points_mean in results.")

common <- intersect(rownames(xA), rownames(xB))
if (length(common) < 5L) stop("Too few overlapping countries for comparison.")
xA <- xA[common, , drop = FALSE]
xB <- xB[common, , drop = FALSE]

safe_cor <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 3L) return(NA_real_)
  suppressWarnings(cor(a[ok], b[ok]))
}

# Align signs by maximizing |cor(dim_k_A, +/- dim_k_B)|
r11 <- safe_cor(xA[, 1], xB[, 1])
r11f <- safe_cor(xA[, 1], -xB[, 1])
sign1 <- if (is.finite(r11f) && (is.na(r11) || abs(r11f) > abs(r11))) -1 else 1

r22 <- safe_cor(xA[, 2], xB[, 2])
r22f <- safe_cor(xA[, 2], -xB[, 2])
sign2 <- if (is.finite(r22f) && (is.na(r22) || abs(r22f) > abs(r22))) -1 else 1

xB_aligned <- xB
xB_aligned[, 1] <- sign1 * xB_aligned[, 1]
xB_aligned[, 2] <- sign2 * xB_aligned[, 2]

dim1_cor <- safe_cor(xA[, 1], xB_aligned[, 1])
dim2_cor <- safe_cor(xA[, 2], xB_aligned[, 2])

cross_12 <- safe_cor(xA[, 1], xB_aligned[, 2]) # dim1_A vs dim2_B
cross_21 <- safe_cor(xA[, 2], xB_aligned[, 1]) # dim2_A vs dim1_B

# Alpha/beta correlations (after applying same sign flips to B betas)
alpha_cor <- safe_cor(A$alpha, B$alpha)
beta_cor_1 <- safe_cor(A$beta[, 1], sign1 * B$beta[, 1])
beta_cor_2 <- safe_cor(A$beta[, 2], sign2 * B$beta[, 2])

# Top/bottom countries (Part A)
top_bottom <- function(v, n = 5L) {
  v2 <- v
  names(v2) <- rownames(xA)
  v2 <- v2[is.finite(v2)]
  ord <- order(v2, decreasing = TRUE)
  list(
    top = v2[head(ord, n)],
    bottom = v2[tail(ord, n)]
  )
}
tb1 <- top_bottom(xA[, 1], 5L)
tb2 <- top_bottom(xA[, 2], 5L)

dir.create("outputs/v7_comparison", recursive = TRUE, showWarnings = FALSE)
out_path <- sprintf("outputs/v7_comparison/%s_report.txt", domain)

converged_A <- A$runtime$conv %||% NA_integer_
iters_A <- A$runtime$iters %||% NA_integer_
secs_A <- A$runtime$seconds %||% NA_real_

converged_B <- B$runtime$conv %||% NA_integer_
iters_B <- B$runtime$iters %||% NA_integer_
secs_B <- B$runtime$seconds %||% NA_real_

fmt_named_vec <- function(x) {
  paste(sprintf("%s=%.4f", names(x), as.numeric(x)), collapse = ", ")
}

fmt_labels <- function(x) {
  if (is.null(x)) return("(null)")
  if (length(x) == 0L) return("(empty)")
  paste(as.character(x), collapse = " | ")
}

lines <- c(
  sprintf("V7 Comparison Report | domain=%s", domain),
  "",
  "Part A (country anchors):",
  sprintf("  converged=%s | iters=%s | runtime_seconds=%s",
          as.character(converged_A), as.character(iters_A), as.character(secs_A)),
  sprintf("  anchors=%s", paste(A$anchors$iso, collapse = ", ")),
  "",
  "Part B (item anchors):",
  sprintf("  converged=%s | iters=%s | runtime_seconds=%s",
          as.character(converged_B), as.character(iters_B), as.character(secs_B)),
  sprintf("  anchor_items=%s", paste(B$anchor_items, collapse = ", ")),
  sprintf("  anchor_item_labels=%s", fmt_labels(B$anchor_item_labels)),
  "",
  "Ideal point correlations (A vs B, with per-dimension sign alignment):",
  sprintf("  sign_alignment: dim1_B *= %d, dim2_B *= %d", sign1, sign2),
  sprintf("  cor(dim1_A, dim1_B_aligned)=%.4f", dim1_cor),
  sprintf("  cor(dim2_A, dim2_B_aligned)=%.4f", dim2_cor),
  "",
  "Cross-dimension correlations (rotation check):",
  sprintf("  cor(dim1_A, dim2_B_aligned)=%.4f", cross_12),
  sprintf("  cor(dim2_A, dim1_B_aligned)=%.4f", cross_21),
  "",
  "Item parameter correlations:",
  sprintf("  cor(alpha_A, alpha_B)=%.4f", alpha_cor),
  sprintf("  cor(beta1_A, beta1_B_aligned)=%.4f", beta_cor_1),
  sprintf("  cor(beta2_A, beta2_B_aligned)=%.4f", beta_cor_2),
  "",
  "Top-5 / Bottom-5 countries (Part A):",
  sprintf("  dim1 top: %s", fmt_named_vec(tb1$top)),
  sprintf("  dim1 bottom: %s", fmt_named_vec(tb1$bottom)),
  sprintf("  dim2 top: %s", fmt_named_vec(tb2$top)),
  sprintf("  dim2 bottom: %s", fmt_named_vec(tb2$bottom)),
  ""
)

writeLines(lines, out_path, useBytes = TRUE)
cat(sprintf("Saved: %s\n", out_path))
