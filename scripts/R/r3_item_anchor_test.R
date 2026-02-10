#!/usr/bin/env Rscript
set.seed(2026)

suppressPackageStartupMessages({
  library(dplyr)
})

K <- 2L
domains <- c(
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)

anchors_by_domain <- list(
  investment = list(iso = c("DNK", "IRN", "CHN"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  security = list(iso = c("DNK", "IRN", "UKR"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  environment = list(iso = c("DNK", "SAU", "AUS"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  human_rights = list(iso = c("DNK", "PRK", "USA"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  arms_control = list(iso = c("NZL", "ISR", "IND"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  intellectual_property = list(iso = c("DNK", "AGO", "BRA"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2)))
)

ctrl <- list(
  verbose = TRUE,
  thresh = 1e-4,
  maxit = 5000L,
  checkfreq = 50L,
  estimate_omega = FALSE,
  thresh_loglik = 0.01,
  loglik_patience = 5L,
  ncores = 4L
)
if (.Platform$OS.type == "windows") ctrl$ncores <- 1L

out_dir <- file.path("outputs", "r3_item_anchors")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

anchor_csv <- file.path(out_dir, "anchor_items.csv")
if (!file.exists(anchor_csv)) stop("Missing Phase 1 output: ", anchor_csv)
anchor_items <- utils::read.csv(anchor_csv, stringsAsFactors = FALSE, na.strings = c("", "NA"))

load_dynirt <- function() {
  ok_rcpp <- TRUE
  tryCatch(
    source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R"),
    error = function(e) {
      message("Rcpp compilation/load failed; falling back to scripts/R/dynIRT_KD.R")
      ok_rcpp <<- FALSE
      source("scripts/R/dynIRT_KD.R")
    }
  )
  ok_rcpp
}

pca_init_x <- function(flow, K) {
  rc_num <- matrix(as.numeric(flow$rc), nrow = nrow(flow$rc), ncol = ncol(flow$rc))
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(ncol(rc_num))) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }
  col_var <- apply(rc_num, 2, stats::var)
  rc_pca <- if (any(col_var == 0)) rc_num[, col_var > 0, drop = FALSE] else rc_num
  pca <- stats::prcomp(rc_pca, center = TRUE, scale. = FALSE)
  x_pca <- pca$x[, 1:K, drop = FALSE]
  for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_start <- array(NA_real_, dim = c(nrow(flow$rc), K, flow$T))
  for (t in seq_len(flow$T)) x_start[, , t] <- x_pca
  x_start
}

compute_x_mean <- function(x_arr, flow) {
  N <- nrow(flow$rc); K <- dim(x_arr)[2]; T_periods <- flow$T
  sl <- as.integer(flow$startlegis)
  el <- as.integer(flow$endlegis)
  x_mean <- matrix(NA_real_, N, K)
  for (i in seq_len(N)) {
    s <- sl[i] + 1L
    e <- el[i] + 1L
    if (e >= s && s >= 1L && e <= T_periods) {
      x_mean[i, ] <- if (s == e) x_arr[i, , s] else rowMeans(x_arr[i, , s:e])
    }
  }
  rownames(x_mean) <- flow$country_codes
  x_mean
}

cor_aligned <- function(x_base, codes_base, x_new, codes_new) {
  common <- intersect(as.character(codes_base), as.character(codes_new))
  if (length(common) < 5) return(c(NA_real_, NA_real_))
  ib <- match(common, as.character(codes_base))
  inew <- match(common, as.character(codes_new))
  c(
    suppressWarnings(stats::cor(x_base[ib, 1], x_new[inew, 1], use = "complete.obs")),
    suppressWarnings(stats::cor(x_base[ib, 2], x_new[inew, 2], use = "complete.obs"))
  )
}

cor_beta1_flip_aligned <- function(beta_base, labels_base, beta_new, labels_new) {
  lb <- as.character(labels_base)
  ln <- as.character(labels_new)
  common <- intersect(lb, ln)
  if (length(common) < 10) return(NA_real_)
  ib <- match(common, lb)
  inew <- match(common, ln)
  suppressWarnings(stats::cor(beta_base[ib, 1], -beta_new[inew, 1], use = "complete.obs"))
}

make_country_priors <- function(flow, iso, pos) {
  N <- nrow(flow$rc); K <- ncol(pos)
  idx <- match(iso, flow$country_codes)
  if (any(is.na(idx))) {
    missing <- iso[is.na(idx)]
    stop(sprintf("Anchor countries not found in flow matrix: %s", paste(missing, collapse = ", ")))
  }
  x_mu0 <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)
  for (a in seq_along(idx)) {
    x_mu0[idx[a], ] <- pos[a, ]
    x_Sigma0[idx[a], ] <- 0.01
  }
  list(x_mu0 = x_mu0, x_Sigma0 = x_Sigma0, idx = idx)
}

run_estimation <- function(flow, x_mu0, x_Sigma0, alpha_start, beta_start, x_start, strategy_label) {
  t0 <- proc.time()
  res <- dynIRT_KD(
    .data = list(
      rc = flow$rc,
      startlegis = matrix(as.integer(flow$startlegis), ncol = 1),
      endlegis = matrix(as.integer(flow$endlegis), ncol = 1),
      bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
      T = as.integer(flow$T)
    ),
    .starts = list(
      alpha = as.numeric(alpha_start),
      beta = as.matrix(beta_start),
      x = x_start
    ),
    .priors = list(
      x.mu0 = x_mu0,
      x.Sigma0 = x_Sigma0,
      beta.mu = rep(0, K + 1),
      beta.sigma = 25 * diag(K + 1),
      omega = 0.1 * diag(K)
    ),
    .control = ctrl,
    K = K
  )
  elapsed <- (proc.time() - t0)["elapsed"]

  out <- list(
    domain = flow$domain,
    strategy = strategy_label,
    ideal_points = res$means$x,
    ideal_points_mean = compute_x_mean(res$means$x, flow),
    alpha = res$means$alpha,
    beta = res$means$beta,
    country_codes = flow$country_codes,
    item_labels = flow$item_labels,
    period_labels = flow$period_labels,
    anchors = list(
      iso = attr(x_mu0, "anchors_iso"),
      positions = attr(x_mu0, "anchors_pos"),
      rationale = attr(x_mu0, "anchors_rationale")
    ),
    runtime = list(
      seconds = as.numeric(elapsed),
      iters = res$runtime$iters,
      conv = res$runtime$conv,
      loglik_trace = res$runtime$loglik_trace
    )
  )
  out
}

cat("R3 Phase 2: Three-way item anchor test\n\n")
using_rcpp <- load_dynirt()
cat(sprintf("dynIRT implementation: %s\n\n", if (using_rcpp) "rcpp_merged" else "pure_R"))

summary_rows <- list()

for (dom in domains) {
  cat(sprintf("=== Domain: %s ===\n", dom))

  flow_path <- file.path("data", "processed", paste0(dom, "_flow_matrix.rds"))
  if (!file.exists(flow_path)) stop("Missing: ", flow_path)
  flow <- readRDS(flow_path)
  flow$domain <- dom

  base_path <- sprintf("outputs/v7_country_anchors/%s_results.rds", dom)
  if (!file.exists(base_path)) stop("Missing V7 baseline: ", base_path)
  base <- readRDS(base_path)

  beta0 <- as.matrix(base$beta)
  alpha0 <- as.numeric(base$alpha)

  # Index mapping by item_id label (V7 stores item_labels in results)
  item_ids <- as.character(base$item_labels)
  if (length(item_ids) != nrow(beta0)) stop("Baseline item_labels length != nrow(beta) for domain=", dom)
  names(item_ids) <- NULL

  # Warn if data ordering changed since V7 output (comparisons will be aligned by IDs where possible).
  if (!identical(as.character(base$country_codes), as.character(flow$country_codes))) {
    warning(sprintf("[R3] Country code order differs between V7 results and current flow matrix (domain=%s). Correlations will be aligned by country code.", dom))
  }
  if (!identical(as.character(base$item_labels), as.character(flow$item_labels))) {
    warning(sprintf("[R3] Item label order differs between V7 results and current flow matrix (domain=%s). Beta comparisons will be aligned by item label.", dom))
  }

  a_dom <- anchor_items %>% dplyr::filter(.data$domain == dom)
  setA <- a_dom %>% dplyr::filter(.data$set_name == "A")
  setB <- a_dom %>% dplyr::filter(.data$set_name == "B")

  # ---- Test A: Sign verification ----
  sign_df <- a_dom %>%
    dplyr::mutate(
      v7_beta1_sign = dplyr::case_when(
        is.na(.data$v7_beta1) ~ NA_character_,
        .data$v7_beta1 > 0 ~ "positive",
        .data$v7_beta1 < 0 ~ "negative",
        TRUE ~ "zero"
      ),
      v7_beta1_positive = ifelse(is.na(.data$v7_beta1), NA, .data$v7_beta1 > 0)
    ) %>%
    dplyr::select(
      domain,
      set_name,
      item_id,
      item_name,
      v7_beta1,
      v7_beta2,
      expected_sign,
      v7_beta1_sign,
      v7_beta1_positive
    )

  sign_path <- file.path(out_dir, sprintf("%s_sign_check.csv", dom))
  utils::write.csv(sign_df, sign_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Test A saved: ", sign_path, "\n", sep = "")

  nA <- nrow(setA)
  nA_pos <- if (nA == 0) 0L else sum(setA$v7_beta1 > 0, na.rm = TRUE)
  nB <- nrow(setB)
  nB_pos <- if (nB == 0) 0L else sum(setB$v7_beta1 > 0, na.rm = TRUE)
  cat(sprintf("Test A: theory items positive = %d/%d | data items positive = %d/%d\n", nA_pos, nA, nB_pos, nB))

  # ---- Test B: Re-estimate with constrained item starts (Set A) ----
  cfg <- anchors_by_domain[[dom]]
  pri <- make_country_priors(flow, cfg$iso, cfg$pos)

  # Attach metadata for saving
  attr(pri$x_mu0, "anchors_iso") <- cfg$iso
  attr(pri$x_mu0, "anchors_pos") <- cfg$pos
  attr(pri$x_mu0, "anchors_rationale") <- "V7 anchors (dim1 +/-2, dim2 -2)"

  x_start <- base$ideal_points
  if (!is.array(x_start) || any(dim(x_start) != c(nrow(flow$rc), K, flow$T))) {
    x_start <- pca_init_x(flow, K)
  }

  beta_start <- beta0
  if (nrow(setA) > 0) {
    idxA <- match(setA$item_id, item_ids)
    bad <- which(is.na(idxA))
    if (length(bad) > 0) {
      warning(sprintf("[R3] Some Set A items not found in baseline item_labels for domain=%s; skipping %d items.",
                      dom, length(bad)))
      idxA <- idxA[!is.na(idxA)]
    }
    if (length(idxA) > 0) beta_start[idxA, 1] <- abs(beta_start[idxA, 1])
  }

  resB <- run_estimation(
    flow = flow,
    x_mu0 = pri$x_mu0,
    x_Sigma0 = pri$x_Sigma0,
    alpha_start = alpha0,
    beta_start = beta_start,
    x_start = x_start,
    strategy_label = "r3_constrained_item_starts_setA"
  )

  b_path <- file.path(out_dir, sprintf("%s_constrained_results.rds", dom))
  saveRDS(resB, b_path)
  cat("Test B saved: ", b_path, "\n", sep = "")

  corB <- cor_aligned(base$ideal_points_mean, base$country_codes, resB$ideal_points_mean, resB$country_codes)
  corB1 <- corB[1]; corB2 <- corB[2]
  cat(sprintf("Test B: cor(mean ideal points) dim1=%.4f dim2=%.4f\n", corB1, corB2))

  # ---- Test C: Re-estimate with SWAPPED dim1 anchors ----
  pos_swapped <- cfg$pos
  pos_swapped[1, 1] <- -pos_swapped[1, 1]
  pos_swapped[2, 1] <- -pos_swapped[2, 1]
  # dim2 anchor unchanged
  pri2 <- make_country_priors(flow, cfg$iso, pos_swapped)

  attr(pri2$x_mu0, "anchors_iso") <- cfg$iso
  attr(pri2$x_mu0, "anchors_pos") <- pos_swapped
  attr(pri2$x_mu0, "anchors_rationale") <- "SWAPPED dim1 anchors; dim2 unchanged"

  x_start2 <- x_start
  x_start2[, 1, ] <- -x_start2[, 1, , drop = FALSE]
  beta_start2 <- beta0
  beta_start2[, 1] <- -beta_start2[, 1]

  resC <- run_estimation(
    flow = flow,
    x_mu0 = pri2$x_mu0,
    x_Sigma0 = pri2$x_Sigma0,
    alpha_start = alpha0,
    beta_start = beta_start2,
    x_start = x_start2,
    strategy_label = "r3_swapped_dim1_anchors"
  )

  c_path <- file.path(out_dir, sprintf("%s_swapped_results.rds", dom))
  saveRDS(resC, c_path)
  cat("Test C saved: ", c_path, "\n", sep = "")

  corC <- cor_aligned(base$ideal_points_mean, base$country_codes, resC$ideal_points_mean, resC$country_codes)
  corC1 <- corC[1]; corC2 <- corC[2]
  cat(sprintf("Test C: cor(mean ideal points) dim1=%.4f dim2=%.4f (expect dim1 ~ -1)\n", corC1, corC2))

  betaC <- as.matrix(resC$beta)
  flip_beta1_cor <- cor_beta1_flip_aligned(beta0, base$item_labels, betaC, resC$item_labels)

  flip_prop_setA <- NA_real_
  if (nrow(setA) > 0) {
    idx0 <- match(setA$item_id, as.character(base$item_labels))
    idx1 <- match(setA$item_id, as.character(resC$item_labels))
    ok <- which(!is.na(idx0) & !is.na(idx1))
    if (length(ok) > 0) {
      s0 <- sign(beta0[idx0[ok], 1])
      s1 <- sign(betaC[idx1[ok], 1])
      ok <- which(s0 != 0 & s1 != 0)
      if (length(ok) > 0) flip_prop_setA <- mean(s0[ok] == -s1[ok])
    }
  }

  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    domain = dom,
    n_theory_items = nA,
    n_theory_pos_v7 = nA_pos,
    n_data_items = nB,
    n_data_pos_v7 = nB_pos,
    testB_cor_dim1 = corB1,
    testB_cor_dim2 = corB2,
    testC_cor_dim1 = corC1,
    testC_cor_dim2 = corC2,
    testC_beta1_flip_cor = flip_beta1_cor,
    testC_setA_flip_prop = flip_prop_setA,
    stringsAsFactors = FALSE
  )

  cat("\n")
}

summary_tbl <- dplyr::bind_rows(summary_rows)
summary_path <- file.path(out_dir, "summary_table.csv")
utils::write.csv(summary_tbl, summary_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("Saved summary: ", summary_path, "\n\n", sep = "")
print(summary_tbl)

# ---- Write findings markdown ----
md_path <- file.path(out_dir, "R3_findings.md")

domain_notes <- c()
for (i in seq_len(nrow(summary_tbl))) {
  r <- summary_tbl[i, ]
  theory_txt <- if (r$n_theory_items == 0) {
    "theory items: none found in V7 (Set A empty)"
  } else {
    sprintf("theory items positive in V7: %d/%d", r$n_theory_pos_v7, r$n_theory_items)
  }
  b_txt <- sprintf("Test B cor(mean IP): dim1=%.3f dim2=%.3f", r$testB_cor_dim1, r$testB_cor_dim2)
  c_txt <- sprintf("Test C cor(mean IP): dim1=%.3f dim2=%.3f", r$testC_cor_dim1, r$testC_cor_dim2)
  beta_txt <- sprintf("Test C beta1 flip cor (baseline vs -swapped): %.3f", r$testC_beta1_flip_cor)

  flags <- c()
  if (!is.na(r$testB_cor_dim1) && r$testB_cor_dim1 < 0.95) flags <- c(flags, "LOW Test B dim1 correlation")
  if (!is.na(r$testC_cor_dim1) && r$testC_cor_dim1 > -0.95) flags <- c(flags, "WEAK dim1 flip under swapped anchors")
  if (r$n_theory_items > 0 && r$n_theory_pos_v7 < r$n_theory_items) flags <- c(flags, "THEORY sign mismatch in V7")

  flag_txt <- if (length(flags) == 0) "OK" else paste(flags, collapse = "; ")

  domain_notes <- c(
    domain_notes,
    sprintf("- `%s`: %s | %s | %s | %s | %s", r$domain, theory_txt, b_txt, c_txt, beta_txt, flag_txt)
  )
}

lines <- c(
  "# R3 Robustness Check: Item Anchor Sensitivity (2D dynamic IRT)",
  "",
  sprintf("Date: %s (local)", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## What R3 Tests",
  "- Test A (Sign verification): whether theoretically motivated items (Set A) have positive `beta1` in V7 baseline.",
  "- Test B (Constrained item starts): reruns V7 with the same country anchors, but forces Set A items to start with `beta1 >= 0` to test whether convergence depends on item-start sign.",
  "- Test C (Swapped dim1 anchors): reruns V7 with dim1 positive/negative anchors swapped; identification implies dim1 ideal points should flip sign (correlation ~ -1 with baseline) and `beta1` should also flip (correlation between baseline `beta1` and `-beta1_swapped` should be high).",
  "",
  "## Outputs",
  "- Per domain sign checks: `outputs/r3_item_anchors/{domain}_sign_check.csv`",
  "- Test B results: `outputs/r3_item_anchors/{domain}_constrained_results.rds`",
  "- Test C results: `outputs/r3_item_anchors/{domain}_swapped_results.rds`",
  "- Summary: `outputs/r3_item_anchors/summary_table.csv`",
  "",
  "## Summary Table (Key Diagnostics)",
  "",
  "Columns:",
  "- `n_theory_pos_v7 / n_theory_items`: how many theory-driven items had `beta1>0` in V7 baseline.",
  "- `testB_cor_dim1, testB_cor_dim2`: correlation of mean ideal points vs baseline after constrained starts (should be ~ +1 if robust).",
  "- `testC_cor_dim1`: correlation of mean dim1 ideal points vs baseline under swapped anchors (should be ~ -1).",
  "- `testC_beta1_flip_cor`: correlation of baseline `beta1` with `-beta1_swapped` across all items (should be high).",
  "- `testC_setA_flip_prop`: share of Set A items whose `beta1` sign flips under swapped anchors (should be near 1 when Set A exists).",
  "",
  "```",
  paste(capture.output(print(summary_tbl, row.names = FALSE)), collapse = "\n"),
  "```",
  "",
  "## Interpretation Guidance",
  "- If Test B correlations are ~1.0, item-start sign constraints do not materially affect the V7 solution (good robustness).",
  "- If Test C dim1 correlations are ~-1.0 and `beta1` flips strongly, the model is behaving as expected under identification changes.",
  "- Deviations in Test C (e.g., dim1 not flipping) are red flags for identification/configuration or a coding issue in anchoring.",
  "",
  "## Domain-Level Diagnostics (From `summary_table.csv`)",
  domain_notes,
  "",
  "## Notes",
  "- Some domains may not contain the theory-driven treaty names in `data/processed/item_codebook.csv` (e.g., investment/security codebooks may not carry those labels). In those cases, Set A can be empty by design and Test A becomes non-informative for that domain."
)
writeLines(lines, md_path, useBytes = TRUE)
cat("Saved findings: ", md_path, "\n", sep = "")
