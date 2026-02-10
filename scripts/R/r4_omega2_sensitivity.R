#!/usr/bin/env Rscript
set.seed(2026)

# Robustness check R4: omega2 (evolution variance) sensitivity analysis for 2D dynIRT_KD

# Prefer Rcpp (fast), but fall back to pure R if compilation/linking fails.
rcpp_ok <- tryCatch({
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
  TRUE
}, error = function(e) {
  cat("Rcpp failed, falling back to pure R\n")
  source("scripts/R/dynIRT_KD.R")
  FALSE
})

K <- 2L
omega2_grid <- c(0.01, 0.05, 0.1, 0.2, 0.5)
domains <- c(
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)

# V7 anchor configurations (must match exactly).
anchors_by_domain <- list(
  investment = list(
    iso = c("DNK", "IRN", "CHN"),
    positions = rbind(c(+2, 0), c(-2, 0), c(0, -2))
  ),
  security = list(
    iso = c("DNK", "IRN", "UKR"),
    positions = rbind(c(+2, 0), c(-2, 0), c(0, -2))
  ),
  environment = list(
    iso = c("DNK", "SAU", "AUS"),
    positions = rbind(c(+2, 0), c(-2, 0), c(0, -2))
  ),
  human_rights = list(
    iso = c("DNK", "PRK", "USA"),
    positions = rbind(c(+2, 0), c(-2, 0), c(0, -2))
  ),
  arms_control = list(
    iso = c("NZL", "ISR", "IND"),
    positions = rbind(c(+2, 0), c(-2, 0), c(0, -2))
  ),
  intellectual_property = list(
    iso = c("DNK", "AGO", "BRA"),
    positions = rbind(c(+2, 0), c(-2, 0), c(0, -2))
  )
)

out_dir <- "outputs/r4_omega2_sensitivity"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

omega2_to_str <- function(x) {
  # Stable-ish, filesystem-safe numeric string with minimal noise.
  s <- format(x, scientific = FALSE, trim = TRUE)
  s <- sub("\\s+", "", s)
  s
}

pca_init_x_start <- function(flow, K) {
  # PCA initialization exactly as V7 scripts (see v7_environment_country_anchors.R).
  N <- nrow(flow$rc)
  J <- ncol(flow$rc)
  T_periods <- flow$T

  rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }

  col_var <- apply(rc_num, 2, var)
  rc_pca <- if (any(col_var == 0, na.rm = TRUE)) rc_num[, col_var > 0, drop = FALSE] else rc_num
  pca <- prcomp(rc_pca, center = TRUE, scale. = FALSE)
  npc <- min(K, ncol(pca$x))
  x_pca <- pca$x[, seq_len(npc), drop = FALSE]
  if (npc < K) {
    x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
  }
  for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))

  x_start <- array(NA_real_, dim = c(N, K, T_periods))
  for (t in seq_len(T_periods)) x_start[, , t] <- x_pca
  x_start
}

make_anchor_priors <- function(flow, anchor_iso, anchor_positions, K) {
  N <- nrow(flow$rc)
  x_mu0 <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)

  anchor_idx <- match(anchor_iso, flow$country_codes)
  if (any(is.na(anchor_idx))) {
    missing <- anchor_iso[is.na(anchor_idx)]
    stop(sprintf("Anchor countries not found in flow matrix: %s", paste(missing, collapse = ", ")))
  }
  for (a in seq_along(anchor_idx)) {
    x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
    x_Sigma0[anchor_idx[a], ] <- 0.01
  }

  list(x_mu0 = x_mu0, x_Sigma0 = x_Sigma0, anchor_idx = anchor_idx)
}

flatten_dim <- function(x_arr, k) {
  # x_arr: N x K x T array
  as.vector(x_arr[, k, , drop = FALSE])
}

rows <- list()

for (domain in domains) {
  flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
  baseline_path <- file.path("outputs/v7_country_anchors", paste0(domain, "_results.rds"))

  cat(sprintf("\n=== Domain: %s ===\n", domain))
  cat(sprintf("Flow: %s\n", flow_path))
  cat(sprintf("Baseline: %s\n", baseline_path))

  flow <- readRDS(flow_path)
  N <- nrow(flow$rc)
  J <- ncol(flow$rc)
  T_periods <- flow$T

  baseline <- readRDS(baseline_path)
  x_base <- baseline$ideal_points
  if (is.null(x_base)) stop(sprintf("Baseline file missing $ideal_points: %s", baseline_path))
  if (!identical(dim(x_base), c(N, K, T_periods))) {
    stop(sprintf("Baseline ideal point dims mismatch for %s: baseline=%s, expected=%s",
                 domain,
                 paste(dim(x_base), collapse = "x"),
                 paste(c(N, K, T_periods), collapse = "x")))
  }

  anch <- anchors_by_domain[[domain]]
  if (is.null(anch)) stop(sprintf("Missing anchors for domain: %s", domain))

  anchor_iso <- anch$iso
  anchor_positions <- anch$positions
  if (length(anchor_iso) != 3L || !is.matrix(anchor_positions) || nrow(anchor_positions) != 3L || ncol(anchor_positions) != K) {
    stop(sprintf("Bad anchor spec for %s", domain))
  }

  pri_anchor <- make_anchor_priors(flow, anchor_iso, anchor_positions, K)
  # Match V7 per-domain scripts: each domain run starts from the same RNG state.
  # Also keeps the omega2 comparisons "ceteris paribus" (same initialization across omega2 values).
  set.seed(2026)
  x_start <- pca_init_x_start(flow, K)
  beta_start <- matrix(rnorm(J * K, 0, 0.1), J, K)
  alpha_start <- numeric(J)

  for (omega2 in omega2_grid) {
    omega2_str <- omega2_to_str(omega2)
    cat(sprintf("\nRunning: domain=%s | omega2=%s | N=%d J=%d T=%d K=%d | rcpp_ok=%d\n",
                domain, omega2_str, N, J, T_periods, K, as.integer(rcpp_ok)))

    t0 <- proc.time()
    res <- dynIRT_KD(
      .data = list(
        rc = flow$rc,
        startlegis = matrix(as.integer(flow$startlegis), ncol = 1),
        endlegis = matrix(as.integer(flow$endlegis), ncol = 1),
        bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
        T = T_periods
      ),
      .starts = list(
        alpha = alpha_start,
        beta = beta_start,
        x = x_start
      ),
      .priors = list(
        x.mu0 = pri_anchor$x_mu0,
        x.Sigma0 = pri_anchor$x_Sigma0,
        beta.mu = rep(0, K + 1),
        beta.sigma = 25 * diag(K + 1),  # Diffuse prior on item parameters (sd=5 per element)
        omega = omega2 * diag(K)
      ),
      .control = list(
        verbose = TRUE,
        thresh = 1e-4,
        maxit = 5000L,
        checkfreq = 50L,
        estimate_omega = FALSE,
        thresh_loglik = 0.01,
        loglik_patience = 5L,
        ncores = 4L
      ),
      K = K
    )
    elapsed <- (proc.time() - t0)["elapsed"]

    iters <- res$runtime$iters
    conv <- res$runtime$conv
    cat(sprintf("Done: omega2=%s | seconds=%.1f | iters=%d | conv=%d\n", omega2_str, elapsed, iters, conv))

    x_hat <- res$means$x
    mean_dim1_by_period <- sapply(seq_len(T_periods), function(t) mean(x_hat[, 1, t], na.rm = TRUE))
    mean_dim2_by_period <- sapply(seq_len(T_periods), function(t) mean(x_hat[, 2, t], na.rm = TRUE))

    cor_dim1 <- suppressWarnings(cor(flatten_dim(x_hat, 1), flatten_dim(x_base, 1), use = "complete.obs"))
    cor_dim2 <- suppressWarnings(cor(flatten_dim(x_hat, 2), flatten_dim(x_base, 2), use = "complete.obs"))

    trend_df <- data.frame(period = seq_len(T_periods), mean_dim1 = mean_dim1_by_period)
    slope <- as.numeric(coef(lm(mean_dim1 ~ period, data = trend_df))[["period"]])

    out_obj <- list(
      domain = domain,
      strategy = "r4_omega2_sensitivity",
      omega2 = omega2,
      omega = omega2 * diag(K),
      ideal_points = x_hat,
      alpha = res$means$alpha,
      beta = res$means$beta,
      country_codes = flow$country_codes,
      item_labels = flow$item_labels,
      period_labels = flow$period_labels,
      anchors = list(
        iso = anchor_iso,
        positions = anchor_positions,
        sigma = 0.01
      ),
      aggregate = data.frame(
        period = seq_len(T_periods),
        mean_dim1 = mean_dim1_by_period,
        mean_dim2 = mean_dim2_by_period
      ),
      runtime = list(
        seconds = as.numeric(elapsed),
        iters = iters,
        conv = conv,
        loglik_trace = res$runtime$loglik_trace
      ),
      baseline = list(
        path = baseline_path,
        cor_dim1_with_baseline = cor_dim1,
        cor_dim2_with_baseline = cor_dim2
      )
    )

    out_path <- file.path(out_dir, sprintf("%s_omega2_%s_results.rds", domain, omega2_str))
    saveRDS(out_obj, out_path)
    cat(sprintf("Saved: %s\n", out_path))

    rows[[length(rows) + 1L]] <- data.frame(
      domain = domain,
      omega2 = omega2,
      iters = iters,
      conv = conv,
      cor_dim1_with_baseline = cor_dim1,
      cor_dim2_with_baseline = cor_dim2,
      mean_dim1_p1 = mean_dim1_by_period[1],
      mean_dim1_pT = mean_dim1_by_period[T_periods],
      trend_slope_dim1 = slope,
      stringsAsFactors = FALSE
    )
  }
}

tab <- do.call(rbind, rows)
tab <- tab[order(tab$domain, tab$omega2), , drop = FALSE]
csv_path <- file.path(out_dir, "sensitivity_table.csv")
write.csv(tab, csv_path, row.names = FALSE)
cat(sprintf("\nWrote: %s\n", csv_path))

report_path <- file.path(out_dir, "sensitivity_report.txt")
con <- file(report_path, open = "wt", encoding = "UTF-8")
writeLines(sprintf("R4 omega2 sensitivity (2D dynIRT_KD) | run_date_utc=%s", format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC")), con)
writeLines(sprintf("omega2 grid: %s", paste(omega2_grid, collapse = ", ")), con)
writeLines(sprintf("domains: %s", paste(domains, collapse = ", ")), con)
writeLines("", con)

for (domain in domains) {
  d <- tab[tab$domain == domain, , drop = FALSE]
  if (nrow(d) == 0) next
  writeLines(sprintf("Domain: %s", domain), con)
  writeLines(sprintf("  Convergence: %d/%d converged", sum(d$conv == 1, na.rm = TRUE), nrow(d)), con)
  # Correlations vs baseline (omega2=0.1 in V7).
  d_cor <- d[, c("omega2", "cor_dim1_with_baseline", "cor_dim2_with_baseline", "iters", "conv")]
  # Render a compact fixed-width table.
  fmt <- function(x, digits = 3) ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits))
  for (i in seq_len(nrow(d_cor))) {
    writeLines(sprintf("  omega2=%s | cor(dim1)=%s | cor(dim2)=%s | iters=%d | conv=%d",
                       omega2_to_str(d_cor$omega2[i]),
                       fmt(d_cor$cor_dim1_with_baseline[i], 3),
                       fmt(d_cor$cor_dim2_with_baseline[i], 3),
                       d_cor$iters[i],
                       d_cor$conv[i]), con)
  }
  # Trend summary
  d_tr <- d[order(d$omega2), , drop = FALSE]
  writeLines(sprintf("  Mean dim1 (p1 -> pT) at omega2=0.1: %s -> %s | slope=%s",
                     fmt(d_tr$mean_dim1_p1[d_tr$omega2 == 0.1][1], 3),
                     fmt(d_tr$mean_dim1_pT[d_tr$omega2 == 0.1][1], 3),
                     fmt(d_tr$trend_slope_dim1[d_tr$omega2 == 0.1][1], 5)), con)
  writeLines("", con)
}
close(con)
cat(sprintf("Wrote: %s\n", report_path))
