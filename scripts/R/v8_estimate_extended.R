#!/usr/bin/env Rscript
set.seed(2026)

invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE))

K <- 2L

domains <- c("investment", "environment", "human_rights", "arms_control", "intellectual_property")

anchors_by_domain <- list(
  investment = list(iso = c("DNK", "IRN", "CHN"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  environment = list(iso = c("DNK", "SAU", "AUS"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  human_rights = list(iso = c("DNK", "PRK", "USA"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  arms_control = list(iso = c("NZL", "ISR", "IND"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2))),
  intellectual_property = list(iso = c("DNK", "AGO", "BRA"), pos = rbind(c(+2, 0), c(-2, 0), c(0, -2)))
)

out_dir <- "outputs/v8_extended"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

rcpp_ok <- tryCatch({
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
  TRUE
}, error = function(e) {
  cat("Rcpp failed, falling back to pure R\n")
  source("scripts/R/dynIRT_KD.R")
  FALSE
})

for (domain in domains) {
  cat(sprintf("\n=== V8 Extended Estimation: %s ===\n", domain))

  flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix_extended.rds")))
  N <- nrow(flow$rc)
  J <- ncol(flow$rc)
  T_periods <- flow$T

  if (T_periods != 7L) {
    cat(sprintf("WARNING: expected T=7, got T=%d for %s\n", T_periods, domain))
  }

  anchor_iso <- anchors_by_domain[[domain]]$iso
  anchor_positions <- anchors_by_domain[[domain]]$pos
  anchor_idx <- match(anchor_iso, flow$country_codes)
  if (any(is.na(anchor_idx))) {
    missing <- anchor_iso[is.na(anchor_idx)]
    stop(sprintf("Anchor countries not found in flow matrix for %s: %s", domain, paste(missing, collapse = ", ")))
  }

  x_mu0 <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)
  for (a in seq_along(anchor_idx)) {
    x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
    x_Sigma0[anchor_idx[a], ] <- 0.01
  }

  # PCA initialization (same pattern as V7)
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
  if (npc < K) x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
  for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_start <- array(NA_real_, dim = c(N, K, T_periods))
  for (t in seq_len(T_periods)) x_start[, , t] <- x_pca

  .priors <- list(
    x.mu0 = x_mu0,
    x.Sigma0 = x_Sigma0,
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  )
  .control <- list(
    verbose = TRUE,
    thresh = 1e-4,
    maxit = 5000L,
    checkfreq = 50L,
    estimate_omega = FALSE,
    thresh_loglik = 0.01,
    loglik_patience = 5L,
    ncores = 4L
  )

  cat(sprintf("N=%d, J=%d, T=%d, K=%d | rcpp_ok=%d\n", N, J, T_periods, K, as.integer(rcpp_ok)))
  cat(sprintf(
    "Anchors: %s (%s) | sigma=0.01\n",
    paste(anchor_iso, collapse = ", "),
    paste(apply(anchor_positions, 1, function(r) sprintf("(%+.0f,%+.0f)", r[1], r[2])), collapse = ", ")
  ))

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
      alpha = numeric(J),
      beta = matrix(rnorm(J * K, 0, 0.1), J, K),
      x = x_start
    ),
    .priors = .priors,
    .control = .control,
    K = K
  )
  elapsed <- as.numeric((proc.time() - t0)["elapsed"])

  # Compute mean ideal points across active periods
  sl <- as.integer(flow$startlegis)
  el <- as.integer(flow$endlegis)
  x_mean <- matrix(NA_real_, N, K)
  for (i in seq_len(N)) {
    s <- sl[i] + 1L
    e <- el[i] + 1L
    if (e >= s && s >= 1 && e <= T_periods) {
      x_mean[i, ] <- if (s == e) res$means$x[i, , s] else rowMeans(res$means$x[i, , s:e])
    }
  }
  rownames(x_mean) <- flow$country_codes
  colnames(x_mean) <- paste0("dim", seq_len(K))

  agg <- data.frame(
    period = seq_len(T_periods),
    mean_dim1 = sapply(seq_len(T_periods), function(t) mean(res$means$x[, , t][, 1], na.rm = TRUE)),
    sd_dim1   = sapply(seq_len(T_periods), function(t) sd(res$means$x[, , t][, 1], na.rm = TRUE)),
    mean_dim2 = sapply(seq_len(T_periods), function(t) mean(res$means$x[, , t][, 2], na.rm = TRUE)),
    sd_dim2   = sapply(seq_len(T_periods), function(t) sd(res$means$x[, , t][, 2], na.rm = TRUE))
  )
  if (!is.null(flow$period_labels)) agg$period_label <- flow$period_labels

  out <- list(
    domain = domain,
    strategy = "v8_extended_2024",
    ideal_points = res$means$x,        # N x K x T array
    ideal_points_mean = x_mean,        # N x K matrix
    alpha = res$means$alpha,
    beta = res$means$beta,
    country_codes = flow$country_codes,
    item_labels = flow$item_labels,
    period_labels = flow$period_labels,
    anchors = list(iso = anchor_iso, positions = anchor_positions, sigma = 0.01),
    aggregate = agg,
    runtime = list(
      seconds = elapsed,
      iters = res$runtime$iters,
      conv = res$runtime$conv,
      loglik_trace = res$runtime$loglik_trace
    )
  )

  save_path <- file.path(out_dir, sprintf("%s_results.rds", domain))
  saveRDS(out, save_path)

  cat(sprintf("Convergence: %d | Iters: %d | Time: %.1fs\n", out$runtime$conv, out$runtime$iters, out$runtime$seconds))
  cat(sprintf("Saved: %s\n", save_path))
}

