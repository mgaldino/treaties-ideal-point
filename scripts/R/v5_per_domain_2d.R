#!/usr/bin/env Rscript
# V5: Per-Domain 2D Estimation with dynIRT_KD (K=2)
# Usage: Rscript scripts/R/v5_per_domain_2d.R [domain1] [domain2] ...
# Run from project root: /Users/manoelgaldino/Documents/DCP/Papers/Ideal point

args <- commandArgs(trailingOnly = TRUE)

all_domains <- c("investment", "security", "environment",
                 "human_rights", "arms_control", "intellectual_property")

domains_to_run <- if (length(args) > 0) args else all_domains
invalid <- setdiff(domains_to_run, all_domains)
if (length(invalid) > 0) stop("Unknown domain(s): ", paste(invalid, collapse = ", "))

source("scripts/R/dynIRT_KD.R")
set.seed(2026)
K <- 2L
maxit_default <- 1500L
maxit_env <- Sys.getenv("MAXIT", "")
maxit <- if (nzchar(maxit_env)) as.integer(maxit_env) else maxit_default
if (is.na(maxit) || maxit < 1L) {
  stop("MAXIT must be a positive integer")
}

# --- Anchor configuration (K+1 = 3 anchors for K=2) ---
# Primary set: DNK/IRN/CHN (estimation_plan_2d.md §3.3)
# Fallback: domain-specific 1D pair + CHN
anchor_config <- list(
  investment            = list(primary = c("DNK", "IRN", "CHN"),
                               fallback = c("DNK", "IRN", "CHN")),
  security              = list(primary = c("DNK", "IRN", "CHN"),
                               fallback = c("DNK", "IRN", "CHN")),
  environment           = list(primary = c("DNK", "IRN", "CHN"),
                               fallback = c("DNK", "SAU", "CHN")),
  human_rights          = list(primary = c("DNK", "IRN", "CHN"),
                               fallback = c("DNK", "PRK", "CHN")),
  arms_control          = list(primary = c("DNK", "IRN", "CHN"),
                               fallback = c("NZL", "ISR", "CHN")),
  intellectual_property = list(primary = c("DNK", "IRN", "CHN"),
                               fallback = c("DNK", "AGO", "CHN"))
)

# Non-collinear positions (area = 4, verified)
anchor_positions <- rbind(
  c(+2, +2),  # anchor 1: pro-ILO
  c(-2, -2),  # anchor 2: anti-ILO
  c(+1, -1)   # anchor 3: mixed
)

output_dir <- "outputs/v5_per_domain_2d"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- PCA-based initialization for K=2 ---
build_pca_starts_2d <- function(rc, T_periods, K = 2L) {
  N <- nrow(rc); J <- ncol(rc)
  rc_num <- matrix(as.numeric(rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }
  pca <- prcomp(rc_num, center = TRUE, scale. = FALSE)
  npc <- min(K, ncol(pca$x))
  x_pca <- pca$x[, seq_len(npc), drop = FALSE]
  if (npc < K) {
    x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
  }
  for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_start <- array(NA_real_, dim = c(N, K, T_periods))
  for (t in seq_len(T_periods)) x_start[, , t] <- x_pca
  list(
    x     = x_start,
    alpha = numeric(J),
    beta  = matrix(rnorm(J * K, 0, 0.1), nrow = J, ncol = K)
  )
}

# --- Estimate one domain ---
estimate_domain_2d <- function(domain) {
  cat(sprintf("\n========== %s ==========\n", toupper(domain)))

  flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
  if (!file.exists(flow_path)) stop("Missing: ", flow_path)
  flow <- readRDS(flow_path)

  N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
  cat(sprintf("N=%d, J=%d, T=%d\n", N, J, T_periods))
  if (J < 50) cat(sprintf("WARNING: only %d items — K=2 may be weakly identified\n", J))

  # Flatten startlegis/endlegis to integer vectors
  sl <- as.integer(flow$startlegis)
  el <- as.integer(flow$endlegis)
  bs <- as.integer(flow$bill.session)

  # Select anchors (primary, then fallback)
  cfg <- anchor_config[[domain]]
  anchor_iso <- cfg$primary
  anchor_idx <- match(anchor_iso, flow$country_codes)
  if (any(is.na(anchor_idx))) {
    missing <- anchor_iso[is.na(anchor_idx)]
    cat(sprintf("Primary anchors missing: %s. Trying fallback.\n",
                paste(missing, collapse = ", ")))
    anchor_iso <- cfg$fallback
    anchor_idx <- match(anchor_iso, flow$country_codes)
    if (any(is.na(anchor_idx))) {
      stop("Anchor(s) not in data: ",
           paste(anchor_iso[is.na(anchor_idx)], collapse = ", "))
    }
  }
  cat(sprintf("Anchors: %s (idx %s)\n",
              paste(anchor_iso, collapse = "/"),
              paste(anchor_idx, collapse = "/")))

  # Priors
  x_mu0    <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)
  for (a in seq_along(anchor_idx)) {
    x_mu0[anchor_idx[a], ]    <- anchor_positions[a, ]
    x_Sigma0[anchor_idx[a], ] <- 0.01
  }

  starts <- build_pca_starts_2d(flow$rc, T_periods, K)

  t0 <- proc.time()
  res <- dynIRT_KD(
    .data = list(
      rc           = flow$rc,
      startlegis   = matrix(sl, ncol = 1),
      endlegis     = matrix(el, ncol = 1),
      bill.session = matrix(bs, ncol = 1),
      T            = T_periods
    ),
    .starts = list(alpha = starts$alpha, beta = starts$beta, x = starts$x),
    .priors = list(
      x.mu0      = x_mu0,
      x.Sigma0   = x_Sigma0,
      beta.mu    = rep(0, K + 1),
      beta.sigma = 25 * diag(K + 1),
      omega      = 0.1 * diag(K)
    ),
    .control = list(
      verbose        = TRUE,
      thresh         = 1e-4,
      maxit          = maxit,
      checkfreq      = 25L,
      estimate_omega = FALSE,
      thresh_aitken  = 1e-4,
      ncores         = 4L
    ),
    K = K
  )
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("Elapsed: %.1fs | Iters: %d | Conv: %d\n",
              elapsed, res$runtime$iters, res$runtime$conv))

  # Summary: mean ideal point over active periods
  x_est  <- res$means$x  # N x K x T
  x_mean <- matrix(NA_real_, nrow = N, ncol = K)
  for (i in seq_len(N)) {
    s <- sl[i] + 1L
    e <- el[i] + 1L
    if (e >= s && s >= 1 && e <= T_periods) {
      x_mean[i, ] <- if (s == e) x_est[i, , s] else rowMeans(x_est[i, , s:e])
    }
  }
  rownames(x_mean) <- flow$country_codes
  colnames(x_mean) <- paste0("dim", seq_len(K))

  out <- list(
    domain            = domain,
    ideal_points      = x_est,
    ideal_points_mean = x_mean,
    alpha             = res$means$alpha,
    beta              = res$means$beta,
    country_codes     = flow$country_codes,
    period_labels     = flow$period_labels,
    item_labels       = flow$item_labels,
    anchors           = list(iso = anchor_iso, positions = anchor_positions),
    runtime           = list(seconds = elapsed, iters = res$runtime$iters,
                             conv = res$runtime$conv,
                             loglik_trace = res$runtime$loglik_trace),
    omega             = res$omega
  )
  saveRDS(out, file.path(output_dir, paste0(domain, "_2d_results.rds")))
  cat(sprintf("Saved: %s/%s_2d_results.rds\n", output_dir, domain))
  invisible(out)
}

# --- Main ---
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v5_per_domain_2d_%s.log", ts)
sink(log_file, split = TRUE)

cat(sprintf("V5 Per-Domain 2D Estimation\nTime: %s\nDomains: %s\nK = %d\n\n",
            ts, paste(domains_to_run, collapse = ", "), K))
cat(sprintf("maxit = %d\n\n", maxit))

results <- list()
for (d in domains_to_run) {
  tryCatch(
    results[[d]] <- estimate_domain_2d(d),
    error = function(e) cat(sprintf("\nERROR [%s]: %s\n", d, e$message))
  )
}

cat("\n\n========== SUMMARY ==========\n")
for (d in domains_to_run) {
  if (!is.null(results[[d]])) {
    r <- results[[d]]$runtime
    cat(sprintf("%-25s conv=%d  iters=%3d  time=%6.1fs\n",
                d, r$conv, r$iters, r$seconds))
  } else {
    cat(sprintf("%-25s FAILED\n", d))
  }
}

sink()
cat(sprintf("\nLog written to: %s\n", log_file))
