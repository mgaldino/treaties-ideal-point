#!/usr/bin/env Rscript
# V5: Per-Domain 2D Diagnostic Snapshots for dynIRT_KD (K=2)
# Usage: Rscript scripts/R/v5_per_domain_2d_diagnostic.R [domain1] [domain2] ...
#        Optional: --snapshots=250,500,1000
# Env vars:
#   SNAPSHOTS=250,500,1000
#   NCORES=4

args <- commandArgs(trailingOnly = TRUE)

extract_arg <- function(args, prefix) {
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit) == 0) return(NULL)
  sub(paste0("^", prefix), "", hit[1])
}

parse_int_vec <- function(x) {
  if (is.null(x) || !nzchar(x)) return(integer())
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  vals <- as.integer(parts)
  if (any(is.na(vals))) stop("Invalid integer list: ", x)
  vals
}

snapshots_arg <- extract_arg(args, "--snapshots=")
args <- args[!grepl("^--snapshots=", args)]

snapshots_env <- Sys.getenv("SNAPSHOTS", "")
snapshots <- parse_int_vec(snapshots_arg)
if (length(snapshots) == 0) snapshots <- parse_int_vec(snapshots_env)
if (length(snapshots) == 0) snapshots <- c(250L, 500L)
snapshots <- sort(unique(snapshots))
if (any(snapshots <= 0)) stop("Snapshots must be positive integers")

all_domains <- c("investment", "security", "environment",
                 "human_rights", "arms_control", "intellectual_property")

domains_to_run <- if (length(args) > 0) args else all_domains
invalid <- setdiff(domains_to_run, all_domains)
if (length(invalid) > 0) stop("Unknown domain(s): ", paste(invalid, collapse = ", "))

source("scripts/R/dynIRT_KD.R")
K <- 2L

ncores_env <- Sys.getenv("NCORES", "4")
ncores <- as.integer(ncores_env)
if (is.na(ncores) || ncores < 1L) ncores <- 1L

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

output_dir <- "outputs/v5_per_domain_2d_diag"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Stability thresholds (heuristic; see notes in briefing) ---
stability_thresholds <- list(
  full = list(
    cor_min = 0.99,
    p95_abs_max = 0.10,
    max_abs_max = 0.25
  ),
  mean = list(
    cor_min = 0.995,
    p95_abs_max = 0.05,
    max_abs_max = 0.15
  )
)

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

compute_x_mean <- function(x_est, startlegis, endlegis, T_periods) {
  N <- dim(x_est)[1]
  K <- dim(x_est)[2]
  x_mean <- matrix(NA_real_, nrow = N, ncol = K)
  for (i in seq_len(N)) {
    s <- startlegis[i] + 1L
    e <- endlegis[i] + 1L
    if (e >= s && s >= 1 && e <= T_periods) {
      x_mean[i, ] <- if (s == e) x_est[i, , s] else rowMeans(x_est[i, , s:e])
    }
  }
  x_mean
}

compare_snapshots <- function(x_prev, x_curr, x_mean_prev, x_mean_curr) {
  diff <- x_curr - x_prev
  abs_diff <- abs(diff)
  out <- list(
    max_abs  = max(abs_diff, na.rm = TRUE),
    mean_abs = mean(abs_diff, na.rm = TRUE),
    p95_abs  = as.numeric(stats::quantile(abs_diff, 0.95, na.rm = TRUE))
  )

  K <- dim(x_prev)[2]
  cor_dims <- rep(NA_real_, K)
  for (k in seq_len(K)) {
    cor_dims[k] <- suppressWarnings(
      stats::cor(as.vector(x_prev[, k, ]), as.vector(x_curr[, k, ]),
                 use = "pairwise.complete.obs")
    )
  }
  out$cor_dim1 <- cor_dims[1]
  if (K >= 2) out$cor_dim2 <- cor_dims[2]

  diff_mean <- x_mean_curr - x_mean_prev
  abs_diff_mean <- abs(diff_mean)
  out$max_abs_mean  <- max(abs_diff_mean, na.rm = TRUE)
  out$mean_abs_mean <- mean(abs_diff_mean, na.rm = TRUE)
  out$p95_abs_mean  <- as.numeric(stats::quantile(abs_diff_mean, 0.95, na.rm = TRUE))

  cor_mean_dims <- rep(NA_real_, ncol(x_mean_prev))
  for (k in seq_len(ncol(x_mean_prev))) {
    cor_mean_dims[k] <- suppressWarnings(
      stats::cor(x_mean_prev[, k], x_mean_curr[, k],
                 use = "pairwise.complete.obs")
    )
  }
  out$cor_mean_dim1 <- cor_mean_dims[1]
  if (length(cor_mean_dims) >= 2) out$cor_mean_dim2 <- cor_mean_dims[2]
  out
}

estimate_domain_diag <- function(domain, snapshots, ncores) {
  cat(sprintf("\n========== %s ==========\n", toupper(domain)))

  flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
  if (!file.exists(flow_path)) stop("Missing: ", flow_path)
  flow <- readRDS(flow_path)

  N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
  cat(sprintf("N=%d, J=%d, T=%d\n", N, J, T_periods))

  sl <- as.integer(flow$startlegis)
  el <- as.integer(flow$endlegis)
  bs <- as.integer(flow$bill.session)

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

  x_mu0    <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)
  for (a in seq_along(anchor_idx)) {
    x_mu0[anchor_idx[a], ]    <- anchor_positions[a, ]
    x_Sigma0[anchor_idx[a], ] <- 0.01
  }

  set.seed(2026)
  starts <- build_pca_starts_2d(flow$rc, T_periods, K)

  prev_iter <- 0L
  loglik_all <- numeric(0)
  x_store <- list()
  x_mean_store <- list()
  iter_store <- integer(0)

  alpha <- starts$alpha
  beta  <- starts$beta
  x_est <- starts$x

  for (s in snapshots) {
    iter_needed <- s - prev_iter
    if (iter_needed <= 0L) next

    t0 <- proc.time()
    res <- dynIRT_KD(
      .data = list(
        rc           = flow$rc,
        startlegis   = matrix(sl, ncol = 1),
        endlegis     = matrix(el, ncol = 1),
        bill.session = matrix(bs, ncol = 1),
        T            = T_periods
      ),
      .starts = list(alpha = alpha, beta = beta, x = x_est),
      .priors = list(
        x.mu0      = x_mu0,
        x.Sigma0   = x_Sigma0,
        beta.mu    = rep(0, K + 1),
        beta.sigma = 25 * diag(K + 1),
        omega      = 0.1 * diag(K)
      ),
      .control = list(
        verbose        = TRUE,
        thresh         = 0,       # force full iterations for snapshots
        maxit          = iter_needed,
        checkfreq      = 25L,
        estimate_omega = FALSE,
        thresh_aitken  = NULL,
        thresh_loglik  = NULL,
        ncores         = ncores
      ),
      K = K
    )
    elapsed <- (proc.time() - t0)["elapsed"]

    loglik_all <- c(loglik_all, res$runtime$loglik_trace)
    actual_iter <- prev_iter + res$runtime$iters

    x_est <- res$means$x
    alpha <- res$means$alpha
    beta  <- res$means$beta

    x_mean <- compute_x_mean(x_est, sl, el, T_periods)
    rownames(x_mean) <- flow$country_codes
    colnames(x_mean) <- paste0("dim", seq_len(K))

    snapshot <- list(
      domain            = domain,
      iter              = actual_iter,
      ideal_points      = x_est,
      ideal_points_mean = x_mean,
      alpha             = alpha,
      beta              = beta,
      country_codes     = flow$country_codes,
      period_labels     = flow$period_labels,
      item_labels       = flow$item_labels,
      anchors           = list(iso = anchor_iso, positions = anchor_positions),
      runtime           = list(
        iters        = actual_iter,
        seconds      = as.numeric(elapsed),
        conv         = res$runtime$conv,
        loglik_trace = loglik_all
      ),
      omega             = res$omega
    )
    saveRDS(snapshot, file.path(output_dir, paste0(domain, "_snapshot_iter", actual_iter, ".rds")))
    cat(sprintf("Saved snapshot: %s/%s_snapshot_iter%d.rds\n",
                output_dir, domain, actual_iter))

    x_store[[length(x_store) + 1L]] <- x_est
    x_mean_store[[length(x_mean_store) + 1L]] <- x_mean
    iter_store <- c(iter_store, actual_iter)

    prev_iter <- actual_iter
    if (actual_iter < s) {
      cat(sprintf("WARNING: early stop at iter %d (target %d). Halting.\n",
                  actual_iter, s))
      break
    }
  }

  summary_rows <- list()
  if (length(iter_store) >= 2L) {
    for (i in 2:length(iter_store)) {
      metrics <- compare_snapshots(
        x_store[[i - 1L]], x_store[[i]],
        x_mean_store[[i - 1L]], x_mean_store[[i]]
      )
      ll_prev <- loglik_all[iter_store[i - 1L]]
      ll_curr <- loglik_all[iter_store[i]]
      stable_full <- is.finite(metrics$cor_dim1) &&
        is.finite(metrics$cor_dim2) &&
        is.finite(metrics$p95_abs) &&
        is.finite(metrics$max_abs) &&
        metrics$cor_dim1 >= stability_thresholds$full$cor_min &&
        metrics$cor_dim2 >= stability_thresholds$full$cor_min &&
        metrics$p95_abs <= stability_thresholds$full$p95_abs_max &&
        metrics$max_abs <= stability_thresholds$full$max_abs_max
      stable_mean <- is.finite(metrics$cor_mean_dim1) &&
        is.finite(metrics$cor_mean_dim2) &&
        is.finite(metrics$p95_abs_mean) &&
        is.finite(metrics$max_abs_mean) &&
        metrics$cor_mean_dim1 >= stability_thresholds$mean$cor_min &&
        metrics$cor_mean_dim2 >= stability_thresholds$mean$cor_min &&
        metrics$p95_abs_mean <= stability_thresholds$mean$p95_abs_max &&
        metrics$max_abs_mean <= stability_thresholds$mean$max_abs_max
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        domain          = domain,
        iter_prev       = iter_store[i - 1L],
        iter_curr       = iter_store[i],
        loglik_prev     = ll_prev,
        loglik_curr     = ll_curr,
        loglik_delta    = ll_curr - ll_prev,
        max_abs         = metrics$max_abs,
        mean_abs        = metrics$mean_abs,
        p95_abs         = metrics$p95_abs,
        cor_dim1        = metrics$cor_dim1,
        cor_dim2        = metrics$cor_dim2,
        max_abs_mean    = metrics$max_abs_mean,
        mean_abs_mean   = metrics$mean_abs_mean,
        p95_abs_mean    = metrics$p95_abs_mean,
        cor_mean_dim1   = metrics$cor_mean_dim1,
        cor_mean_dim2   = metrics$cor_mean_dim2,
        stable_full     = stable_full,
        stable_mean     = stable_mean,
        stable_both     = stable_full && stable_mean,
        full_cor_min    = stability_thresholds$full$cor_min,
        full_p95_max    = stability_thresholds$full$p95_abs_max,
        full_max_max    = stability_thresholds$full$max_abs_max,
        mean_cor_min    = stability_thresholds$mean$cor_min,
        mean_p95_max    = stability_thresholds$mean$p95_abs_max,
        mean_max_max    = stability_thresholds$mean$max_abs_max,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(summary_rows) == 0) return(NULL)
  do.call(rbind, summary_rows)
}

ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
log_file <- sprintf("logs/v5_per_domain_2d_diag_%s.log", ts)
sink(log_file, split = TRUE)

cat(sprintf("V5 Per-Domain 2D Diagnostic Snapshots\nTime: %s\nDomains: %s\nK = %d\n",
            ts, paste(domains_to_run, collapse = ", "), K))
cat(sprintf("Snapshots: %s\nncores: %d\n\n",
            paste(snapshots, collapse = ", "), ncores))
cat(sprintf("Stability thresholds (full): cor >= %.3f, p95_abs <= %.3f, max_abs <= %.3f\n",
            stability_thresholds$full$cor_min,
            stability_thresholds$full$p95_abs_max,
            stability_thresholds$full$max_abs_max))
cat(sprintf("Stability thresholds (mean): cor >= %.3f, p95_abs_mean <= %.3f, max_abs_mean <= %.3f\n\n",
            stability_thresholds$mean$cor_min,
            stability_thresholds$mean$p95_abs_max,
            stability_thresholds$mean$max_abs_max))

summary_all <- list()
for (d in domains_to_run) {
  tryCatch(
    summary_all[[d]] <- estimate_domain_diag(d, snapshots, ncores),
    error = function(e) cat(sprintf("\nERROR [%s]: %s\n", d, e$message))
  )
}

summary_df <- do.call(rbind, summary_all)
if (!is.null(summary_df)) {
  summary_csv <- file.path(output_dir, "diagnostic_summary.csv")
  summary_rds <- file.path(output_dir, "diagnostic_summary.rds")
  write.csv(summary_df, summary_csv, row.names = FALSE)
  saveRDS(summary_df, summary_rds)
  cat(sprintf("\nSaved summary: %s\nSaved summary: %s\n",
              summary_csv, summary_rds))
}

sink()
cat(sprintf("\nLog written to: %s\n", log_file))
