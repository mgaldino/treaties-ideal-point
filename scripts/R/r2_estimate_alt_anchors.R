#!/usr/bin/env Rscript
set.seed(2026)

# R2 Robustness Check: Alternative country anchor sensitivity (estimation step)
# Runs dynIRT_KD() for each domain x alternative anchor set (Alt1, Alt2),
# saving results to outputs/r2_alt_anchors/{domain}_{set_name}_results.rds

options(stringsAsFactors = FALSE)

domains <- c(
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)

dir.create("outputs/r2_alt_anchors", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

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

anchor_csv <- file.path("outputs/r2_alt_anchors", "anchor_sets.csv")
if (!file.exists(anchor_csv)) {
  stop(sprintf("Missing anchor sets CSV (run Phase 1 first): %s", anchor_csv))
}
anchors <- read.csv(anchor_csv)
req_cols <- c("domain", "set_name", "anchor_role", "iso3", "pc1_loading")
if (!all(req_cols %in% names(anchors))) {
  stop(sprintf("anchor_sets.csv must contain columns: %s", paste(req_cols, collapse = ", ")))
}
anchors$domain <- as.character(anchors$domain)
anchors$set_name <- as.character(anchors$set_name)
anchors$anchor_role <- as.character(anchors$anchor_role)
anchors$iso3 <- toupper(trimws(as.character(anchors$iso3)))

make_pca_start <- function(flow, K) {
  N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

  rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }

  # Drop zero-variance columns before PCA
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

compute_x_mean <- function(x_array, startlegis, endlegis) {
  N <- dim(x_array)[1]
  K <- dim(x_array)[2]
  T_periods <- dim(x_array)[3]
  sl <- as.integer(startlegis)
  el <- as.integer(endlegis)
  x_mean <- matrix(NA_real_, N, K)
  for (i in seq_len(N)) {
    s <- sl[i] + 1L
    e <- el[i] + 1L
    if (e >= s && s >= 1L && e <= T_periods) {
      x_mean[i, ] <- if (s == e) x_array[i, , s] else rowMeans(x_array[i, , s:e])
    }
  }
  x_mean
}

cat(sprintf("R2 Phase 2: Estimating with alternative anchors | K=%d | rcpp_ok=%d\n\n",
            K, as.integer(rcpp_ok)))

for (domain in domains) {
  flow_file <- file.path("data/processed", sprintf("%s_flow_matrix.rds", domain))
  if (!file.exists(flow_file)) stop(sprintf("Missing flow matrix: %s", flow_file))
  flow <- readRDS(flow_file)

  N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
  cat(sprintf("Domain: %s | N=%d, J=%d, T=%d\n", domain, N, J, T_periods))

  for (set_name in c("Alt1", "Alt2")) {
    key <- anchors[anchors$domain == domain & anchors$set_name == set_name, , drop = FALSE]
    if (nrow(key) != 3L) {
      stop(sprintf("Expected 3 anchors for %s x %s, found %d", domain, set_name, nrow(key)))
    }

    # Ensure deterministic order for positions assignment
    role_order <- c("dim1_pos", "dim1_neg", "dim2")
    key$anchor_role <- factor(key$anchor_role, levels = role_order)
    key <- key[order(key$anchor_role), , drop = FALSE]

    anchor_iso <- key$iso3
    anchor_idx <- match(anchor_iso, flow$country_codes)
    if (any(is.na(anchor_idx))) {
      missing <- anchor_iso[is.na(anchor_idx)]
      stop(sprintf("Anchor countries not found in flow matrix for %s x %s: %s",
                   domain, set_name, paste(missing, collapse = ", ")))
    }

    anchor_positions <- rbind(
      c(+2,  0),  # dim1 positive
      c(-2,  0),  # dim1 negative
      c( 0, -2)   # dim2 anchor
    )

    x_mu0 <- matrix(0, nrow = N, ncol = K)
    x_Sigma0 <- matrix(1, nrow = N, ncol = K)
    for (a in seq_along(anchor_idx)) {
      x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
      x_Sigma0[anchor_idx[a], ] <- 0.01
    }

    # PCA initialization (same as V7 scripts)
    x_start <- make_pca_start(flow, K)

    ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    log_file <- sprintf("logs/r2_%s_%s_country_%s.log", domain, set_name, ts)
    sink(log_file, split = TRUE)
    cat(sprintf("R2 Alt Anchors: %s | set=%s | N=%d, J=%d, T=%d, K=%d | rcpp_ok=%d\n",
                domain, set_name, N, J, T_periods, K, as.integer(rcpp_ok)))
    cat(sprintf("Anchors (%s): %s\n",
                paste(role_order, collapse = ", "),
                paste(anchor_iso, collapse = ", ")))
    cat(sprintf("Anchor positions: %s\n",
                paste(apply(anchor_positions, 1, function(r) sprintf("(%+.0f,%+.0f)", r[1], r[2])),
                      collapse = ", ")))
    cat("\n")

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
      .priors = list(
        x.mu0 = x_mu0,
        x.Sigma0 = x_Sigma0,
        beta.mu = rep(0, K + 1),
        beta.sigma = 25 * diag(K + 1),  # Diffuse prior on item parameters (sd=5 per element)
        omega = 0.1 * diag(K)           # Evolution variance; see R4 sensitivity check (0.01--0.5)
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
    cat(sprintf("\nDone: %.1fs | Iters: %d | Conv: %d\n",
                elapsed, res$runtime$iters, res$runtime$conv))

    x_mean <- compute_x_mean(res$means$x, flow$startlegis, flow$endlegis)
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
    cat("\nAggregate trends:\n")
    print(agg)

    out <- list(
      domain = domain,
      strategy = "r2_alt_country_anchors",
      alt_set = set_name,
      ideal_points = res$means$x,
      ideal_points_mean = x_mean,
      alpha = res$means$alpha,
      beta = res$means$beta,
      country_codes = flow$country_codes,
      item_labels = flow$item_labels,
      period_labels = flow$period_labels,
      anchors = list(
        iso = anchor_iso,
        roles = role_order,
        positions = anchor_positions,
        pc1_loading = key$pc1_loading
      ),
      aggregate = agg,
      runtime = list(
        seconds = as.numeric(elapsed),
        iters = res$runtime$iters,
        conv = res$runtime$conv,
        loglik_trace = res$runtime$loglik_trace
      )
    )

    out_file <- sprintf("outputs/r2_alt_anchors/%s_%s_results.rds", domain, set_name)
    saveRDS(out, out_file)
    cat(sprintf("\nSaved: %s\n", out_file))
    sink()

    cat(sprintf("  %s x %s saved | conv=%d | iters=%d | seconds=%.1f\n",
                domain, set_name, res$runtime$conv, res$runtime$iters, as.numeric(elapsed)))
  }
  cat("\n")
}

