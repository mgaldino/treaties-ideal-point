#!/usr/bin/env Rscript

# Profiling script for dynIRT_KD (R version)

source("scripts/R/dynIRT_KD.R")
set.seed(2026)

K <- 2L
domain <- "investment"

flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
if (!file.exists(flow_path)) stop("Missing: ", flow_path)
flow <- readRDS(flow_path)

# Anchor configuration (from v5_per_domain_2d.R)
anchor_config <- list(
  investment = list(primary = c("DNK", "IRN", "CHN"),
                    fallback = c("DNK", "IRN", "CHN"))
)
anchor_positions <- rbind(
  c(+2, +2),
  c(-2, -2),
  c(+1, -1)
)

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

N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
sl <- as.integer(flow$startlegis)
el <- as.integer(flow$endlegis)
bs <- as.integer(flow$bill.session)

cfg <- anchor_config[[domain]]
anchor_iso <- cfg$primary
anchor_idx <- match(anchor_iso, flow$country_codes)
if (any(is.na(anchor_idx))) {
  anchor_iso <- cfg$fallback
  anchor_idx <- match(anchor_iso, flow$country_codes)
  if (any(is.na(anchor_idx))) stop("Anchor(s) not in data")
}

x_mu0    <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ]    <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

starts <- build_pca_starts_2d(flow$rc, T_periods, K)

log_file <- "logs/rcpp_codex_profile.log"
prof_file <- "logs/rcpp_codex_profile.prof"

sink(log_file, split = TRUE)
cat(sprintf("Profiling dynIRT_KD (domain=%s)\n", domain))
cat(sprintf("N=%d, J=%d, T=%d\n", N, J, T_periods))

Rprof(prof_file, interval = 0.01)
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
    maxit          = 50L,
    checkfreq      = 10L,
    estimate_omega = FALSE,
    thresh_aitken  = 1e-4,
    ncores         = 4L
  ),
  K = K
)
Rprof(NULL)

prof_summary <- summaryRprof(prof_file)
print(prof_summary)

saveRDS(
  list(summary = prof_summary, runtime = res$runtime),
  file = "outputs/rcpp_codex_profile_summary.rds"
)

sink()
cat(sprintf("Profiling log written to: %s\n", log_file))
