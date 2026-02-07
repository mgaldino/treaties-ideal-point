#!/usr/bin/env Rscript
set.seed(2026)

source("scripts/R/dynIRT_KD.R")

K <- 2L
flow <- readRDS(file.path("data/processed", "investment_flow_matrix.rds"))
N <- nrow(flow$rc)
J <- ncol(flow$rc)
T_periods <- as.integer(flow$T)

anchor_iso <- c("DNK", "IRN", "CHN")
anchor_idx <- match(anchor_iso, flow$country_codes)
if (any(is.na(anchor_idx))) {
  missing <- anchor_iso[is.na(anchor_idx)]
  stop(sprintf("Missing anchors in data: %s", paste(missing, collapse = ", ")))
}
anchor_positions <- rbind(c(+2, +2), c(-2, -2), c(+1, -1))

x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

# PCA starts
rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA
rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) {
  m <- mean(rc_num[, j], na.rm = TRUE)
  if (is.nan(m)) m <- 0.5
  rc_num[is.na(rc_num[, j]), j] <- m
}

pca <- prcomp(rc_num, center = TRUE, scale. = FALSE)
x_pca <- pca$x[, 1:K]
for (k in seq_len(K)) {
  x_pca[, k] <- as.numeric(scale(x_pca[, k]))
}

x_start <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) {
  x_start[, , t] <- x_pca
}

alpha_start <- numeric(J)
beta_start <- matrix(rnorm(J * K, 0, 0.1), nrow = J, ncol = K)

res <- dynIRT_KD(
  .data = list(
    rc = flow$rc,
    startlegis = matrix(as.integer(flow$startlegis), ncol = 1),
    endlegis = matrix(as.integer(flow$endlegis), ncol = 1),
    bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
    T = T_periods
  ),
  .starts = list(alpha = alpha_start, beta = beta_start, x = x_start),
  .priors = list(
    x.mu0 = x_mu0,
    x.Sigma0 = x_Sigma0,
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  ),
  .control = list(
    verbose = FALSE,
    thresh = 1e-10,
    maxit = 50L,
    checkfreq = 50L,
    estimate_omega = FALSE,
    ncores = 1L,
    thresh_loglik = NULL,
    thresh_aitken = NULL
  ),
  K = K
)

out_dir <- file.path("data/processed/python_validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv_noheader <- function(x, path) {
  write.table(x, path, sep = ",", row.names = FALSE, col.names = FALSE)
}

write_csv_noheader(flow$rc, file.path(out_dir, "rc.csv"))
write_csv_noheader(as.integer(flow$startlegis), file.path(out_dir, "startlegis.csv"))
write_csv_noheader(as.integer(flow$endlegis), file.path(out_dir, "endlegis.csv"))
write_csv_noheader(as.integer(flow$bill.session), file.path(out_dir, "bill_session.csv"))

metadata <- data.frame(N = N, J = J, T = T_periods, K = K)
write.csv(metadata, file.path(out_dir, "metadata.csv"), row.names = FALSE)

write_csv_noheader(flow$country_codes, file.path(out_dir, "country_codes.csv"))
write_csv_noheader(x_mu0, file.path(out_dir, "x_mu0.csv"))
write_csv_noheader(x_Sigma0, file.path(out_dir, "x_Sigma0_diag.csv"))

write_csv_noheader(alpha_start, file.path(out_dir, "alpha_start.csv"))
write_csv_noheader(beta_start, file.path(out_dir, "beta_start.csv"))

for (t in seq_len(T_periods)) {
  write_csv_noheader(
    x_start[, , t],
    file.path(out_dir, sprintf("x_start_t%d.csv", t))
  )
}

beta_mu <- rep(0, K + 1)
beta_sigma <- 25 * diag(K + 1)
omega <- 0.1 * diag(K)
write_csv_noheader(beta_mu, file.path(out_dir, "beta_mu.csv"))
write_csv_noheader(beta_sigma, file.path(out_dir, "beta_sigma.csv"))
write_csv_noheader(omega, file.path(out_dir, "omega.csv"))

write_csv_noheader(res$means$alpha, file.path(out_dir, "ref_alpha.csv"))
write_csv_noheader(res$means$beta, file.path(out_dir, "ref_beta.csv"))

for (t in seq_len(T_periods)) {
  write_csv_noheader(
    res$means$x[, , t],
    file.path(out_dir, sprintf("ref_x_t%d.csv", t))
  )
}

write_csv_noheader(res$runtime$loglik_trace, file.path(out_dir, "ref_loglik_trace.csv"))

cat(sprintf("Exported validation data to %s\n", out_dir))
