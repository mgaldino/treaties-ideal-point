suppressPackageStartupMessages({
  library(emIRT)
})

set.seed(123)

# ---- Carregar dados ----
flow <- readRDS("data/processed/environment_flow_matrix.rds")

N <- nrow(flow$rc)
J <- ncol(flow$rc)
T_periods <- flow$T
K <- 1L

cat(sprintf("Environment: N=%d, J=%d, T=%d\n", N, J, T_periods))

# ---- Construir starts (idênticos para todos) ----
rc_pca <- flow$rc
rc_pca[rc_pca == 0] <- NA
rc_pca[rc_pca == -1] <- 0
for (j in seq_len(J)) {
  col_mean <- mean(rc_pca[, j], na.rm = TRUE)
  if (is.nan(col_mean)) col_mean <- 0
  rc_pca[is.na(rc_pca[, j]), j] <- col_mean
}

pca <- prcomp(rc_pca, center = TRUE, scale. = TRUE)
pc1 <- as.numeric(scale(pca$x[, 1]))

x_start_2d <- matrix(rep(pc1, times = T_periods), nrow = N, ncol = T_periods)
alpha_start <- matrix(rnorm(J, 0, 0.1), ncol = 1)
beta_start  <- matrix(rnorm(J, 0, 0.5), ncol = 1)

# ---- Priors ----
x_mu0    <- matrix(0, nrow = N, ncol = 1)
x_sigma0 <- matrix(1, nrow = N, ncol = 1)

pos_idx <- which(flow$country_codes == "DNK")
neg_idx <- which(flow$country_codes == "SAU")
stopifnot(length(pos_idx) == 1, length(neg_idx) == 1)

x_mu0[pos_idx, 1]    <-  2.0
x_sigma0[pos_idx, 1] <-  0.01
x_mu0[neg_idx, 1]    <- -2.0
x_sigma0[neg_idx, 1] <-  0.01

beta_mu    <- matrix(c(0, 0), nrow = 2, ncol = 1)
beta_sigma <- matrix(c(25, 0, 0, 25), nrow = 2, ncol = 2)
omega2_val <- 0.1

# ---- emIRT::dynIRT() ----
cat("\n===== emIRT::dynIRT() =====\n")
t0_emirt <- proc.time()
res_emirt <- emIRT::dynIRT(
  .data = list(
    rc           = flow$rc,
    startlegis   = flow$startlegis,
    endlegis     = flow$endlegis,
    bill.session = flow$bill.session,
    T            = T_periods
  ),
  .starts = list(
    x     = x_start_2d,
    alpha = alpha_start,
    beta  = beta_start
  ),
  .priors = list(
    x.mu0      = x_mu0,
    x.sigma0   = x_sigma0,
    beta.mu    = beta_mu,
    beta.sigma = beta_sigma,
    omega2     = matrix(omega2_val, nrow = N, ncol = 1)
  ),
  .control = list(
    threads   = 1L,
    verbose   = TRUE,
    thresh    = 1e-6,
    maxit     = 800L,
    checkfreq = 50L
  )
)
time_emirt <- as.numeric((proc.time() - t0_emirt)["elapsed"])
cat(sprintf("emIRT: %d iters, conv=%d, %.1fs\n",
            res_emirt$runtime$iters, res_emirt$runtime$conv, time_emirt))

# ---- dynIRT_KD (normal) ----
source("scripts/R/kalman.R")
source("scripts/R/da_step.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD.R")

# Convert x_start to N x 1 x T
x_start_3d <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) {
  x_start_3d[, 1, t] <- x_start_2d[, t]
}

.starts_kd <- list(
  x     = x_start_3d,
  alpha = as.numeric(alpha_start),
  beta  = beta_start
)

.priors_kd <- list(
  x.mu0      = x_mu0,
  x.Sigma0   = x_sigma0,
  beta.mu    = as.numeric(beta_mu),
  beta.sigma = beta_sigma,
  omega      = matrix(omega2_val, nrow = K, ncol = K)
)

.control <- list(
  threads        = 1L,
  verbose        = TRUE,
  thresh         = 1e-6,
  maxit          = 800L,
  checkfreq      = 50L,
  estimate_omega = FALSE,
  ncores         = 8L
)

cat("\n===== dynIRT_KD(K=1) — normal =====\n")
t0_kd <- proc.time()
res_kd_normal <- dynIRT_KD(.data = list(
  rc           = flow$rc,
  startlegis   = matrix(as.integer(flow$startlegis), ncol = 1),
  endlegis     = matrix(as.integer(flow$endlegis), ncol = 1),
  bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
  T            = T_periods
), .starts = .starts_kd, .priors = .priors_kd, .control = .control, K = K)

time_kd_normal <- as.numeric((proc.time() - t0_kd)["elapsed"])
cat(sprintf("dynIRT_KD normal: %d iters, conv=%d, %.1fs\n",
            res_kd_normal$runtime$iters, res_kd_normal$runtime$conv, time_kd_normal))

# ---- dynIRT_KD P=0 (mean-field approx) ----
source("scripts/R/m_step_p0.R")
source("scripts/R/dynIRT_KD_p0.R")

cat("\n===== dynIRT_KD(K=1) — P=0 =====\n")
t0_p0 <- proc.time()
res_kd_p0 <- dynIRT_KD(.data = list(
  rc           = flow$rc,
  startlegis   = matrix(as.integer(flow$startlegis), ncol = 1),
  endlegis     = matrix(as.integer(flow$endlegis), ncol = 1),
  bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
  T            = T_periods
), .starts = .starts_kd, .priors = .priors_kd, .control = .control, K = K)

time_kd_p0 <- as.numeric((proc.time() - t0_p0)["elapsed"])
cat(sprintf("dynIRT_KD P0: %d iters, conv=%d, %.1fs\n",
            res_kd_p0$runtime$iters, res_kd_p0$runtime$conv, time_kd_p0))

# ---- Comparações ----
anchor_idx <- c(pos_idx, neg_idx)
non_anchor <- setdiff(seq_len(N), anchor_idx)

x_emirt <- res_emirt$means$x
alpha_emirt <- res_emirt$means$alpha
beta_emirt <- res_emirt$means$beta

x_kd_normal <- res_kd_normal$means$x[, 1, ]
alpha_kd_normal <- res_kd_normal$means$alpha
beta_kd_normal <- res_kd_normal$means$beta

x_kd_p0 <- res_kd_p0$means$x[, 1, ]
alpha_kd_p0 <- res_kd_p0$means$alpha
beta_kd_p0 <- res_kd_p0$means$beta

compute_corrs <- function(x_a, x_b, alpha_a, alpha_b, beta_a, beta_b, label) {
  cat(sprintf("\n===== %s =====\n", label))
  x_cor_by_period <- numeric(T_periods)
  for (t in seq_len(T_periods)) {
    x_cor_by_period[t] <- cor(x_a[non_anchor, t], x_b[non_anchor, t])
    cat(sprintf("  Period %d: r = %.6f\n", t, x_cor_by_period[t]))
  }
  mean_x_cor <- mean(x_cor_by_period)
  x_cor_global <- cor(as.vector(x_a[non_anchor, ]), as.vector(x_b[non_anchor, ]))
  cat(sprintf("  Mean r: %.6f\n", mean_x_cor))
  cat(sprintf("  Global r: %.6f\n", x_cor_global))

  r_alpha <- cor(alpha_a, alpha_b)
  r_beta  <- cor(as.vector(beta_a), as.vector(beta_b))
  cat(sprintf("  Alpha r: %.6f\n", r_alpha))
  cat(sprintf("  Beta  r: %.6f\n", r_beta))

  list(
    x_cor_by_period = x_cor_by_period,
    mean_x_cor = mean_x_cor,
    x_cor_global = x_cor_global,
    r_alpha = r_alpha,
    r_beta = r_beta
  )
}

cor_emirt_vs_normal <- compute_corrs(x_emirt, x_kd_normal, alpha_emirt, alpha_kd_normal,
                                     beta_emirt, beta_kd_normal, "emIRT vs dynIRT_KD (normal)")

cor_emirt_vs_p0 <- compute_corrs(x_emirt, x_kd_p0, alpha_emirt, alpha_kd_p0,
                                 beta_emirt, beta_kd_p0, "emIRT vs dynIRT_KD (P=0)")

cor_normal_vs_p0 <- compute_corrs(x_kd_normal, x_kd_p0, alpha_kd_normal, alpha_kd_p0,
                                  beta_kd_normal, beta_kd_p0, "dynIRT_KD normal vs P=0")

# ---- Log-likelihood ----
# Use compute_loglik from dynIRT_KD (normal)
source("scripts/R/dynIRT_KD.R")

x_emirt_arr <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) x_emirt_arr[, 1, t] <- x_emirt[, t]

ll_emirt <- compute_loglik(flow$rc, as.numeric(alpha_emirt),
                           as.matrix(beta_emirt), x_emirt_arr, flow$bill.session)
ll_kd_normal <- compute_loglik(flow$rc, alpha_kd_normal, beta_kd_normal,
                               res_kd_normal$means$x, flow$bill.session)
ll_kd_p0 <- compute_loglik(flow$rc, alpha_kd_p0, beta_kd_p0,
                           res_kd_p0$means$x, flow$bill.session)

cat(sprintf("\nLL emIRT:      %.4f\n", ll_emirt))
cat(sprintf("LL dynIRT normal: %.4f\n", ll_kd_normal))
cat(sprintf("LL dynIRT P0:    %.4f\n", ll_kd_p0))

# ---- Salvar ----
dir.create("outputs/v2_test_p0", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(
  x_emirt = x_emirt, alpha_emirt = alpha_emirt, beta_emirt = beta_emirt,
  x_kd_normal = x_kd_normal, alpha_kd_normal = alpha_kd_normal, beta_kd_normal = beta_kd_normal,
  x_kd_p0 = x_kd_p0, alpha_kd_p0 = alpha_kd_p0, beta_kd_p0 = beta_kd_p0,
  cor_emirt_vs_normal = cor_emirt_vs_normal,
  cor_emirt_vs_p0 = cor_emirt_vs_p0,
  cor_normal_vs_p0 = cor_normal_vs_p0,
  ll_emirt = ll_emirt, ll_kd_normal = ll_kd_normal, ll_kd_p0 = ll_kd_p0
), "outputs/v2_test_p0/results.rds")

cat("\nResults saved to outputs/v2_test_p0/results.rds\n")
