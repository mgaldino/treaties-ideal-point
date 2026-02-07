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

# ---- Construir starts (idênticos) ----
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

# ---- Construir priors ----
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

.data <- list(
  rc           = flow$rc,
  startlegis   = flow$startlegis,
  endlegis     = flow$endlegis,
  bill.session = flow$bill.session,
  T            = T_periods
)

.starts <- list(
  x     = x_start_2d,
  alpha = alpha_start,
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
  maxit          = 500L,
  checkfreq      = 50L,
  estimate_omega = FALSE,
  ncores         = 1L
)

# ---- Execução 1: Block (atual) ----
source("scripts/R/kalman.R")
source("scripts/R/da_step.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD.R")

# Converter x_start de N×T para N×1×T array
x_start_3d <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) {
  x_start_3d[, 1, t] <- x_start_2d[, t]
}

.starts_block <- list(
  x     = x_start_3d,
  alpha = as.numeric(alpha_start),
  beta  = beta_start
)

cat("\n===== dynIRT_KD(K=1) — BLOCK =====\n")
t0_block <- proc.time()
res_block <- dynIRT_KD(.data, .starts_block, .priors_kd, .control, K = 1L)
time_block <- as.numeric((proc.time() - t0_block)["elapsed"])

# ---- Execução 2: Sequencial ----
source("scripts/R/kalman_sequential.R")
source("scripts/R/dynIRT_KD.R")

cat("\n===== dynIRT_KD(K=1) — SEQ =====\n")
t0_seq <- proc.time()
res_seq <- dynIRT_KD(.data, .starts_block, .priors_kd, .control, K = 1L)
time_seq <- as.numeric((proc.time() - t0_seq)["elapsed"])

# ---- Comparação ----
cat("\n===== COMPARAÇÃO BLOCK vs SEQUENCIAL =====\n")

x_block <- res_block$means$x[, 1, ]
x_seq   <- res_seq$means$x[, 1, ]

anchor_idx <- c(pos_idx, neg_idx)
non_anchor <- setdiff(seq_len(N), anchor_idx)

for (t in seq_len(T_periods)) {
  r <- cor(x_block[non_anchor, t], x_seq[non_anchor, t])
  cat(sprintf("  Period %d: r = %.10f\n", t, r))
}

r_global <- cor(as.vector(x_block[non_anchor, ]), as.vector(x_seq[non_anchor, ]))
cat(sprintf("  Global: r = %.10f\n", r_global))

max_diff_x <- max(abs(x_block - x_seq))
cat(sprintf("  Max |x_block - x_seq|: %.2e\n", max_diff_x))

alpha_block <- res_block$means$alpha
alpha_seq   <- res_seq$means$alpha
beta_block  <- res_block$means$beta
beta_seq    <- res_seq$means$beta

r_alpha <- cor(alpha_block, alpha_seq)
r_beta  <- cor(as.vector(beta_block), as.vector(beta_seq))
max_diff_alpha <- max(abs(alpha_block - alpha_seq))
max_diff_beta  <- max(abs(beta_block - beta_seq))

cat(sprintf("\nAlpha: r = %.10f, max |diff| = %.2e\n", r_alpha, max_diff_alpha))
cat(sprintf("Beta:  r = %.10f, max |diff| = %.2e\n", r_beta, max_diff_beta))

# LL
source("scripts/R/dynIRT_KD.R")
ll_block <- compute_loglik(flow$rc, alpha_block, beta_block,
                           res_block$means$x, flow$bill.session)
ll_seq   <- compute_loglik(flow$rc, alpha_seq, beta_seq,
                           res_seq$means$x, flow$bill.session)
cat(sprintf("\nLL block: %.4f\nLL seq:   %.4f\nDiff:     %.4f\n",
            ll_block, ll_seq, ll_block - ll_seq))

cat(sprintf("\nIterations: block=%d, seq=%d\n",
            res_block$runtime$iters, res_seq$runtime$iters))
cat(sprintf("Runtime: block=%.1fs, seq=%.1fs\n",
            res_block$runtime$seconds, res_seq$runtime$seconds))

# ---- Veredito ----
cat("\n===== VEREDITO =====\n")
pass_x     <- r_global > (1 - 1e-6) && max_diff_x < 1e-4
pass_alpha <- r_alpha > (1 - 1e-6) && max_diff_alpha < 1e-4
pass_beta  <- r_beta > (1 - 1e-6) && max_diff_beta < 1e-4

if (pass_x && pass_alpha && pass_beta) {
  cat("PASS: Block e Sequencial são numericamente equivalentes.\n")
  cat("Conclusão: Refatoração do block Kalman NÃO introduziu regressão.\n")
} else {
  cat("FAIL: Block e Sequencial divergem.\n")
  if (!pass_x)     cat(sprintf("  x: r=%.10f, max_diff=%.2e\n", r_global, max_diff_x))
  if (!pass_alpha)  cat(sprintf("  alpha: r=%.10f, max_diff=%.2e\n", r_alpha, max_diff_alpha))
  if (!pass_beta)   cat(sprintf("  beta: r=%.10f, max_diff=%.2e\n", r_beta, max_diff_beta))
  cat("Conclusão: Possível bug na refatoração do block Kalman. Investigar.\n")
}

# ---- Salvar ----
dir.create("outputs/v2_test_block_seq", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(
  x_block = x_block, x_seq = x_seq,
  alpha_block = alpha_block, alpha_seq = alpha_seq,
  beta_block = beta_block, beta_seq = beta_seq,
  r_global = r_global, max_diff_x = max_diff_x,
  r_alpha = r_alpha, r_beta = r_beta,
  max_diff_alpha = max_diff_alpha, max_diff_beta = max_diff_beta,
  ll_block = ll_block, ll_seq = ll_seq,
  iters_block = res_block$runtime$iters,
  iters_seq = res_seq$runtime$iters,
  time_block = res_block$runtime$seconds,
  time_seq = res_seq$runtime$seconds
), "outputs/v2_test_block_seq/results.rds")

cat("\nResults saved to outputs/v2_test_block_seq/results.rds\n")
