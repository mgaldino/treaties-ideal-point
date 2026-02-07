suppressPackageStartupMessages({
  library(emIRT)
})

set.seed(123)

# ---- Carregar dados ----
flow <- readRDS("data/processed/environment_flow_matrix.rds")

N <- nrow(flow$rc)
J <- ncol(flow$rc)
T_periods <- flow$T  # = 6
K <- 1L

cat(sprintf("Environment: N=%d, J=%d, T=%d
", N, J, T_periods))

# ---- Construir starts (idênticos para ambos) ----
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

# Anchors: DNK (+2), SAU (-2)
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

# ---- Rodar emIRT::dynIRT() ----
cat("
===== emIRT::dynIRT() =====
")
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
    maxit     = 500L,
    checkfreq = 50L
  )
)
time_emirt <- as.numeric((proc.time() - t0_emirt)["elapsed"])
cat(sprintf("emIRT: %d iters, conv=%d, %.1fs
",
            res_emirt$runtime$iters, res_emirt$runtime$conv, time_emirt))

# ---- Rodar dynIRT_KD(K=1) ----
source("scripts/R/dynIRT_KD.R")

cat("
===== dynIRT_KD(K=1) =====
")

# MAPEAMENTO DE FORMATOS (CRÍTICO):
#
# emIRT                          →  dynIRT_KD(K=1)
# -----------------------------------------
# x_start: N×T matrix            →  N×1×T array
# alpha_start: J×1 matrix        →  numeric vector length J
# beta_start: J×1 matrix         →  J×1 matrix (OK, já é J×K com K=1)
# x.mu0: N×1 matrix              →  N×1 matrix (OK)
# x.sigma0: N×1 matrix           →  N×1 matrix (diagonal variances, OK)
# beta.mu: 2×1 matrix            →  numeric vector length 2
# beta.sigma: 2×2 matrix         →  2×2 matrix (OK)
# omega2: N×1 matrix (per-country) →  1×1 matrix (shared Omega)
#   NOTA: emIRT permite omega2 diferente por país;
#   dynIRT_KD usa Omega compartilhado. Como no script todos
#   os países têm omega2=0.1, são equivalentes.

# Converter x_start de N×T para N×1×T array
x_start_3d <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) {
  x_start_3d[, 1, t] <- x_start_2d[, t]
}

# Converter startlegis/endlegis para o formato esperado
# emIRT usa integer vector ou N×1 matrix; dynIRT_KD espera o mesmo
startlegis_vec <- as.integer(flow$startlegis)
endlegis_vec   <- as.integer(flow$endlegis)
billsession_vec <- as.integer(flow$bill.session)

t0_kd <- proc.time()

# Using 8 cores as requested to demonstrate speedup
ncores_target <- 8L

res_kd <- dynIRT_KD(
  .data = list(
    rc           = flow$rc,
    startlegis   = matrix(startlegis_vec, ncol = 1),
    endlegis     = matrix(endlegis_vec, ncol = 1),
    bill.session = matrix(billsession_vec, ncol = 1),
    T            = T_periods
  ),
  .starts = list(
    alpha = as.numeric(alpha_start),
    beta  = beta_start,        # J×1 matrix = J×K with K=1
    x     = x_start_3d         # N×1×T array
  ),
  .priors = list(
    x.mu0      = x_mu0,        # N×1 matrix
    x.Sigma0   = x_sigma0,     # N×1 matrix (diagonal variances)
    beta.mu    = as.numeric(beta_mu),     # numeric vector length 2
    beta.sigma = beta_sigma,              # 2×2 matrix
    omega      = matrix(omega2_val, K, K) # 1×1 matrix
  ),
  .control = list(
    threads        = 1L,
    verbose        = TRUE,
    thresh         = 1e-6,
    maxit          = 500L,
    checkfreq      = 50L,
    estimate_omega = FALSE,
    ncores         = ncores_target
  ),
  K = K
)
time_kd <- as.numeric((proc.time() - t0_kd)["elapsed"])
cat(sprintf("dynIRT_KD: %d iters, conv=%d, %.1fs
",
            res_kd$runtime$iters, res_kd$runtime$conv, time_kd))

# ---- Comparação ----
cat("
===== COMPARAÇÃO =====
")

# Extrair ideal points
# emIRT retorna x como N×T matrix
# dynIRT_KD retorna x como N×K×T array; para K=1, extrair x[,1,]
x_emirt <- res_emirt$means$x                      # N × T
x_kd    <- res_kd$means$x[, 1, ]                  # N × T (drop K dim)

# Correlação dos ideal points (excluindo anchors)
anchor_idx <- c(pos_idx, neg_idx)
non_anchor <- setdiff(seq_len(N), anchor_idx)

x_cor_by_period <- numeric(T_periods)
for (t in seq_len(T_periods)) {
  x_cor_by_period[t] <- cor(x_emirt[non_anchor, t], x_kd[non_anchor, t])
}
mean_x_cor <- mean(x_cor_by_period)

cat("Ideal point correlations by period (non-anchor):
")
for (t in seq_len(T_periods)) {
  cat(sprintf("  Period %d: r = %.6f
", t, x_cor_by_period[t]))
}
cat(sprintf("  Mean: r = %.6f (target > 0.99)
", mean_x_cor))

# Correlação global (todos os pontos empilhados)
x_cor_global <- cor(as.vector(x_emirt[non_anchor, ]),
                    as.vector(x_kd[non_anchor, ]))
cat(sprintf("  Global: r = %.6f
", x_cor_global))

# Extrair item parameters
# emIRT: alpha é J×1 matrix, beta é J×1 matrix
# dynIRT_KD: alpha é J-vector, beta é J×1 matrix
alpha_emirt <- as.numeric(res_emirt$means$alpha)
alpha_kd    <- as.numeric(res_kd$means$alpha)
beta_emirt  <- as.numeric(res_emirt$means$beta)
beta_kd     <- as.numeric(res_kd$means$beta)

alpha_cor <- cor(alpha_emirt, alpha_kd)
beta_cor  <- cor(beta_emirt, beta_kd)
cat(sprintf("
Alpha correlation: r = %.6f (target > 0.99)
", alpha_cor))
cat(sprintf("Beta correlation:  r = %.6f (target > 0.99)
", beta_cor))

# Iterações
cat(sprintf("
Iterations: emIRT=%d, dynIRT_KD=%d (target: within ±20%%)
",
            res_emirt$runtime$iters, res_kd$runtime$iters))
iter_ratio <- res_kd$runtime$iters / res_emirt$runtime$iters
cat(sprintf("Ratio: %.2f (target: 0.80 to 1.20)
", iter_ratio))

# Runtime
cat(sprintf("Runtime: emIRT=%.1fs, dynIRT_KD=%.1fs
", time_emirt, time_kd))
cat(sprintf("Speedup (vs emIRT single-thread): %.2fx
", time_emirt / time_kd))

# ---- PASS/FAIL ----
cat("
===== VERDICT =====
")
pass <- TRUE

if (mean_x_cor < 0.99) {
  cat(sprintf("FAIL: Mean ideal point correlation = %.6f < 0.99
", mean_x_cor))
  pass <- FALSE
} else {
  cat(sprintf("PASS: Mean ideal point correlation = %.6f >= 0.99
", mean_x_cor))
}

if (alpha_cor < 0.99) {
  cat(sprintf("FAIL: Alpha correlation = %.6f < 0.99
", alpha_cor))
  pass <- FALSE
} else {
  cat(sprintf("PASS: Alpha correlation = %.6f >= 0.99
", alpha_cor))
}

if (beta_cor < 0.99) {
  cat(sprintf("FAIL: Beta correlation = %.6f < 0.99
", beta_cor))
  pass <- FALSE
} else {
  cat(sprintf("PASS: Beta correlation = %.6f >= 0.99
", beta_cor))
}

cat(sprintf("
Overall: %s
", ifelse(pass, "V2 PASSED", "V2 FAILED")))

# ---- Salvar resultados ----
dir.create("outputs/v2_equivalence", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(
  x_emirt = x_emirt, x_kd = x_kd,
  alpha_emirt = alpha_emirt, alpha_kd = alpha_kd,
  beta_emirt = beta_emirt, beta_kd = beta_kd,
  x_cor_by_period = x_cor_by_period,
  mean_x_cor = mean_x_cor, x_cor_global = x_cor_global,
  alpha_cor = alpha_cor, beta_cor = beta_cor,
  iters_emirt = res_emirt$runtime$iters,
  iters_kd = res_kd$runtime$iters,
  time_emirt = time_emirt, time_kd = time_kd,
  pass = pass
), "outputs/v2_equivalence/v2_results.rds")

cat("
Results saved to outputs/v2_equivalence/v2_results.rds
")
