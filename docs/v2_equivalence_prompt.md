# Prompt para Agente 1 — V2: Equivalência K=1 entre dynIRT_KD e emIRT::dynIRT

## Objetivo

Validar que `dynIRT_KD(K=1)` produz resultados equivalentes a `emIRT::dynIRT()` no mesmo dataset real. Esta é a Phase V2 do plano de verificação (ver `docs/estimation_plan_2d.md`, Section 15, Phase V2).

**Critérios de sucesso:**
- Correlação dos ideal points: r > 0.99
- Correlação dos item parameters (alpha, beta): r > 0.99
- Número de iterações até convergência: similar (±20%)

## Dados

Usar o domínio **environment**:
```
data/processed/environment_flow_matrix.rds
```

Este é um objeto RDS contendo uma lista com:
- `rc`: N×J integer matrix (206×1849). Votes: +1, -1, 0.
- `startlegis`: integer vector length N. 0-indexed first period.
- `endlegis`: integer vector length N. 0-indexed last period.
- `bill.session`: integer vector length J. 0-indexed period per item.
- `T`: integer = 6 (number of periods).
- `country_codes`: character vector length N (ISO3 codes).
- `item_labels`, `period_labels`: character vectors.

## Passo 1: Rodar emIRT::dynIRT()

O script `scripts/R/03_estimate_ideal_points.R` já faz isso. Mas para a comparação V2, precisamos extrair exatamente os starts e priors usados, para reutilizá-los com `dynIRT_KD`.

Criar o script `scripts/R/v2_equivalence_test.R` com o seguinte:

```r
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

cat(sprintf("Environment: N=%d, J=%d, T=%d\n", N, J, T_periods))

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
    maxit     = 500L,
    checkfreq = 50L
  )
)
time_emirt <- as.numeric((proc.time() - t0_emirt)["elapsed"])
cat(sprintf("emIRT: %d iters, conv=%d, %.1fs\n",
            res_emirt$runtime$iters, res_emirt$runtime$conv, time_emirt))
```

## Passo 2: Rodar dynIRT_KD(K=1)

Adicionar ao mesmo script, LOGO ABAIXO do Passo 1:

```r
# ---- Rodar dynIRT_KD(K=1) ----
source("scripts/R/dynIRT_KD.R")

cat("\n===== dynIRT_KD(K=1) =====\n")

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
    estimate_omega = FALSE
  ),
  K = K
)
time_kd <- as.numeric((proc.time() - t0_kd)["elapsed"])
cat(sprintf("dynIRT_KD: %d iters, conv=%d, %.1fs\n",
            res_kd$runtime$iters, res_kd$runtime$conv, time_kd))
```

## Passo 3: Comparar resultados

Adicionar ao final do mesmo script:

```r
# ---- Comparação ----
cat("\n===== COMPARAÇÃO =====\n")

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

cat("Ideal point correlations by period (non-anchor):\n")
for (t in seq_len(T_periods)) {
  cat(sprintf("  Period %d: r = %.6f\n", t, x_cor_by_period[t]))
}
cat(sprintf("  Mean: r = %.6f (target > 0.99)\n", mean_x_cor))

# Correlação global (todos os pontos empilhados)
x_cor_global <- cor(as.vector(x_emirt[non_anchor, ]),
                    as.vector(x_kd[non_anchor, ]))
cat(sprintf("  Global: r = %.6f\n", x_cor_global))

# Extrair item parameters
# emIRT: alpha é J×1 matrix, beta é J×1 matrix
# dynIRT_KD: alpha é J-vector, beta é J×1 matrix
alpha_emirt <- as.numeric(res_emirt$means$alpha)
alpha_kd    <- as.numeric(res_kd$means$alpha)
beta_emirt  <- as.numeric(res_emirt$means$beta)
beta_kd     <- as.numeric(res_kd$means$beta)

alpha_cor <- cor(alpha_emirt, alpha_kd)
beta_cor  <- cor(beta_emirt, beta_kd)
cat(sprintf("\nAlpha correlation: r = %.6f (target > 0.99)\n", alpha_cor))
cat(sprintf("Beta correlation:  r = %.6f (target > 0.99)\n", beta_cor))

# Iterações
cat(sprintf("\nIterations: emIRT=%d, dynIRT_KD=%d (target: within ±20%%)\n",
            res_emirt$runtime$iters, res_kd$runtime$iters))
iter_ratio <- res_kd$runtime$iters / res_emirt$runtime$iters
cat(sprintf("Ratio: %.2f (target: 0.80 to 1.20)\n", iter_ratio))

# Runtime
cat(sprintf("Runtime: emIRT=%.1fs, dynIRT_KD=%.1fs\n", time_emirt, time_kd))

# ---- PASS/FAIL ----
cat("\n===== VERDICT =====\n")
pass <- TRUE

if (mean_x_cor < 0.99) {
  cat(sprintf("FAIL: Mean ideal point correlation = %.6f < 0.99\n", mean_x_cor))
  pass <- FALSE
} else {
  cat(sprintf("PASS: Mean ideal point correlation = %.6f >= 0.99\n", mean_x_cor))
}

if (alpha_cor < 0.99) {
  cat(sprintf("FAIL: Alpha correlation = %.6f < 0.99\n", alpha_cor))
  pass <- FALSE
} else {
  cat(sprintf("PASS: Alpha correlation = %.6f >= 0.99\n", alpha_cor))
}

if (beta_cor < 0.99) {
  cat(sprintf("FAIL: Beta correlation = %.6f < 0.99\n", beta_cor))
  pass <- FALSE
} else {
  cat(sprintf("PASS: Beta correlation = %.6f >= 0.99\n", beta_cor))
}

cat(sprintf("\nOverall: %s\n", ifelse(pass, "V2 PASSED", "V2 FAILED")))

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

cat("\nResults saved to outputs/v2_equivalence/v2_results.rds\n")
```

## Instruções de execução

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript scripts/R/v2_equivalence_test.R 2>&1 | tee logs/v2_equivalence.log
```

## O que fazer se V2 FALHAR

Se alguma correlação ficar abaixo de 0.99, investigar:

1. **Verificar o mapeamento de formatos.** O erro mais provável é incompatibilidade de dimensões entre emIRT e dynIRT_KD. Usar `str()` para inspecionar os inputs de ambos antes de rodar.

2. **Verificar bill.session indexação.** Ambos usam 0-indexed. Confirmar que `flow$bill.session` é o mesmo vetor passado a ambos.

3. **Verificar se startlegis/endlegis são tratados igualmente.** emIRT espera N×1 integer matrix; dynIRT_KD aceita N×1 matrix ou integer vector.

4. **Convergência.** Se um convergiu e outro não, aumentar `maxit` para 1000 e re-rodar. As correlações são calculadas nos estimates finais, que podem diferir se um parou antes.

5. **omega2 per-country vs shared.** emIRT usa `omega2` N×1 (um por país); dynIRT_KD usa `omega` K×K (compartilhado). No script, todos os países têm omega2=0.1, então são equivalentes. Se houvesse variação per-country, os resultados difeririam.

6. **Se r estiver entre 0.95-0.99**: isso pode ser aceitável — diferenças na implementação do Kalman filter (sequencial vs block, Joseph form vs information form) e no tratamento de missing data podem introduzir pequenas discrepâncias numéricas que acumulam ao longo de 500 iterações EM. Registrar o resultado e discutir.

## Arquivos que NÃO devem ser modificados

- `scripts/R/dynIRT_KD.R`
- `scripts/R/da_step.R`
- `scripts/R/kalman.R`
- `scripts/R/m_step.R`
- `scripts/R/03_estimate_ideal_points.R`
- Quaisquer testes em `tests/`

## Arquivo a criar

- `scripts/R/v2_equivalence_test.R` — o script acima (juntar Passos 1, 2 e 3)

## Saídas esperadas

- `logs/v2_equivalence.log` — log completo
- `outputs/v2_equivalence/v2_results.rds` — resultados serializados
- Print no console: PASS ou FAIL para cada métrica e veredicto geral
