# Tarefa: Teste Block vs Sequencial Kalman para V2

## Objetivo

Verificar se a refatoração do Kalman filter de sequencial (item-por-item) para block (todos os itens do período de uma vez) introduziu alguma regressão numérica. Se os resultados forem idênticos (correlação > 1 - 1e-6), descartamos a hipótese de bug no block Kalman. Se divergirem, identificamos o problema.

## Contexto

- O Kalman filter em `scripts/R/kalman.R` foi recentemente refatorado para usar block updates (information form) em vez de sequential scalar updates.
- V2 original: `dynIRT_KD(K=1)` com block Kalman vs `emIRT::dynIRT()` deu r=0.965.
- Precisamos saber se essa correlação seria a mesma com o Kalman sequencial original.
- Dados: `data/processed/environment_flow_matrix.rds` (N=206, J=1849, T=6)

## Pastas de trabalho

- Criar arquivos em: `scripts/R/`, `outputs/v2_test_block_seq/`, `logs/`
- NÃO modificar: `scripts/R/kalman.R` (o atual com block), `scripts/R/dynIRT_KD.R`, `outputs/v2_equivalence/`

## Passos

### Passo 1: Criar Kalman sequencial

Criar `scripts/R/kalman_sequential.R` com uma versão da função `kalman_smoother_country` que processa os itens **um por um** (scalar updates), em vez do block update.

A função atual em `scripts/R/kalman.R` tem duas branches no forward pass (linhas 84-127):
- `length(obs_idx) == 1`: scalar update (já existente)
- `length(obs_idx) > 1`: block update via information form `P = (P^{-1} + H'H)^{-1}`

A versão sequencial deve substituir o branch `length(obs_idx) > 1` por um loop sobre cada item individualmente:

```r
# Em kalman_sequential.R, substituir o bloco "else" (linhas 100-126) por:
} else {
  # Sequential update: process each observation one at a time
  obs_items <- item_indices[obs_idx]
  for (k in seq_along(obs_items)) {
    j <- obs_items[k]
    beta_j <- beta[j, ]  # K-vector

    innovation <- y_star_i[obs_idx[k]] - alpha[j] - sum(beta_j * x_hat)
    S <- as.numeric(crossprod(beta_j, P_hat %*% beta_j)) + 1.0
    K_gain <- (P_hat %*% beta_j) / S

    x_hat <- x_hat + as.numeric(K_gain) * innovation

    # Joseph form for numerical stability
    IKb <- I_K - K_gain %*% t(beta_j)
    P_hat <- IKb %*% P_hat %*% t(IKb) + K_gain %*% t(K_gain)
    P_hat <- (P_hat + t(P_hat)) / 2
  }
}
```

IMPORTANTE: O backward pass (RTS smoother) e o lag-one cross-covariance NÃO mudam. Copiar integralmente de `kalman.R`.

A função deve ter o mesmo nome `kalman_smoother_country` para ser drop-in compatible.

### Passo 2: Criar script de teste

Criar `scripts/R/v2_test2_block_vs_seq.R` que:

1. Carrega os dados
2. Roda `dynIRT_KD(K=1)` com o Kalman **block** (atual) — 500 iterações
3. Roda `dynIRT_KD(K=1)` com o Kalman **sequencial** — 500 iterações
4. Compara os resultados

Para a execução com Kalman sequencial, a forma mais limpa é:

```r
# Execução 1: Block (atual)
source("scripts/R/kalman.R")  # Carrega kalman_smoother_country (block)
source("scripts/R/da_step.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD.R")

set.seed(123)
# ... construir starts, priors, etc. (copiar de v2_equivalence_test.R linhas 18-50)
res_block <- dynIRT_KD(.data, .starts, .priors, .control, K = 1L)

# Execução 2: Sequencial
# Sobrescrever a função kalman_smoother_country com a versão sequencial
source("scripts/R/kalman_sequential.R")  # Sobrescreve kalman_smoother_country

set.seed(123)
# ... mesmos starts, priors (IMPORTANTE: re-gerar com mesmo seed)
res_seq <- dynIRT_KD(.data, .starts, .priors, .control, K = 1L)
```

ATENÇÃO ao `set.seed(123)`: é crucial que ambas as execuções partam dos mesmos starts. Como os starts dependem de `rnorm()` (para alpha_start e beta_start), o seed deve ser setado antes de gerar os starts para cada execução. A maneira mais segura é gerar os starts UMA VEZ e reutilizar:

```r
set.seed(123)
# Gerar starts (copiar de v2_equivalence_test.R:18-32)
# ...

# Execução 1 (block)
source("scripts/R/kalman.R")
source("scripts/R/dynIRT_KD.R")  # Re-source para garantir que usa o kalman certo
res_block <- dynIRT_KD(.data, .starts, .priors, .control, K = 1L)

# Execução 2 (sequencial) — mesmos starts
source("scripts/R/kalman_sequential.R")  # Sobrescreve kalman_smoother_country
source("scripts/R/dynIRT_KD.R")          # Re-source dynIRT_KD para pegar o novo kalman
res_seq <- dynIRT_KD(.data, .starts, .priors, .control, K = 1L)
```

NOTA: `dynIRT_KD.R` faz source de `kalman.R` no início (linhas 13-23), MAS apenas se `kalman_smoother_country` não existe. Portanto, se você fizer `source("scripts/R/kalman_sequential.R")` ANTES de `source("scripts/R/dynIRT_KD.R")`, a versão sequencial já estará definida e o dynIRT_KD NÃO a sobrescreverá. Verifique isso no código (linhas 13-14: `if (!exists("kalman_smoother_country", mode = "function"))`).

Parâmetros:
```r
.control = list(
  threads        = 1L,
  verbose        = TRUE,
  thresh         = 1e-6,
  maxit          = 500L,
  checkfreq      = 50L,
  estimate_omega = FALSE,
  ncores         = 1L          # IMPORTANTE: usar 1 core para evitar diferenças de
                               # ordem de execução entre mclapply e lapply.
                               # A comparação block vs seq deve ser determinística.
)
```

IMPORTANTE: Usar `ncores = 1L` para ambas as execuções. Com `mclapply`, a ordem de acumulação de floating-point pode diferir, introduzindo ruído que mascara a comparação.

### Passo 3: Comparar resultados

```r
cat("\n===== COMPARAÇÃO BLOCK vs SEQUENCIAL =====\n")

# Ideal points
x_block <- res_block$means$x[, 1, ]  # N x T
x_seq   <- res_seq$means$x[, 1, ]

# Excluir anchors
anchor_idx <- c(pos_idx, neg_idx)
non_anchor <- setdiff(seq_len(N), anchor_idx)

# Correlação por período
for (t in seq_len(T_periods)) {
  r <- cor(x_block[non_anchor, t], x_seq[non_anchor, t])
  cat(sprintf("  Period %d: r = %.10f\n", t, r))
}

# Correlação global
r_global <- cor(as.vector(x_block[non_anchor, ]), as.vector(x_seq[non_anchor, ]))
cat(sprintf("  Global: r = %.10f\n", r_global))

# Diferença absoluta máxima (mais informativa que correlação para equivalência numérica)
max_diff_x <- max(abs(x_block - x_seq))
cat(sprintf("  Max |x_block - x_seq|: %.2e\n", max_diff_x))

# Item parameters
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
source("scripts/R/dynIRT_KD.R")  # Para compute_loglik
ll_block <- compute_loglik(rc, alpha_block, beta_block,
                           res_block$means$x, bill.session)
ll_seq   <- compute_loglik(rc, alpha_seq, beta_seq,
                           res_seq$means$x, bill.session)
cat(sprintf("\nLL block: %.4f\nLL seq:   %.4f\nDiff:     %.4f\n", ll_block, ll_seq, ll_block - ll_seq))

# Convergência
cat(sprintf("\nIterations: block=%d, seq=%d\n",
            res_block$runtime$iters, res_seq$runtime$iters))
cat(sprintf("Runtime: block=%.1fs, seq=%.1fs\n",
            res_block$runtime$seconds, res_seq$runtime$seconds))
```

### Passo 4: Veredito

```r
cat("\n===== VEREDITO =====\n")
# Critério: correlação > 1 - 1e-6 E max_diff < 1e-4
# (tolerância generosa para 500 iterações sem convergência formal)
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
```

### Passo 5: Salvar

```r
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
```

## Validações

Antes de terminar, verificar:

- [ ] `scripts/R/kalman_sequential.R` existe e define `kalman_smoother_country`
- [ ] Ambas as execuções completaram (500 iterações cada ou convergência)
- [ ] Os starts são idênticos (verificar que as primeiras iterações produzem a mesma LL)
- [ ] `ncores = 1L` em ambas (determinismo)
- [ ] A tabela de comparação está no log
- [ ] O veredito PASS/FAIL está no log
- [ ] `outputs/v2_test_block_seq/results.rds` existe

## Saídas esperadas

1. `scripts/R/kalman_sequential.R` — Kalman sequencial (drop-in replacement)
2. `scripts/R/v2_test2_block_vs_seq.R` — Script do teste
3. `outputs/v2_test_block_seq/results.rds` — Resultados serializados
4. `logs/v2_test2_block_vs_seq.log` — Log completo

## Resultado esperado

O mais provável é **PASS** (block = sequencial), dado que:
- 142 unit tests passam com o block Kalman
- A LL do dynIRT_KD é monótona crescente

Se PASS: descartamos a hipótese de regressão no block Kalman.
Se FAIL: o block Kalman tem um bug e deve ser corrigido antes de prosseguir.

Bônus: reportar a diferença de runtime entre block e sequencial. O block deveria ser mais rápido (menos chamadas a solve/crossprod por período).

## Se falhar

- Se `source("scripts/R/kalman_sequential.R")` não sobrescrever a função: verificar que o nome da função é exatamente `kalman_smoother_country` (mesmo nome que em `kalman.R`).
- Se o dynIRT_KD na segunda execução usar o block: isso significa que `dynIRT_KD.R` fez re-source de `kalman.R`. Verificar a guarda `if (!exists(...))` nas linhas 13-14 do `dynIRT_KD.R`. Se necessário, fazer `source("scripts/R/kalman_sequential.R")` DEPOIS de `source("scripts/R/dynIRT_KD.R")` para sobrescrever.
- Se os starts diferirem entre execuções: gerar os starts UMA VEZ e passar o mesmo objeto `.starts` para ambas as chamadas.
- Se LL divergir na iteração 1: os starts NÃO são idênticos. Verificar seed e ordem de geração.
- NÃO ficar em loop. Se falhar 2x, reportar o erro e parar.
