# Plano de Otimização: dynIRT_KD — Paralelismo (A), Vetorização do DA (B), LL sob demanda (D)

**Data**: 2026-02-06
**Status**: Pronto para implementação
**Arquivos a modificar**: `scripts/R/dynIRT_KD.R`, `scripts/R/da_step.R`
**Testes existentes**: `tests/test_dynIRT_KD.R` (23 testes), `tests/test_da_step.R` (43 testes)
**Regra de ouro**: Todos os 142 testes existentes devem continuar passando após cada mudança.

---

## Contexto

O `dynIRT_KD()` é um EM para IRT dinâmico K-dimensional. Com N=100, J=200, T=6, K=2, roda em ~64s (500 iterações, pure R). Para dados reais (N~180, J~2000), estimamos ~10 min. As três otimizações abaixo combinadas devem reduzir para ~1.5-2 min.

---

## Otimização A: Paralelizar Kalman filter via `parallel::mclapply`

### O que mudar

Arquivo: `scripts/R/dynIRT_KD.R`, dentro de `dynIRT_KD()`, linhas 208-278.

### Situação atual

```r
# Linha 212
for (i in seq_len(N)) {
  # ... chama kalman_smoother_country() para cada país
  # ... escreve resultados em x_smooth[i,,], P_smooth_all[[i]], P_lag_all[[i]]
}
```

O loop é sequencial. Cada iteração do Kalman é independente das demais (não há dependência entre países), mas o resultado é escrito diretamente no array `x_smooth` (efeito colateral).

### Como implementar

1. **Adicionar parâmetro `ncores`** ao `.control` (default = 1L, sem paralelismo).

2. **Extrair o corpo do loop para uma função auxiliar** (interna, não exportada):

```r
.kalman_one_country <- function(i, da_y_star, rc, alpha, beta, bill.session,
                                 x_mu0, x_Sigma0, Omega, startlegis, endlegis,
                                 T_total, K) {
  si <- startlegis[i]
  ei <- endlegis[i]

  obs_items <- which(rc[i, ] != 0L)

  if (length(obs_items) == 0) {
    # Sem observações: propagar prior via random walk
    x_smooth_i <- matrix(NA_real_, nrow = K, ncol = T_total)
    P_smooth_i <- vector("list", T_total)
    P_cov <- x_Sigma0[[i]]
    for (tt in seq_len(T_total)) {
      if (tt == 1L) {
        x_smooth_i[, tt] <- x_mu0[i, ]
        P_smooth_i[[tt]] <- P_cov
      } else {
        x_smooth_i[, tt] <- x_smooth_i[, tt - 1L]
        P_cov <- P_cov + Omega
        P_smooth_i[[tt]] <- P_cov
      }
    }
    return(list(x_smooth_i = x_smooth_i, P_smooth_i = P_smooth_i,
                P_lag_i = vector("list", max(T_total - 1L, 0L))))
  }

  y_star_i <- da_y_star[i, obs_items]

  ks <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = obs_items,
    alpha        = alpha,
    beta         = beta,
    bill.session = bill.session,
    mu0          = x_mu0[i, ],
    Sigma0       = x_Sigma0[[i]],
    Omega        = Omega,
    start_t      = si,
    end_t        = ei,
    T_total      = T_total
  )

  # Montar x_smooth_i como K x T_total
  T_active_i <- ei - si + 1L
  x_smooth_i <- matrix(NA_real_, nrow = K, ncol = T_total)
  P_smooth_i <- vector("list", T_total)

  for (s in seq_len(T_active_i)) {
    t_global <- si + s
    x_smooth_i[, t_global] <- ks$x_smooth[, s]
    P_smooth_i[[t_global]] <- ks$P_smooth[[s]]
  }
  # Preencher períodos inativos
  for (tt in seq_len(T_total)) {
    if (is.null(P_smooth_i[[tt]])) {
      P_smooth_i[[tt]] <- diag(K) * 100
    }
    if (any(is.na(x_smooth_i[, tt]))) {
      # Manter valor anterior (será ignorado pelo resto do EM)
      x_smooth_i[, tt] <- if (tt > 1) x_smooth_i[, tt - 1L] else x_mu0[i, ]
    }
  }

  list(x_smooth_i = x_smooth_i, P_smooth_i = P_smooth_i, P_lag_i = ks$P_lag)
}
```

3. **Substituir o loop por despacho condicional**:

```r
ncores <- .control$ncores %||% 1L

if (ncores > 1L) {
  ks_results <- parallel::mclapply(seq_len(N), function(i) {
    .kalman_one_country(i, da$y_star, rc, alpha, beta, bill.session,
                        x_mu0, x_Sigma0, Omega, startlegis, endlegis,
                        T_total, K)
  }, mc.cores = ncores)
} else {
  ks_results <- lapply(seq_len(N), function(i) {
    .kalman_one_country(i, da$y_star, rc, alpha, beta, bill.session,
                        x_mu0, x_Sigma0, Omega, startlegis, endlegis,
                        T_total, K)
  })
}

# Coletar resultados de volta para x_smooth, P_smooth_all, P_lag_all
for (i in seq_len(N)) {
  res_i <- ks_results[[i]]
  for (tt in seq_len(T_total)) {
    x_smooth[i, , tt] <- res_i$x_smooth_i[, tt]
  }
  P_smooth_all[[i]] <- res_i$P_smooth_i
  P_lag_all[[i]]    <- res_i$P_lag_i
}
```

### Pontos de atenção

- `parallel::mclapply` **não funciona no Windows** (usa fork). Se o usuário estiver no Windows, fazer fallback para `lapply` (ou avisar). O sistema atual é macOS (Darwin), então `mclapply` funciona.
- Não carregar `parallel` com `library()`. Usar `parallel::mclapply()` com namespace explícito.
- O `.kalman_one_country` não deve modificar variáveis externas. Tudo que precisa é passado como argumento. A função `kalman_smoother_country` de `kalman.R` já é pura (sem efeitos colaterais).
- Para `ncores = 1L`, usar `lapply` (evita overhead de fork).

### Teste

- Todos os 23 testes de `test_dynIRT_KD.R` devem passar com `ncores = 1L` (comportamento idêntico).
- Adicionar 1 teste novo: rodar a simulação V1 com `ncores = 2L` e verificar que os resultados são **idênticos** aos de `ncores = 1L` (mesmo seed, mesmos starts). A igualdade deve ser exata (`all.equal(..., tolerance = 1e-12)`) porque não há aleatoriedade no EM — é determinístico dado os starts.

---

## Otimização B: Vetorizar `da_step` (eliminar loop sobre itens)

### O que mudar

Arquivo: `scripts/R/da_step.R`, função `da_step()`, linhas 106-119.

### Situação atual

```r
# Linha 107 — loop desnecessário
for (jj in seq_along(j_idx)) {
  j_col <- j_idx[jj]
  nonmiss <- which(rc_sub[, jj] != 0L)
  if (length(nonmiss) == 0) next
  moments <- truncnorm_moments(mu = mu_mat[nonmiss, jj], y = rc_sub[nonmiss, jj])
  y_star[nonmiss, j_col]     <- moments$y_star
  y_star_var[nonmiss, j_col] <- moments$y_star_var
}
```

O problema: chama `truncnorm_moments` uma vez por item. Mas `truncnorm_moments` já é vetorizado — aceita vetores de qualquer comprimento. Podemos processar **todas as entradas não-missing do período de uma vez**.

### Como implementar

Substituir o bloco `for (jj ...)` (linhas 106-119) por:

```r
    # Máscara de não-missing para todos os itens deste período: N x |j_idx|
    nonmiss_mask <- rc_sub != 0L

    if (any(nonmiss_mask)) {
      # Extrair vetores de mu e y para todas as entradas não-missing
      mu_vec <- mu_mat[nonmiss_mask]
      y_vec  <- rc_sub[nonmiss_mask]

      # Uma única chamada vetorizada
      moments <- truncnorm_moments(mu = mu_vec, y = y_vec)

      # Escrever de volta nas posições corretas
      # Mapear colunas locais (1:|j_idx|) para colunas globais (j_idx)
      # nonmiss_mask é N x |j_idx|; precisamos dos índices globais em y_star
      # Construir a sub-matriz de destino e atribuir
      y_star_sub     <- matrix(NA_real_, nrow = N, ncol = length(j_idx))
      y_star_var_sub <- matrix(NA_real_, nrow = N, ncol = length(j_idx))

      y_star_sub[nonmiss_mask]     <- moments$y_star
      y_star_var_sub[nonmiss_mask] <- moments$y_star_var

      y_star[, j_idx]     <- y_star_sub
      y_star_var[, j_idx] <- y_star_var_sub
    }
```

### Versão mais limpa (preferida)

A versão acima pode ser simplificada. Como `y_star` e `y_star_var` já são inicializados com `NA`, e `nonmiss_mask` seleciona exatamente as posições não-missing:

```r
    nonmiss_mask <- rc_sub != 0L
    if (any(nonmiss_mask)) {
      moments <- truncnorm_moments(mu = mu_mat[nonmiss_mask],
                                    y  = rc_sub[nonmiss_mask])
      # Escrever diretamente: rc_sub e mu_mat têm colunas na ordem de j_idx
      # y_star[, j_idx] já tem as colunas certas
      tmp_ystar     <- matrix(NA_real_, nrow = N, ncol = length(j_idx))
      tmp_ystar_var <- matrix(NA_real_, nrow = N, ncol = length(j_idx))
      tmp_ystar[nonmiss_mask]     <- moments$y_star
      tmp_ystar_var[nonmiss_mask] <- moments$y_star_var
      y_star[, j_idx]     <- tmp_ystar
      y_star_var[, j_idx] <- tmp_ystar_var
    }
```

### Ponto de atenção sobre a ordem de indexação

Quando fazemos `mu_mat[nonmiss_mask]`, R lineariza a matriz **por colunas** (column-major). Ou seja, percorre (linha 1 col 1), (linha 2 col 1), ..., (linha N col 1), (linha 1 col 2), etc. O mesmo acontece com `rc_sub[nonmiss_mask]`. Como ambas usam a mesma máscara booleana, a correspondência é preservada. E ao escrever `tmp_ystar[nonmiss_mask] <- moments$y_star`, a mesma ordem column-major é usada. **Portanto a correspondência está correta.**

### Teste

- Os 43 testes de `test_da_step.R` devem passar sem alteração.
- Para validação extra, antes de apagar o loop antigo: em uma sessão interativa, rodar `da_step` com ambas implementações no mesmo input e verificar `all.equal()` dos resultados.

---

## Otimização D: Computar log-likelihood apenas quando necessário

### O que mudar

Arquivo: `scripts/R/dynIRT_KD.R`, dentro do EM loop, linhas 307-353.

### Situação atual

```r
# Linha 309 — chamado TODA iteração
ll <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)
loglik_trace[m] <- ll
```

O `compute_loglik` percorre todas as N×J entradas não-missing. Para N=180, J=2000, isso é ~250K avaliações de `pnorm()` **toda iteração**. O valor de `ll` é usado para:
1. Print a cada `checkfreq` iterações (linha 312)
2. Aitken stopping (linha 329, precisa de 3 valores consecutivos)

### Como implementar

Substituir a lógica de convergência (linhas 307-353) por:

```r
    # ==== Convergence check ====
    delta <- param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

    # Determinar se precisamos da LL nesta iteração
    need_ll <- FALSE
    if (verbose && (m == 1L || m %% checkfreq == 0L)) need_ll <- TRUE
    if (!is.null(thresh_aitken) && m >= 1L) need_ll <- TRUE
    # Sempre computar na última iteração
    if (delta < thresh) need_ll <- TRUE

    if (need_ll) {
      ll <- compute_loglik(rc, alpha, beta, x_smooth, bill.session)
      loglik_trace[m] <- ll
    } else {
      loglik_trace[m] <- NA_real_
    }

    if (verbose && (m == 1L || m %% checkfreq == 0L)) {
      message(sprintf(
        "Iter %4d | loglik = %.4f | delta = %.2e | Omega[1,1] = %.4f",
        m, ll, delta, Omega[1, 1]
      ))
    }

    if (delta < thresh) {
      conv <- 1L
      if (verbose) {
        message(sprintf("Converged at iteration %d (delta = %.2e)", m, delta))
      }
      loglik_trace <- loglik_trace[seq_len(m)]
      break
    }

    # Aitken early stopping (não muda — já tem guard is.finite)
    if (!is.null(thresh_aitken) && m >= 3L) {
      ll_m   <- loglik_trace[m]
      ll_m1  <- loglik_trace[m - 1L]
      ll_m2  <- loglik_trace[m - 2L]
      # ... (idêntico ao código atual)
    }
```

### Caso sem Aitken e checkfreq = 50

Se `thresh_aitken = NULL` e `checkfreq = 50`, a LL será computada apenas nas iterações 1, 50, 100, 150, ... e na iteração de convergência. Para 500 iterações, são ~10 chamadas ao invés de 500. Economia: ~98% das chamadas a `compute_loglik`.

### Caso com Aitken

Se `thresh_aitken` está ativo, a LL é necessária **toda iteração** (porque o Aitken precisa de 3 valores consecutivos). Nesse caso, a otimização D não ajuda. Isso está correto — quando se usa Aitken, a parada precoce compensa o custo da LL. Mas documente isso: **Aitken e otimização D são mutuamente exclusivos em benefício**.

### Atenção: `loglik_trace` com NAs

O `loglik_trace` terá `NA` nas iterações em que a LL não foi computada. Isso afeta código downstream que lê o trace. Solução: na saída final, filtrar NAs:

```r
loglik_trace = loglik_trace[!is.na(loglik_trace)]
```

Ou manter o vetor completo e documentar que NAs = não computado. **A primeira opção é mais segura** para compatibilidade.

### Teste

- Todos os 23 testes de `test_dynIRT_KD.R` devem passar. Alguns testes verificam que o `loglik_trace` é não-decrescente — esses continuarão funcionando se filtrarmos NAs.
- Verificar especificamente o teste de monotonicidade do LL: ele faz `diff(loglik_trace)`. Se houver NAs no meio, `diff` produz NAs. Então o filtro é obrigatório antes de retornar.

---

## Ordem de Implementação

```
B (vetorizar da_step)  →  rodar testes  →  A (paralelizar Kalman)  →  rodar testes  →  D (LL sob demanda)  →  rodar testes
```

**Por que esta ordem**:
- B é a mudança mais isolada (só toca `da_step.R`), fácil de validar.
- A é a mais impactante, mas requer refatorar o loop principal.
- D é simples mas toca a lógica de convergência, que é sensível. Fazer por último.

---

## Validação Final

Após implementar A + B + D, rodar a validação V1 (simulação) para confirmar que os resultados são idênticos:

```r
source("scripts/R/v1_simulation_study.R")
res <- run_single_replication(seed = 42, N = 100, J = 200, T_sim = 6, K = 2,
                               maxit = 500, thresh = 1e-6)
```

Os resultados numéricos devem ser **exatamente iguais** aos da versão original (com `ncores = 1`). O tempo deve ser menor.

Para testar com paralelismo:
```r
# Modificar .control dentro de run_single_replication para incluir ncores = 4
```

---

## Atualização de Execução (Sandbox)

**Data**: 2026-02-06  
**Script**: `scripts/R/optimization_ABD/run_v1_validation_ABD.R`  
**Log**: `logs/v1_validation_ABD_20260206T013000Z.log`  
**Saídas**: `outputs/optimization_ABD/v1_validation_summary.txt`, `outputs/optimization_ABD/v1_validation_summary.rds`

Resumo:
- `parallel::detectCores()` retornou 1, então `ncores_half=1`.
- Equivalência numérica confirmada: `x=TRUE`, `alpha=TRUE`, `beta=TRUE` (tol=1e-12).
- Runtime: seq 65.79s (iters=500, conv=0), par 66.02s (iters=500, conv=0), ambos com `ncores_used=1`.
- Recovery (seq e par): mean x corr = 0.9396 (>= 0.93).

Observação:
- Sem mais cores disponíveis no sandbox, não foi possível medir speedup real via paralelismo. Rodar este mesmo script em ambiente com múltiplos cores para avaliar ganho.

## Comandos para rodar os testes

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript -e 'testthat::test_file("tests/test_da_step.R")'
Rscript -e 'testthat::test_file("tests/test_kalman.R")'
Rscript -e 'testthat::test_file("tests/test_m_step.R")'
Rscript -e 'testthat::test_file("tests/test_dynIRT_KD.R")'
```

Todos devem reportar 0 falhas.

---

## Q&A — Dúvidas que o agente implementador pode ter

### Q1: Preciso instalar algum pacote novo?
**R**: Não. `parallel` faz parte do base R (vem instalado). Não adicione dependências.

### Q2: A função `.kalman_one_country` deve ficar onde?
**R**: Dentro de `scripts/R/dynIRT_KD.R`, **fora** da função `dynIRT_KD()` (como função auxiliar no nível do módulo, similar a `compute_loglik` e `param_change`). O ponto `.` no início do nome é convenção R para "interna/não exportada".

### Q3: O `mclapply` faz cópia dos dados para cada worker?
**R**: Sim, via fork (copy-on-write no macOS/Linux). Os arrays `rc`, `alpha`, `beta`, `da$y_star` etc. são somente leitura dentro de `.kalman_one_country`, então o sistema operacional não copia de verdade (CoW). Não é preciso se preocupar com uso de memória adicional para N=180.

### Q4: E se algum teste acessa `loglik_trace` e espera um vetor sem NAs?
**R**: Garanta que o `loglik_trace` retornado em `runtime$loglik_trace` **nunca contenha NAs**. Filtre antes de retornar: `loglik_trace <- loglik_trace[!is.na(loglik_trace)]`. Assim a interface pública não muda.

### Q5: Os testes de `da_step` usam `y_star_var` no resultado?
**R**: Sim, alguns testes verificam `y_star_var`. A vetorização deve produzir ambos (`y_star` e `y_star_var`). A função `truncnorm_moments` já retorna ambos. Certifique-se de escrever `y_star_var` também na versão vetorizada.

### Q6: O `test_dynIRT_KD.R` verifica convergência formal?
**R**: Alguns testes verificam `result$runtime$conv`, `result$runtime$loglik_trace`, e que a LL é não-decrescente. A lógica de `conv` e `delta` não muda. A LL trace muda (pode ser mais curta se filtrada), mas como os testes rodam com configurações pequenas, a LL é tipicamente computada em todas iterações relevantes.

### Q7: Devo mudar a assinatura da função `dynIRT_KD()`?
**R**: Não. A assinatura pública permanece idêntica. O novo parâmetro `ncores` é lido de `.control$ncores` com default `1L`, assim como `thresh_aitken` já é lido de `.control`. Não é preciso adicionar argumento formal.

### Q8: Preciso atualizar o `dynIRT_KD_fast.R`?
**R**: **Não**. O `dynIRT_KD_fast.R` é código de referência descartado (ver MEMORY.md). Não o modifique.

### Q9: Na otimização B, a ordem dos elementos em `mu_mat[nonmiss_mask]` importa?
**R**: Sim, e está correta por construção. R lineariza matrizes em **column-major order**. Como `mu_mat`, `rc_sub`, `tmp_ystar` e `tmp_ystar_var` têm as mesmas dimensões (N × |j_idx|), e todas usam a mesma máscara booleana `nonmiss_mask`, os elementos extraídos e escritos correspondem às mesmas posições (i, jj). Não é necessário reordenar.

### Q10: Devo rodar o `v1_simulation_study.R` completo (10 seeds)?
**R**: Para validação, basta rodar **uma replicação** (seed=42) e comparar os resultados numéricos (ideal points, alpha, beta) com os da versão original. Se forem idênticos (com `ncores=1`), a refatoração está correta. Rodar com `ncores > 1` e verificar que os resultados são iguais confirma o paralelismo.

### Q11: E se `testthat` não estiver instalado?
**R**: Instale com `install.packages("testthat")`. Mas ele já deve estar instalado — os 142 testes existentes dependem dele.

### Q12: Posso mudar `kalman.R` ou `m_step.R`?
**R**: **Não**. Essas otimizações não tocam nesses arquivos. Só modifique `scripts/R/dynIRT_KD.R` e `scripts/R/da_step.R`.

### Q13: Como verifico que o paralelismo realmente está mais rápido?
**R**: Compare `result$runtime$seconds` entre `ncores=1` e `ncores=4` (ou quantos cores a máquina tiver). Espere speedup de ~2-4x com 4 cores (overhead de fork reduz o ganho teórico). Para N=100 da simulação o ganho pode ser modesto; o ganho real aparece com N=180 dos dados de tratados.

### Q14: E se eu quiser rodar os testes em paralelo?
**R**: Os testes são independentes entre si. Você pode rodar os 4 arquivos de teste em paralelo no terminal. Mas cada arquivo de teste deve ser rodado sequencialmente (o `testthat` cuida disso internamente).
