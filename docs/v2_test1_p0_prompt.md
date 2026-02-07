# Tarefa: Teste P=0 (Mean-Field Approximation) para V2

## Objetivo

Verificar se a discrepância entre `dynIRT_KD(K=1)` e `emIRT::dynIRT()` é explicada pelo uso de `P_smooth` no M-step. O nosso M-step calcula o suficiente-estatístico completo `E[x x'|Y] = Var[x|Y] + E[x]E[x]'`, enquanto variational inference tipicamente usa apenas `E[x]E[x]'` (sem `Var[x|Y]`).

Se zerar `P_smooth` no M-step e a correlação com emIRT subir para ~0.99, a diferença é algorítmica (EM exato vs mean-field VI). Se não subir, a causa é outra.

## Contexto

- V2 original: correlações r=0.965 (ideal points), r=0.932 (alpha), r=0.945 (beta). Target era r>0.99.
- Log: `logs/v2_equivalence_retry.log`
- Script original: `scripts/R/v2_equivalence_test.R`
- Dados: `data/processed/environment_flow_matrix.rds` (N=206, J=1849, T=6)
- Resultados anteriores: `outputs/v2_equivalence/v2_results.rds`

## Pastas de trabalho

- Criar/modificar arquivos em: `scripts/R/`, `outputs/v2_test_p0/`, `logs/`
- NÃO modificar: `scripts/R/m_step.R` (o original), `scripts/R/dynIRT_KD.R` (o original), `outputs/v2_equivalence/`

## Passos

### Passo 1: Criar uma versão modificada do M-step

Criar `scripts/R/m_step_p0.R` copiando `scripts/R/m_step.R` e modificando a função `m_step_items` para aceitar um flag `use_P_smooth = TRUE` (default). Quando `use_P_smooth = FALSE`, zerar o `P_sum`:

```r
# Em m_step_p0.R, modificar m_step_items:
m_step_items_p0 <- function(y_star, rc, x_smooth, P_smooth,
                             bill.session, beta_mu, beta_sigma,
                             voters_by_item = NULL,
                             use_P_smooth = TRUE) {
  # ... (copiar todo o código de m_step_items)

  # Na parte onde calcula P_sum (linhas 65-68 do original):
  if (use_P_smooth) {
    P_sum <- matrix(0, nrow = K, ncol = K)
    for (i in voters) {
      P_sum <- P_sum + P_smooth[[i]][[t_j]]
    }
  } else {
    P_sum <- matrix(0, nrow = K, ncol = K)  # ZERO — ignora Var[x|Y]
  }

  # ... (resto igual)
}
```

IMPORTANTE: Não modificar o `m_step.R` original. Criar um arquivo separado `m_step_p0.R`.

### Passo 2: Criar o script de teste

Criar `scripts/R/v2_test1_p0.R` que:

1. Carrega os dados (`data/processed/environment_flow_matrix.rds`)
2. Roda `emIRT::dynIRT()` com os mesmos parâmetros do V2 original (copiar de `scripts/R/v2_equivalence_test.R` linhas 1-88)
3. Roda `dynIRT_KD(K=1)` **duas vezes**:
   - (a) Com `P_smooth` normal (controle — deve reproduzir V2 original)
   - (b) Com `P_smooth = 0` no M-step (teste P=0)

Para a execução (b), a forma mais simples é:
- Source `scripts/R/m_step_p0.R` e `scripts/R/da_step.R` e `scripts/R/kalman.R`
- Temporariamente sobrescrever `m_step_items` no environment com `m_step_items_p0` (com `use_P_smooth = FALSE`)
- Chamar `dynIRT_KD()` normalmente (ele chamará a versão sobrescrita)
- Restaurar o original depois

Alternativa mais limpa: copiar `dynIRT_KD.R` para `dynIRT_KD_p0.R`, modificando apenas a chamada ao M-step para usar `m_step_items_p0(..., use_P_smooth = FALSE)`. Preferir esta abordagem.

Parâmetros para ambas as execuções do dynIRT_KD:
```r
K <- 1L
set.seed(123)
# ... (mesmos starts, priors, anchors que v2_equivalence_test.R)
.control = list(
  threads        = 1L,
  verbose        = TRUE,
  thresh         = 1e-6,
  maxit          = 800L,      # Aumentar um pouco para dar margem
  checkfreq      = 50L,
  estimate_omega = FALSE,
  ncores         = 8L         # Usar paralelismo
)
```

### Passo 3: Comparar resultados

Computar e reportar:

```r
# Três comparações:
# 1. emIRT vs dynIRT_KD (P_smooth normal) — controle, deve ser ~0.965
# 2. emIRT vs dynIRT_KD (P=0) — teste principal
# 3. dynIRT_KD (normal) vs dynIRT_KD (P=0) — quanto P_smooth muda a solução

# Para cada comparação, reportar:
# - Correlação ideal points por período (excl. anchors)
# - Correlação média
# - Correlação alpha
# - Correlação beta
# - LL de cada solução (usar compute_loglik do dynIRT_KD.R)
```

### Passo 4: Salvar resultados

```r
dir.create("outputs/v2_test_p0", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(
  # Resultados emIRT
  x_emirt = x_emirt, alpha_emirt = alpha_emirt, beta_emirt = beta_emirt,
  # Resultados dynIRT_KD normal
  x_kd_normal = x_kd_normal, alpha_kd_normal = alpha_kd_normal, beta_kd_normal = beta_kd_normal,
  # Resultados dynIRT_KD P=0
  x_kd_p0 = x_kd_p0, alpha_kd_p0 = alpha_kd_p0, beta_kd_p0 = beta_kd_p0,
  # Correlações
  cor_emirt_vs_normal = ...,
  cor_emirt_vs_p0 = ...,
  cor_normal_vs_p0 = ...,
  # LL
  ll_emirt = ..., ll_kd_normal = ..., ll_kd_p0 = ...
), "outputs/v2_test_p0/results.rds")
```

### Passo 5: Log

Redirecionar toda a saída para `logs/v2_test1_p0.log`:

```r
# No início do script:
sink("logs/v2_test1_p0.log", split = TRUE)
# ... todo o código ...
sink()
```

Ou rodar via:
```bash
Rscript scripts/R/v2_test1_p0.R 2>&1 | tee logs/v2_test1_p0.log
```

## Validações

Antes de terminar, verificar:

- [ ] A execução (a) (P_smooth normal) reproduz os resultados V2 originais (correlações devem ser ~0.965 ± 0.002)
- [ ] A execução (b) (P=0) completa sem erro
- [ ] As 3 tabelas de correlação estão no log
- [ ] As LL das 3 soluções estão no log
- [ ] O arquivo `outputs/v2_test_p0/results.rds` existe e contém todos os campos

## Saídas esperadas

1. `scripts/R/m_step_p0.R` — M-step modificado com flag `use_P_smooth`
2. `scripts/R/v2_test1_p0.R` — Script do teste
3. `outputs/v2_test_p0/results.rds` — Resultados serializados
4. `logs/v2_test1_p0.log` — Log completo

## Interpretação dos resultados

| Cenário | Correlação emIRT vs P=0 | Conclusão |
|---------|------------------------|-----------|
| r sobe para ~0.99 | P_smooth é a causa da discrepância. Diferença algorítmica confirmada. |
| r sobe para ~0.98 | P_smooth explica parte. Pode haver fatores adicionais (anchors, outros). |
| r não muda (~0.96) | P_smooth NÃO é a causa principal. Investigar outras hipóteses. |

## Se falhar

- Se `emIRT::dynIRT()` falhar: verificar que `library(emIRT)` está disponível. Se não, `install.packages("emIRT")`.
- Se `dynIRT_KD()` falhar: verificar que `scripts/R/da_step.R`, `scripts/R/kalman.R`, `scripts/R/m_step.R` existem e são source-áveis.
- Se a execução P=0 não convergir em 800 iterações: tudo bem, reportar os resultados na iteração final. NÃO aumentar maxit além de 800.
- Se a execução controle não reproduzir V2 (~0.965): algo mudou no código base. Investigar antes de prosseguir.
- NÃO ficar em loop tentando. Se algo falhar 2x, reportar o erro e parar.
