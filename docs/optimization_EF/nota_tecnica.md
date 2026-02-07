# Nota Técnica — Otimização EF do dynIRT_KD (M-step)

**Data**: 2026-02-06  
**Objetivo**: Acelerar o M-step (atualização de α e β) sem alterar a solução do EM.

## Resumo da mudança

A função `m_step_items()` foi reescrita para **acumular estatísticas suficientes em forma vetorizada**, reduzindo o custo do loop interno sobre países. O cálculo agora usa:

- `sum_x = colSums(X)`
- `sum_xx = crossprod(X)`
- `sum_y = sum(Y)`
- `sum_yx = crossprod(X, Y)`
- `sum_P = Σ_i P_it`

Com isso, o cálculo de `Sigma_zz` e `Sigma_zy` deixa de construir `Ez_outer` para cada país e passa a usar álgebra matricial (BLAS). A soma das covariâncias `P_it` permanece em loop, porém com custo O(K²) por país (K pequeno), resultando em menor overhead total.

Além disso, foi introduzido o parâmetro opcional `voters_by_item` (lista de índices por item), para evitar `which(rc[, j] != 0L)` a cada iteração do EM. Em `dynIRT_KD()`, essa lista é pré‑computada uma única vez.

## Compatibilidade

- Interface pública de `dynIRT_KD()` inalterada.
- `m_step_items()` mantém os mesmos argumentos obrigatórios; `voters_by_item` é opcional.
- Resultados numéricos permanecem equivalentes (testes unitários atualizados).

## Testes

- Adicionado teste: `m_step_items: voters_by_item yields identical results`.
- Rodar:

```r
Rscript -e 'testthat::test_file("tests/test_m_step.R")'
```

## Benchmark

Script: `scripts/R/optimization_EF/benchmark_m_step_fast.R`  
Saídas: `outputs/optimization_EF/benchmark_m_step_summary.{rds,txt}`

Esse script compara a implementação lenta (referência) com a versão atual e registra speedup e diferenças máximas em α e β.

### Resultados (M-step isolado)

- **Speedup**: 4.25x (0.068s → 0.016s)  
- **Diferenças numéricas**: max |α| = 5.6e-17, max |β| = 2.8e-16  
- **Arquivo**: `outputs/optimization_EF/benchmark_m_step_summary.txt`  
- **Log**: `logs/benchmark_m_step_optEF_20260206T015200Z.log`

---

## Benchmark end-to-end (V1)

Script: `scripts/R/optimization_EF/run_v1_benchmark_EF.R`  
Saídas: `outputs/optimization_EF/v1_benchmark_summary.{rds,txt}`

### Resultados

- **Equivalência**: `x=TRUE`, `alpha=TRUE`, `beta=TRUE`  
- **Tempo (slow)**: 63.58s (500 iterações, conv=0)  
- **Tempo (fast)**: 51.44s (500 iterações, conv=0)  
- **Speedup**: 1.24x  
- **Arquivo**: `outputs/optimization_EF/v1_benchmark_summary.txt`  
- **Log**: `logs/v1_benchmark_optEF_20260206T020230Z.log`

---

## Observação sobre cores no sandbox

`parallel::detectCores()` retornou `NA` neste ambiente, portanto o código caiu para **1 core**.  
Log: `logs/detect_cores_20260206T020000Z.log`

---

## Testes

- Bateria completa: `testthat::test_dir("tests")`  
- **Resultado**: 147 testes passaram  
- **Log**: `logs/test_all_optEF_20260206T020100Z.log`
