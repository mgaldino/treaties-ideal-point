# Nota Técnica — Otimização KB do Kalman (update em bloco)

**Data**: 2026-02-06  
**Objetivo**: Refatorar o Kalman para update em bloco por período (R = 1), reduzindo overhead de loops por item e mantendo equivalência com `emIRT::dynIRT()`.

## Resumo da mudança

- Update em bloco por período usando a forma de informação:
  - `P_new = (P^{-1} + H' H)^{-1}`
  - `x_new = P_new (P^{-1} x + H' (y - α))`
- Mantém **R = I** para equivalência na fase V2.
- Caminho sequencial preservado quando `n_t = 1`.

## Observação

Uma versão heteroscedástica (Rₜ = diag(Var[y*])) é possível, mas **não implementada** para preservar equivalência com o `emIRT::dynIRT()`.

## Testes e benchmarks

- Testes unitários em `tests/test_kalman.R` com tolerância 1e-10.
- Benchmark isolado em `scripts/R/optimization_KB/benchmark_kalman_block.R`.

### Resultados (benchmark Kalman)

- **Speedup**: 1.23x (0.027s → 0.022s)  
- **Diferenças numéricas**: max |x| = 1.7e-16, max |P| = 1.0e-17  
- **Arquivo**: `outputs/optimization_KB/benchmark_kalman_summary.txt`  
- **Log**: `logs/benchmark_kalman_optKB_20260206T023100Z.log`

### Testes

- `tests/test_kalman.R` (log: `logs/test_kalman_optKB_20260206T023000Z.log`)  
- Bateria completa: `testthat::test_dir("tests")`  
  - **Resultado**: 150 testes passaram  
  - **Log**: `logs/test_all_optKB_20260206T023200Z.log`

---

## Benchmark end-to-end (V1)

Script: `scripts/R/optimization_EF/run_v1_benchmark_EF.R`  
Saídas: `outputs/optimization_EF/v1_benchmark_summary.{rds,txt}`

### Resultados

- **Equivalência**: `x=TRUE`, `alpha=TRUE`, `beta=TRUE`  
- **Tempo (slow)**: 35.02s (500 iterações, conv=0)  
- **Tempo (fast)**: 21.20s (500 iterações, conv=0)  
- **Speedup**: 1.65x  
- **Arquivo**: `outputs/optimization_EF/v1_benchmark_summary.txt`  
- **Log**: `logs/v1_benchmark_optKB_20260206T023300Z.log`

---

## Validação Gemini (8 cores)

**Data**: 2026-02-06  
**Agente**: Gemini CLI  
**Ambiente**: macOS (Sandbox), 8 cores dedicados

Execução de benchmark end-to-end (V1) para validar performance com paralelismo (8 cores).

- **Script**: `scripts/R/optimization_EF/run_v1_benchmark_EF.R` (adaptado para 8 cores)
- **Equivalência**: `x=TRUE`, `alpha=TRUE`, `beta=TRUE`
- **Tempo (slow)**: 38.85s
- **Tempo (fast)**: 25.76s
- **Speedup**: 1.51x
