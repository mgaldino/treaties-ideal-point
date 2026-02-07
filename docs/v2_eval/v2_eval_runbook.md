# Runbook — V2 Evaluation (Holdout, Temporal, Robustness)

**Objetivo**: comparar `emIRT::dynIRT()` vs `dynIRT_KD()` em três frentes:
- Holdout 20% por período (5 seeds)
- Generalização temporal (últimos 2 períodos)
- Robustez (repetição por seeds)

## Pré‑requisitos

- Dados: `data/processed/environment_flow_matrix.rds`
- Pacote R: `emIRT` instalado

## Passo 1: Gerar splits

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript scripts/R/v2_eval/prepare_splits.R 2>&1 | tee logs/v2_eval_prepare_splits.log
```

## Passo 2: Rodar avaliação completa

```bash
Rscript scripts/R/v2_eval/run_all.R 2>&1 | tee logs/v2_eval_run_all.log
```

## Saídas

- `outputs/v2_eval/splits.rds` — índices de teste por split
- `outputs/v2_eval/results.rds` — resultados completos (métricas + tempos)
- `outputs/v2_eval/summary.csv` — tabela resumida

## Métricas

- **Log‑score** (soma e média)
- **Brier score**

## Controles usados (padrão)

- `maxit = 500`
- `thresh = 1e-6`
- `ncores = 1`

## Observações

- Sessão fresh é **obrigatória** para reprodutibilidade.
- Se `emIRT` não estiver instalado:
  ```r
  install.packages("emIRT")
  ```
- Se o pipeline for lento, reduza seeds para 3 para um smoke test.
