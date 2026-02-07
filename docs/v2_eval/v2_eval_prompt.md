# Prompt — V2 Evaluation (Holdout, Temporal, Robustness)

Você é um agente executor. Rode a avaliação comparativa entre `emIRT::dynIRT()` e `dynIRT_KD()` usando:

1) **Holdout 20%** por período (5 seeds)  
2) **Generalização temporal** (treinar períodos iniciais, testar últimos 2)  
3) **Robustez** = repetir holdout com 5 seeds  

Métricas obrigatórias: **log‑score** e **Brier score**.

## O que rodar (sessão fresh)

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"

# Gerar splits
Rscript scripts/R/v2_eval/prepare_splits.R 2>&1 | tee logs/v2_eval_prepare_splits.log

# Rodar avaliação completa
Rscript scripts/R/v2_eval/run_all.R 2>&1 | tee logs/v2_eval_run_all.log
```

## Saídas esperadas

- `outputs/v2_eval/splits.rds`
- `outputs/v2_eval/results.rds`
- `outputs/v2_eval/summary.csv`
- Logs:
  - `logs/v2_eval_prepare_splits.log`
  - `logs/v2_eval_run_all.log`

## Observações

- Usar sessão **fresh** (novo Rscript) para evitar contaminação de memória.
- O script já fixa seeds para cada split.
- O controle está configurado para `maxit = 500`, `ncores = 1`.
