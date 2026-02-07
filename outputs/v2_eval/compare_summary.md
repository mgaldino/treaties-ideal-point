# V2 Eval — Comparação de Métricas (emIRT vs dynIRT_KD)

Direção dos deltas:
- `delta_logscore_mean` > 0 favorece dynIRT_KD
- `delta_brier` < 0 favorece dynIRT_KD

## Resumo por split_type

- holdout_by_period (n=5): logscore wins KD=1, EM=4, tie=0; mean Δlogscore=-0.000774; Brier wins KD=5, EM=0, tie=0; mean ΔBrier=-0.001117; mean time ratio KD/EM=175.30
- temporal (n=1): logscore wins KD=1, EM=0, tie=0; mean Δlogscore=0.003466; Brier wins KD=1, EM=0, tie=0; mean ΔBrier=-0.001703; mean time ratio KD/EM=159.77
