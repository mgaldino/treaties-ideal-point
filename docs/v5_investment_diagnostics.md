# Diagnóstico V5 — investment

Data: 2026-02-07

## Arquivos usados

- Resultados principais: `outputs/v5_per_domain_2d/investment_2d_results.rds`
- Resumo de snapshots: `outputs/v5_per_domain_2d_diag/diagnostic_summary.csv`
- Gráfico de tendências: `outputs/v5_per_domain_2d_diag/investment_snapshot_trends.pdf`

## 1. Traço de log-verossimilhança (LL)

Observações:

- LL inicial: -128033.278
- LL final: -22029.603
- Ganho total de LL: 106003.675
- Iteração com 90% do ganho total: 9
- Iteração com 95% do ganho total: 19
- Iteração com 99% do ganho total: 282
- Média de ΔLL (primeiras 100 iterações): 1041.8724
- Média de ΔLL (últimas 500 iterações): 0.0065
- Média de ΔLL (últimas 100 iterações): 0.0054

Q-function não é calculada no código atual; o diagnóstico usa apenas a LL observada.

Tabela 1 — Resumo por intervalo de 500 iterações (LL e estabilidade prática).

| iter_prev| iter_curr| loglik_delta| p95_abs_mean| max_abs_mean| cor_mean_dim1| cor_mean_dim2|stable_both |
|---------:|---------:|------------:|------------:|------------:|-------------:|-------------:|:-----------|
|       500|      1000|      306.602|        0.859|        2.555|         0.989|         0.968|FALSE       |
|      1000|      1500|      210.706|        0.691|        2.963|         0.991|         0.957|FALSE       |
|      1500|      2000|      158.093|        0.594|        2.966|         0.996|         0.965|FALSE       |
|      2000|      2500|       50.697|        0.334|        1.455|         0.998|         0.994|FALSE       |
|      2500|      3000|       27.818|        0.169|        0.852|         0.999|         0.998|FALSE       |
|      3000|      3500|       14.227|        0.121|        0.612|         1.000|         0.999|FALSE       |
|      3500|      4000|        8.188|        0.106|        0.393|         1.000|         0.999|FALSE       |
|      4000|      4500|        5.050|        0.101|        0.268|         1.000|         1.000|FALSE       |
|      4500|      5000|        3.235|        0.097|        0.203|         1.000|         1.000|FALSE       |

Figura 1 — Tendência de estabilização (LL e p95_abs_mean).

Arquivo: `outputs/v5_per_domain_2d_diag/investment_snapshot_trends.pdf`

## 2. Medida de rotação (Procrustes) entre períodos

Definição: para cada par de períodos adjacentes (t-1, t), aplicamos Procrustes ortogonal
com centragem (sem escala) para alinhar `x_t` em `x_{t-1}` e calculamos a correlação por dimensão.

- Correlação mínima por dimensão (mínimo entre períodos): 0.937
- Correlação média mínima por dimensão: 0.982

Tabela 2 — Correlação por dimensão após alinhamento Procrustes.

| period_prev| period_curr| cor_dim1| cor_dim2| cor_min|
|-----------:|-----------:|--------:|--------:|-------:|
|           1|           2|    0.989|    0.937|   0.937|
|           2|           3|    0.993|    0.980|   0.980|
|           3|           4|    0.997|    0.996|   0.996|
|           4|           5|    0.999|    0.997|   0.997|
|           5|           6|    1.000|    0.999|   0.999|

## 3. Norma máxima de β ao longo dos snapshots

Definição: para cada snapshot, calculamos o maior valor da norma L2 por item
(`max_beta_l2`) e o maior coeficiente absoluto (`max_beta_abs`).

Tabela 3 — Norma máxima de β por snapshot.

| iter| max_beta_l2| max_beta_abs|
|----:|-----------:|------------:|
| 1000|       2.227|        2.131|
| 1500|       2.288|        2.197|
| 2000|       2.255|        2.142|
| 2500|       2.478|        2.357|
| 3000|       2.565|        2.458|
| 3500|       2.597|        2.506|
| 4000|       2.613|        2.539|
| 4500|       2.624|        2.564|
| 5000|       2.630|        2.583|
