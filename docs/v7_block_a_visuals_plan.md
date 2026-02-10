# Plano — V7 Block A: Visualizações e revisão de discussão

Data: 2026-02-08

## Objetivo
Implementar o conjunto completo de visualizações solicitadas para o relatório `outputs/v7_report/block_a_report.md`, cobrindo validação externa (UNGA/V-Dem), robustez flow vs stock, dinâmicas temporais e diagnósticos específicos (human rights), e atualizar a discussão do relatório com base nos resultados.

## Escopo
- Domínios: investment, security, environment, human_rights, arms_control, intellectual_property.
- Períodos: 6 janelas (1990–2018).
- Estimações: flow e stock (quando disponíveis).

## Saídas esperadas
- Figuras em `outputs/v7_report/figs/`.
- Relatório atualizado: `outputs/v7_report/block_a_report.md` com figuras e discussão revisada.
- Script reprodutível em R para gerar todos os gráficos.

## Fase 1 — Inventário e preparação (gate)
1. Localizar artefatos V7 (flow/stock) e metadados necessários (period labels, country codes).
2. Validar presença de UNGA e V‑Dem por período.
3. Definir conjunto de países destacados para trajetórias dinâmicas.

## Fase 2 — Visualizações de validação externa
1. Small multiples: scatter Dim1 vs UNGA por período (1 painel por período) para cada domínio.
2. Small multiples: scatter Dim1 vs V‑Dem por período para cada domínio.
3. Heatmap de correlações (domínio × período) para UNGA e V‑Dem.
4. Trajetórias bivariadas (UNGA vs V‑Dem vs Dim1) para países‑chave.

## Fase 3 — Flow vs Stock
1. Scatter Flow vs Stock por período para cada domínio (com linha identidade).
2. Trajetórias comparadas (Flow vs Stock) para países‑chave.
3. Densidades/rios por período (Flow vs Stock).
4. Dispersão (SD/IQR) por período: Flow vs Stock.

## Fase 4 — Dinâmicas temporais
1. Tendências por domínio com bandas de incerteza (mean ± SE ou bootstrap).
2. Agregado sistêmico: tendência agregada (pesos a definir) vs tendências por domínio.
3. Comparação de slopes por domínio (Flow vs Stock).

## Fase 5 — Diagnóstico específico (Human Rights)
1. Polarity check: scatter Dim1 vs V‑Dem com eixo invertido.
2. Leverage/Influence: destacar países extremos e sensíveis em HR.
3. Item‑level: bubble plot de discriminação (beta) vs ano de abertura do tratado (HR).

## Fase 6 — Atualização do relatório
1. Inserir figuras com captions numeradas.
2. Revisar discussão de Finding 1–3 com base nos novos gráficos.
3. Adicionar seção breve sobre HR se necessário.

## Critérios de sucesso
- Todas as figuras geradas com consistência de eixos e labels.
- Discussão revisada com referência a evidências visuais.
- Reprodutibilidade assegurada via script R.

## Riscos
- Falta de resultados stock para algum domínio.
- Ausência de V‑Dem ou UNGA para alguns países/períodos.
- Necessidade de decisão sobre pesos para agregado sistêmico.

