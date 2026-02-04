# Plano de Retomada — Tarifas WITS (BRA 1993–2024)

Data: 2026-02-03

## Objetivo operacional
Retomar a série temporal de tarifas para o Brasil (BRA) a partir do último ponto disponível, cobrindo 1993–2024, mantendo o mesmo pipeline do piloto: aquisição (Python), parsing + mapeamento HS2012 + margens (R), diagnósticos (R) e **outputs por ano**. Ao final, gerar **CSV consolidado** por ano para MFN e margens.

## Escopo
- País: BRA
- Período: 1993–2024 (pular anos já existentes)
- Fontes e scripts: WITS/TRAINS via `scripts/python/acquire_wits_tariffs.py` e scripts R `05–09`
- Saídas: CSVs por ano + consolidado por país

## Fase A — Aquisição (Python)
Decisão: tentar **sem autenticação** WITS nesta retomada. Registrar no log e no HANDOFF.md.
1. Rodar `scripts/python/acquire_wits_tariffs.py` para BRA, anos 1993–2024, pulando diretórios já existentes.
2. Verificar:
   - presença de `countries_metadata.xml`, `data_availability.xml`;
   - presença de ao menos um `tariffs_partner_*.json`;
   - `access_log.json` sem erros (ou erros documentados).
3. Registrar logs em `logs/`.
4. Atualizar `HANDOFF.md` ao final da fase.

## Fase B — Parsing + Mapeamento + Margens (R)
Para cada ano adquirido com sucesso:
1. Parsing:
   - `scripts/R/05_parse_wits_tariffs.R`
2. Mapeamento HS2012:
   - `scripts/R/06_map_hs_to_2012.R`
3. Margens:
   - `scripts/R/07_build_margins.R`
4. Verificar geração dos CSVs de saída e resumo em `outputs/validation`.
5. Atualizar `HANDOFF.md` ao final da fase.

## Fase C — Diagnósticos e Consolidação (R)
1. Diagnóstico HS:
   - `scripts/R/08_diagnose_hs_mapping.R` (BRA, 1990–2024)
2. Atualizar sumário de mapeamento:
   - `scripts/R/09_write_mapping_summary.R`
3. Consolidar séries (BRA):
   - gerar `margins_hs4_timeseries.csv` e `mfn_hs4_timeseries.csv`
4. Atualizar `HANDOFF.md` ao final da fase.

## Critérios de teste por fase
- **Fase A**: todos os anos esperados possuem `access_log.json` e ao menos um `tariffs_partner_*.json`.
- **Fase B**: para cada ano, existem `line_level_*`, `*_hs2012.csv`, `margins_hs4.csv`, `mfn_hs4.csv` e `wits_*_summary_*.csv`.
- **Fase C**: arquivos de diagnóstico e consolidado gerados sem erros e com anos 1990–2024 presentes.
