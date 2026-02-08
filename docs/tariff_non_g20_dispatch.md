# Tariff Acquisition Dispatch Plan — Non-G20 Batch 1

**Date**: 2026-02-08
**Status**: READY FOR DISPATCH

## Goal

Acquire, parse, map, and build margins for WITS/TRAINS tariff data for 10 non-G20 countries. One Gemini agent per country.

## Countries

| # | ISO3 | Country       | Region         |
|---|------|---------------|----------------|
| 1 | SGP  | Singapore     | SE Asia        |
| 2 | THA  | Thailand      | SE Asia        |
| 3 | VNM  | Vietnam       | SE Asia        |
| 4 | CHL  | Chile         | Latin America  |
| 5 | COL  | Colombia      | Latin America  |
| 6 | PER  | Peru          | Latin America  |
| 7 | EGY  | Egypt         | MENA           |
| 8 | CHE  | Switzerland   | Europe         |
| 9 | NOR  | Norway        | Europe         |
|10 | NZL  | New Zealand   | Oceania        |

## Pipeline Per Country

Each agent runs 5 steps for its assigned country (`{ISO3}`), years 1990–2022:

### Step 1 — Acquire raw tariff data (Python)

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
python3 scripts/python/run_with_env.py -- \
  python3 scripts/python/acquire_wits_tariffs_batch.py \
    --reporters {ISO3} \
    --start-year 1990 \
    --end-year 2022 \
    --skip-existing \
    --manifest data/processed/wits_tariffs_manifest_{ISO3}_20260208.csv
```

This loads credentials from `.env` (WITS_USER, WITS_PASS) and downloads SDMX-JSON tariff files per partner per year. Some years may return "no data" — that is expected.

### Step 2 — Parse raw JSON to CSV (R)

For each year where raw data exists:

```bash
for year in $(ls data/raw/tariffs/wits_trn/{ISO3}/ 2>/dev/null); do
  if ls data/raw/tariffs/wits_trn/{ISO3}/$year/tariffs_partner_*.json 1>/dev/null 2>&1; then
    Rscript scripts/R/05_parse_wits_tariffs.R \
      --reporter-iso3 {ISO3} \
      --year $year
  fi
done
```

### Step 3 — Map HS codes to HS2012 (R)

For each year where parsed data exists:

```bash
for year in $(ls data/raw/tariffs/wits_trn/{ISO3}/ 2>/dev/null); do
  if [ -f "data/processed/tariffs/wits_trn/{ISO3}/$year/line_level_ad_valorem.csv" ]; then
    Rscript scripts/R/06_map_hs_to_2012.R \
      --reporter-iso3 {ISO3} \
      --year $year
  fi
done
```

### Step 4 — Build preferential margins (R)

For each year where mapped data exists:

```bash
for year in $(ls data/raw/tariffs/wits_trn/{ISO3}/ 2>/dev/null); do
  if [ -f "data/processed/tariffs/wits_trn/{ISO3}/$year/line_level_ad_valorem_hs2012.csv" ]; then
    Rscript scripts/R/07_build_margins.R \
      --reporter-iso3 {ISO3} \
      --year $year
  fi
done
```

### Step 5 — Consolidate into timeseries (R)

```bash
Rscript scripts/R/10_consolidate_wits_tariffs.R \
  --reporter-iso3 {ISO3}
```

## What To Report

After all 5 steps finish, report:

1. **Acquisition summary**: How many years succeeded, how many "no data", any errors?
2. **Manifest file**: path to the manifest CSV
3. **Parsed years**: list of years that produced `line_level_ad_valorem.csv`
4. **Consolidation**: confirm `margins_hs4_timeseries.csv` and `mfn_hs4_timeseries.csv` exist under `data/processed/tariffs/wits_trn/{ISO3}/`
5. **Any errors**: full error messages if a step failed

## Constraints

- Do **NOT** modify any source code files (`scripts/R/*.R`, `scripts/python/*.py`)
- Do **NOT** install packages — all dependencies are already available
- Do **NOT** ask questions — just run the pipeline and report results
- If a year has no data (WITS returns "Data not found"), that is expected. Skip it and continue.
- If a transient HTTP error occurs, the batch script retries automatically.
- If R or Python is not found, try `/usr/local/bin/Rscript` or `/usr/bin/python3`

## Expected Outputs Per Country

```
data/raw/tariffs/wits_trn/{ISO3}/{YEAR}/
  countries_metadata.xml
  data_availability.xml
  tariffs_partner_*.json
  access_log.json

data/processed/tariffs/wits_trn/{ISO3}/{YEAR}/
  line_level_all.csv
  line_level_ad_valorem.csv
  line_level_ad_valorem_hs2012.csv
  margins_hs4.csv
  mfn_hs4.csv

data/processed/tariffs/wits_trn/{ISO3}/
  margins_hs4_timeseries.csv
  mfn_hs4_timeseries.csv

data/processed/wits_tariffs_manifest_{ISO3}_20260208.csv

outputs/validation/
  wits_tariff_pilot_summary_{ISO3}_{YEAR}.csv
  wits_hs_mapping_summary_{ISO3}_{YEAR}.csv
  wits_margin_summary_{ISO3}_{YEAR}.csv
```

## Gemini Prompt Template

Copy the text below, replace `{ISO3}` with the country code, and dispatch to Gemini:

---

> **Task: Acquire and process WITS tariff data for {ISO3}**
>
> Working directory: `cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"`
>
> Run the following 5 steps in order. Replace `{ISO3}` with the actual country code throughout. Do NOT ask questions. Do NOT modify any existing scripts.
>
> **Step 1 — Acquire:**
> ```bash
> python3 scripts/python/run_with_env.py -- python3 scripts/python/acquire_wits_tariffs_batch.py --reporters {ISO3} --start-year 1990 --end-year 2022 --skip-existing --manifest data/processed/wits_tariffs_manifest_{ISO3}_20260208.csv
> ```
>
> **Step 2 — Parse (for each year with raw data):**
> ```bash
> for year in $(ls data/raw/tariffs/wits_trn/{ISO3}/ 2>/dev/null); do
>   if ls data/raw/tariffs/wits_trn/{ISO3}/$year/tariffs_partner_*.json 1>/dev/null 2>&1; then
>     Rscript scripts/R/05_parse_wits_tariffs.R --reporter-iso3 {ISO3} --year $year
>   fi
> done
> ```
>
> **Step 3 — Map HS to 2012 (for each parsed year):**
> ```bash
> for year in $(ls data/raw/tariffs/wits_trn/{ISO3}/ 2>/dev/null); do
>   if [ -f "data/processed/tariffs/wits_trn/{ISO3}/$year/line_level_ad_valorem.csv" ]; then
>     Rscript scripts/R/06_map_hs_to_2012.R --reporter-iso3 {ISO3} --year $year
>   fi
> done
> ```
>
> **Step 4 — Build margins (for each mapped year):**
> ```bash
> for year in $(ls data/raw/tariffs/wits_trn/{ISO3}/ 2>/dev/null); do
>   if [ -f "data/processed/tariffs/wits_trn/{ISO3}/$year/line_level_ad_valorem_hs2012.csv" ]; then
>     Rscript scripts/R/07_build_margins.R --reporter-iso3 {ISO3} --year $year
>   fi
> done
> ```
>
> **Step 5 — Consolidate:**
> ```bash
> Rscript scripts/R/10_consolidate_wits_tariffs.R --reporter-iso3 {ISO3}
> ```
>
> After all steps, report: (1) acquisition summary (years succeeded/no-data/errors), (2) list of parsed years, (3) confirm timeseries files exist, (4) any errors encountered.
