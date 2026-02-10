# Task: V8 Validation — UNGA + V-Dem correlations for extended data (2020-2024)

Do NOT ask questions. Execute everything silently. Working directory: `/Users/manoelgaldino/Documents/DCP/Papers/Ideal point`

## Context

V8 extended 5 domains to T=7 (1990-2024). Security remains at T=6 (1990-2018, from V7).
We need to re-run external validation (UNGA ideal points + V-Dem liberal democracy index) for the V8 results.

**V8 period structure** (for 5 extended domains):
- Period 1: 1990-1994
- Period 2: 1995-1999
- Period 3: 2000-2004
- Period 4: 2005-2009
- Period 5: 2010-2014
- Period 6: 2015-2019
- Period 7: 2020-2024

**V7 security period structure** (unchanged):
- Period 1-5: same as above
- Period 6: 2015-2018 (note: NOT 2015-2019)

## Step 1: Create `scripts/R/v8_prepare_unga.R`

Adapt from `scripts/R/04_prepare_unga.R`. Key changes:
- `period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)`
- `period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024")`
- Filter `year >= 1990 & year <= 2024` (not 2018)
- Output to `data/processed/unga_ideal_points_period_v8.csv`
- Same column names: iso3, period, unga_ideal_point (note: original uses iso3, not iso3c)

The raw UNGA file is a Stata .dta file at `data/raw/unga_ideal_points/IdealpointsJuly2025.tab`. Read the manifest at `data/raw/unga_ideal_points/manifest.json` to get the path. Use `haven::read_dta()`.

## Step 2: Create `scripts/R/v8_prepare_vdem_period.R`

Adapt from `scripts/R/v7_prepare_vdem_period.R`. Key changes:
- Add period 7: `y >= 2020 & y <= 2024 ~ "2020-2024"` (V-Dem data goes to 2022, so period 7 will have 3 years only)
- Change period 6 from `y >= 2015 & y <= 2018` to `y >= 2015 & y <= 2019`
- Output to `data/processed/vdem_liberal_democracy_period_v8.csv`

Input: `data/raw/vdem/vdem_country_year_v14.rds`

## Step 3: Create `scripts/R/v8_validation.R`

Adapt from `scripts/R/v7_validation.R`. Key changes:

### Loading results
- Load 5 V8 domains from `outputs/v8_extended/{domain}_results.rds`
- Load security from `outputs/v7_country_anchors/security_results.rds`
- All 6 domains have `$ideal_points` (3D: N x K x T), `$country_codes`, `$period_labels`

### Period normalization
The `normalize_period()` function must handle BOTH naming conventions:
- V8 domains: periods 1-7 with labels "1990-1994" through "2020-2024", period 6 = "2015-2019"
- V7 security: periods 1-6 with labels "1990-1994" through "2015-2018", period 6 = "2015-2018"

For joining with UNGA/V-Dem, use period NUMBER (integer), not label. Map security's "2015-2018" to period 6 (same as V8's "2015-2019" period 6 — they overlap).

### UNGA and V-Dem loading
- UNGA: `data/processed/unga_ideal_points_period_v8.csv` (from Step 1)
- V-Dem: load from `data/raw/vdem/vdem_country_year_v14.rds` and aggregate to periods inline (same as v7_validation.R does), but with 7 periods matching V8

### Output
- Directory: `outputs/v8_validation/`
- Files: `correlation_table.csv`, `aggregate_trends.csv`, `validation_report.txt`
- Report should clearly show overall correlations per domain and note which periods are covered

### Important detail
The UNGA CSV uses column `iso3` (not `iso3c`). The V8 results use `country_codes` which are ISO3C codes. Make sure the join works by standardizing to the same column name.

## Step 4: Run all three scripts

```r
source("scripts/R/v8_prepare_unga.R")
source("scripts/R/v8_prepare_vdem_period.R")
source("scripts/R/v8_validation.R")
```

## Step 5: Print key results

After running, print:
1. The overall correlation table (period==0) showing r_unga_dim1 and r_vdem_dim1 per domain
2. The period-level correlations for the new period 7 (2020-2024) if available
3. A comparison: V7 overall correlations (from `outputs/v7_validation/correlation_table.csv`) vs V8 overall correlations

## Verification

- All 6 domains should appear in the output
- UNGA should have 7 periods of data (1990-2024)
- V-Dem should have 7 periods of data (period 7 partial: 2020-2022)
- Security should have 6 periods only
- Correlation values should be finite for all domains with n > 3
