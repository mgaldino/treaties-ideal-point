# Block A.3 — Validation: UNGA + V-Dem for All Domains

**Do NOT ask questions. Just execute.**

Working directory: `cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"`

## What To Do

Create and run `scripts/R/v7_validation.R` that produces the core validation table for the paper.

### Step 1: Load data

- V7 ideal points: `outputs/v7_country_anchors/{domain}_results.rds` for all 6 domains
- UNGA ideal points: `data/processed/unga_ideal_points_period.csv` (already exists, has columns: iso3c, period, idealpoint)
- V-Dem: download the V-Dem Country-Year V14 dataset (or check if `data/raw/vdem/` exists). We need the `v2x_libdem` (liberal democracy) index. If download is not possible, check if any V-Dem data is already in the repo. If not available at all, skip V-Dem and report only UNGA.

### Step 2: Merge and compute correlations

For each domain:
1. Extract dim1 ideal points per country per period from V7 results
2. Merge with UNGA ideal points on (iso3c, period)
3. Merge with V-Dem liberal democracy (averaged within each 5-year period)
4. Compute:
   - `r_unga_dim1`: Pearson correlation of dim1 with UNGA, per period and overall
   - `r_vdem_dim1`: Pearson correlation of dim1 with V-Dem libdem, per period and overall
   - `r_unga_dim2`: same for dim2
   - `r_vdem_dim2`: same for dim2
   - `n`: number of matched countries per period

### Step 3: Aggregate trends

For each domain, compute per period:
- `mean_dim1`, `sd_dim1`, `skew_dim1` (use `e1071::skewness` if available, or manual formula)
- `mean_dim2`, `sd_dim2`

### Step 4: Save outputs

- `outputs/v7_validation/correlation_table.csv`: domain × period × r_unga_dim1 × r_vdem_dim1 × r_unga_dim2 × r_vdem_dim2 × n
- `outputs/v7_validation/aggregate_trends.csv`: domain × period × mean_dim1 × sd_dim1 × skew_dim1 × mean_dim2 × sd_dim2
- `outputs/v7_validation/validation_report.txt`: formatted text with key findings

### Key Questions the Validation Should Answer

1. **Does dim1 correlate with UNGA?** If yes → supports "dim1 = international alignment / ILO support"
2. **Does dim1 correlate MORE with UNGA than with V-Dem?** If yes → dim1 captures international alignment, not just domestic liberalism
3. **Does dim1 show a temporal trend?** Increasing mean = growing ILO support. Decreasing mean = erosion.
4. **Is the trend consistent across domains?** If yes → systemic phenomenon. If no → domain-specific.
5. **Does dispersion (SD) change over time?** Increasing SD = polarization.

## What To Report

1. Full correlation table
2. Which domains have strong UNGA alignment (r > 0.3)? Which don't?
3. UNGA vs V-Dem: which is higher per domain?
4. Temporal trends: direction and magnitude per domain
5. Any data issues (missing countries, merge failures)

## Constraints

- Do NOT modify existing files
- Do NOT ask questions
- If V-Dem data is not available locally, try downloading from `https://v-dem.net/data/the-v-dem-dataset/` or use the `vdemdata` R package. If neither works, skip V-Dem and report UNGA only.
- Period definitions: 1=1990-1994, 2=1995-1999, 3=2000-2004, 4=2005-2009, 5=2010-2014, 6=2015-2018
