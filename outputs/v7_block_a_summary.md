# V7 Block A — Complete Results Summary

Generated: 2026-02-08

## Overview

Block A estimates 2D dynamic IRT ideal points for 6 treaty domains using the `dynIRT_KD` algorithm with Rcpp acceleration. It includes:

- **Wave 1a**: V7 flow-coded estimation (country anchors + item anchors + comparison)
- **Wave 1b**: Stock-coded estimation (build stock matrices + estimate)
- **Wave 2**: Flow-vs-stock comparison + UNGA validation

All 6 domains: investment, security, environment, human_rights, arms_control, intellectual_property.

---

## Wave 1a: V7 Flow Estimation

### Convergence Summary

| Domain | Iters | Runtime | Conv |
|--------|-------|---------|------|
| investment | 21 | 2.3s | 1 |
| security | 24 | 1.5s | 1 |
| environment | 17 | 1.5s | 1 |
| human_rights | 18 | 0.7s | 1 |
| arms_control | 19 | 0.6s | 1 |
| intellectual_property | 23 | 0.7s | 1 |

### V7 Anchors

| Domain | Dim1+ | Dim1- | Dim2 anchor |
|--------|-------|-------|-------------|
| investment | DNK(+2,0) | IRN(-2,0) | CHN(0,-2) |
| security | DNK(+2,0) | IRN(-2,0) | UKR(0,-2) |
| environment | DNK(+2,0) | SAU(-2,0) | AUS(0,-2) |
| human_rights | DNK(+2,0) | PRK(-2,0) | USA(0,-2) |
| arms_control | NZL(+2,0) | ISR(-2,0) | IND(0,-2) |
| intellectual_property | DNK(+2,0) | AGO(-2,0) | BRA(0,-2) |

### Aggregate Trends (Flow, dim1 mean per period)

| Domain | 1990-94 | 1995-99 | 2000-04 | 2005-09 | 2010-14 | 2015-18 | Direction |
|--------|---------|---------|---------|---------|---------|---------|-----------|
| investment | +0.98 | +1.21 | +1.44 | +1.46 | +1.47 | +1.47 | Increasing then flat |
| security | +0.71 | +0.86 | +0.86 | +0.89 | +0.89 | +0.89 | Increasing then flat |
| environment | -0.53 | -0.58 | -0.60 | -0.57 | -0.64 | -0.65 | Declining (more negative) |
| human_rights | +0.03 | +0.03 | +0.04 | +0.04 | +0.05 | +0.05 | Flat |
| arms_control | -0.07 | -0.04 | -0.01 | -0.00 | +0.00 | +0.01 | Slightly increasing |
| intellectual_property | -0.03 | -0.00 | +0.01 | -0.01 | -0.01 | -0.01 | Flat |

Full Wave 1a details: `outputs/v7_wave1a_summary.md`

---

## Wave 1b: Stock Estimation

### Stock Matrix Construction

Stock coding: +1 = party to treaty at any point up to and including the period (cumulative).

| Domain | Flow items | Stock items | Ratio |
|--------|-----------|-------------|-------|
| investment | 2629 | 2629 | 1.0x (all single-period) |
| security | 1216 | 1466 | 1.2x |
| environment | 680 | 1920 | 2.8x |
| human_rights | 368 | 1033 | 2.8x |
| arms_control | 198 | 701 | 3.5x |
| intellectual_property | 335 | 959 | 2.9x |

### Stock Convergence

| Domain | Iters | Runtime | Conv |
|--------|-------|---------|------|
| investment | 21 | 2.3s | 1 |
| security | 22 | 1.1s | 1 |
| environment | 16 | 1.5s | 1 |
| human_rights | 18 | 0.7s | 1 |
| arms_control | 19 | 0.6s | 1 |
| intellectual_property | 23 | 0.7s | 1 |

### Aggregate Trends (Stock, dim1 mean per period)

| Domain | 1990-94 | 1995-99 | 2000-04 | 2005-09 | 2010-14 | 2015-18 |
|--------|---------|---------|---------|---------|---------|---------|
| investment | +0.98 | +1.21 | +1.44 | +1.46 | +1.47 | +1.47 |
| security | -0.24 | -0.22 | -0.43 | -0.42 | -0.42 | -0.42 |
| environment | -0.55 | -0.58 | -0.53 | -0.40 | -0.34 | -0.29 |
| human_rights | -0.04 | +0.01 | +0.00 | +0.01 | +0.01 | +0.01 |
| arms_control | +0.07 | +0.03 | -0.01 | -0.01 | -0.02 | -0.03 |
| intellectual_property | +0.06 | +0.02 | -0.00 | -0.01 | -0.03 | -0.03 |

---

## Wave 2a: Flow vs Stock Comparison

### Per-Domain Robustness (Procrustes-aligned)

| Domain | min r_dim1 | mean r_dim1 | overall r_dim1 | Trend cor | Dir mismatch? |
|--------|-----------|-------------|----------------|-----------|---------------|
| **investment** | 1.000 | 1.000 | 1.000 | 1.000 | No |
| **environment** | 0.960 | 0.979 | 0.984 | -0.788 | YES |
| security | 0.675 | 0.705 | 0.809 | -0.667 | YES |
| intellectual_property | 0.353 | 0.713 | 0.822 | -0.619 | YES |
| arms_control | -0.171 | 0.393 | 0.555 | -0.996 | YES |
| human_rights | -0.443 | -0.405 | 0.481 | 0.378 | No (overall) |

### Key Finding: Coding Choice Matters

- **Robust** (r > 0.9): investment (trivially, stock=flow), environment
- **Sensitive** (r < 0.8): arms_control, human_rights, intellectual_property, security
- **Critical**: 4 of 6 domains show overall direction mismatch in temporal trends between flow and stock
- **Interpretation**: The "activity bias" concern is empirically validated. Stock coding shifts the ranking substantially for arms_control, human_rights, and security. The Procrustes alignment even produces negative correlations for arms_control period 1 and all human_rights periods, suggesting a near-complete axis rotation.

### Top Discrepant Countries (per domain)

- **arms_control**: EST, FIN, KOR, LTU, LVA (Baltic/Nordic early joiners)
- **security**: COD, AGO, RWA, BGR, COG (conflict-affected states)
- **human_rights**: EGY, ESP, CRI, MNE, PRT (mixed: some early Western joiners, some selective participants)
- **intellectual_property**: UZB, BRA, AGO, LIE, EGY
- **environment**: SMR, GRD, PSE, MCO, AUT (microstates + early EU participants)

Full comparison report: `outputs/v7_flow_vs_stock/comparison_report.txt`

---

## Wave 2b: UNGA Validation

### UNGA Correlation with V7 dim1 (overall)

| Domain | r_unga_dim1 | r_unga_dim2 | N matched |
|--------|-------------|-------------|-----------|
| environment | 0.525 | 0.321 | 1124 |
| intellectual_property | 0.516 | -0.449 | 1041 |
| security | 0.432 | -0.126 | 969 |
| arms_control | 0.332 | 0.326 | 1118 |
| human_rights | -0.157 | -0.250 | 1130 |
| investment | 0.094 | -0.046 | 1013 |

### Key Findings

1. **4 of 6 domains have strong UNGA alignment** (|r| > 0.3): environment, IP, security, arms_control
2. **human_rights has NEGATIVE correlation** (-0.157) — dim1 may capture something other than international alignment
3. **investment has near-zero correlation** (0.094) — expected, since BIT networks follow economic logic, not geopolitical alignment
4. **V-Dem was unavailable** (network issues) — comparison skipped

### Temporal Trends (from V7 dim1)

| Domain | delta_mean (p6-p1) | slope | delta_SD | Interpretation |
|--------|-------------------|-------|----------|----------------|
| investment | +0.489 | +0.093 | +0.319 | Growing participation, increasing dispersion |
| security | +0.183 | +0.029 | +0.212 | Moderate growth + polarization |
| arms_control | +0.079 | +0.016 | -0.517 | Slight growth, DECREASING dispersion (convergence) |
| intellectual_property | +0.022 | +0.002 | -0.032 | Essentially flat |
| human_rights | +0.012 | +0.003 | +0.133 | Essentially flat, slight polarization |
| environment | -0.117 | -0.021 | +0.581 | Declining mean + STRONG polarization |

Full validation report: `outputs/v7_validation/validation_report.txt`

---

## File Inventory

### Wave 1a (Flow)
- `outputs/v7_country_anchors/{domain}_results.rds` (6 files)
- `outputs/v7_item_anchors/{domain}_results.rds` (6 files)
- `outputs/v7_comparison/{domain}_report.txt` (6 files)
- `outputs/v7_wave1a_summary.md`

### Wave 1b (Stock)
- `data/processed/{domain}_stock_matrix.rds` (6 files)
- `outputs/v7_stock_country_anchors/{domain}_results.rds` (6 files)

### Wave 2
- `outputs/v7_flow_vs_stock/comparison_table.csv`
- `outputs/v7_flow_vs_stock/comparison_report.txt`
- `outputs/v7_flow_vs_stock/top_discrepant_countries.csv`
- `outputs/v7_validation/correlation_table.csv`
- `outputs/v7_validation/aggregate_trends.csv`
- `outputs/v7_validation/validation_report.txt`

### Scripts (all created by Codex agents)
- `scripts/R/v7_{domain}_country_anchors.R` (6 files)
- `scripts/R/v7_{domain}_item_anchors.R` (6 files)
- `scripts/R/v7_{domain}_compare.R` (6 files)
- `scripts/R/build_stock_matrices.R`
- `scripts/R/v7_stock_{domain}.R` (6 files)
- `scripts/R/v7_flow_vs_stock_compare.R`
- `scripts/R/v7_validation.R`
