# R5 Robustness Check: 3-Year Temporal Windows

**Date**: 2026-02-09
**Agent**: Codex (gpt-5.3-codex) | ~60.5K tokens
**Scripts**: `scripts/R/r5_prepare_3year_data.R`, `scripts/R/r5_estimate_3year.R`, `scripts/R/r5_compare_with_baseline.R`

## Design

Tests whether results are robust to finer temporal resolution. Instead of 6 five-year periods (1990-94 through 2015-18), uses 10 three-year periods (1990-92 through 2017-18).

**Pipeline**:
1. Rebuild flow matrices from raw treaty events with 3-year periodization (T=10)
2. Re-estimate 2D dynIRT_KD with V7 anchors, same omega2=0.1
3. Compare with V7 5-year baseline at 6 mapped period pairs

**Period mapping** (5yr → 3yr):
- 1990-94 ↔ 1990-92 | 1995-99 ↔ 1996-98 | 2000-04 ↔ 2002-04
- 2005-09 ↔ 2005-07 | 2010-14 ↔ 2011-13 | 2015-18 ↔ 2017-18

## Phase 1: 3-Year Flow Matrices

| Domain                | N   | J    | T  | Density |
|-----------------------|-----|------|----|---------|
| investment            | 203 | 2629 | 10 | 90.2%   |
| security              | 164 | 879  | 10 | 59.6%   |
| environment           | 206 | 2444 | 10 | 76.2%   |
| human_rights          | 197 | 58   | 10 | 70.9%   |
| arms_control          | 195 | 38   | 10 | 46.1%   |
| intellectual_property | 179 | 41   | 10 | 73.7%   |

Country and item counts match the 5-year versions. Investment and environment remain the densest domains; arms_control the sparsest.

## Phase 2: Estimation

All 6 domains converged (conv=1) with Rcpp acceleration.

## Phase 3: Comparison with V7 Baseline

### Cross-sectional correlations (dim1) at mapped periods

| Domain                | 1990 | 1996 | 2002 | 2005 | 2011 | 2017 | Min   |
|-----------------------|------|------|------|------|------|------|-------|
| investment            | 0.88 | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 | 0.88  |
| security              | 0.97 | 0.98 | 0.98 | 0.98 | 0.98 | 0.98 | 0.97  |
| environment           | 0.96 | 0.98 | 0.99 | 0.98 | 0.98 | 0.99 | 0.96  |
| human_rights          | 0.88 | 0.94 | 0.94 | 0.94 | 0.95 | 0.95 | 0.88  |
| arms_control          | **-0.84** | 0.01 | 0.66 | 0.85 | 0.91 | 0.92 | **-0.84** |
| intellectual_property | 0.88 | 0.95 | 0.97 | 0.98 | 0.98 | 0.98 | 0.88  |

### Cross-sectional correlations (dim2) at mapped periods

| Domain                | 1990 | 1996 | 2002 | 2005 | 2011 | 2017 | Min    |
|-----------------------|------|------|------|------|------|------|--------|
| investment            | 0.81 | 0.99 | 0.99 | 0.99 | 0.99 | 0.99 | 0.81   |
| security              | 0.90 | 0.91 | 0.95 | 0.95 | 0.95 | 0.95 | 0.90   |
| environment           | 0.98 | 0.98 | 0.98 | 0.98 | 0.98 | 0.98 | 0.98   |
| human_rights          | **-0.38** | **-0.88** | **-0.92** | **-0.96** | **-0.96** | **-0.96** | **-0.96** |
| arms_control          | 0.74 | 0.86 | 0.95 | 0.96 | 0.96 | 0.96 | 0.74   |
| intellectual_property | **-0.55** | **-0.46** | **-0.69** | **-0.88** | **-0.89** | **-0.33** | **-0.89** |

### Aggregate trend correlations (mean dim1 across mapped periods)

| Domain                | Trend r |
|-----------------------|---------|
| investment            | **0.972** |
| security              | **0.993** |
| environment           | **0.815** |
| human_rights          | 0.076   |
| arms_control          | **-0.611** |
| intellectual_property | **0.973** |

## Key Findings

### Dim1 (ILO support dimension) — cross-sectional rankings

1. **Robust (cor > 0.95 everywhere)**: security, environment
2. **Robust after first period (cor > 0.94 from period 2+)**: investment, human_rights, intellectual_property
3. **Problematic**: arms_control — **negative correlation at period 1** (-0.84), weak at period 2 (0.01), recovers to 0.91+ only by period 5-6

### Dim1 — aggregate trends

4. **Strong trend preservation**: investment (0.97), security (0.99), IP (0.97)
5. **Moderate**: environment (0.82) — direction preserved but magnitudes differ with 3-year resolution
6. **Weak/broken**: human_rights (0.08) — essentially zero trend correlation. The 5-year trend is nearly flat and the 3-year trend fluctuates around zero, so the correlation is dominated by noise.
7. **Reversed**: arms_control (-0.61) — the temporal trajectory under 3-year windows moves in the opposite direction from the 5-year baseline. This is the most concerning finding.

### Dim2 (confounder dimension)

8. **Systematically negative** for human_rights and intellectual_property — the 3-year dim2 is **inverted** relative to the 5-year dim2. This likely reflects identification sensitivity: with finer temporal resolution and the same anchors, the second dimension can flip sign.
9. **Stable** for environment (>0.97 everywhere) and arms_control (>0.74)

### Interpretation

- **3 of 6 domains are robust to temporal resolution** (investment, security, IP on dim1)
- **Environment is moderately robust** (cross-sections stable, trend slightly weaker)
- **Arms control and human rights are sensitive**: sparse domains (J=38, J=58) with low density struggle when periods are shortened, as each 3-year window has fewer events to inform ideal points
- The **first period** (1990-92 vs 1990-94) is consistently the weakest — shorter observation window compounds sparsity
- **Dim2 instability** in HR and IP suggests the second dimension is identified less robustly at finer temporal resolution

## Files

- Flow matrices: `data/processed/{domain}_flow_matrix_3year.rds` (6 files)
- Estimation results: `outputs/r5_3year_windows/{domain}_results.rds` (6 files)
- Comparison CSV: `outputs/r5_3year_windows/comparison_table.csv`
- Trend CSV: `outputs/r5_3year_windows/trend_comparison.csv`
- Text report: `outputs/r5_3year_windows/comparison_report.txt`
- Logs: `logs/r5_3yr_{domain}_*.log`
- Scripts: `scripts/R/r5_prepare_3year_data.R`, `scripts/R/r5_estimate_3year.R`, `scripts/R/r5_compare_with_baseline.R`
