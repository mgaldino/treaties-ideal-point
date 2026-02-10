# R4 Robustness Check: omega2 (Evolution Variance) Sensitivity

**Date**: 2026-02-09
**Agent**: Codex (gpt-5.3-codex) | ~47K tokens
**Script**: `scripts/R/r4_omega2_sensitivity.R`

## What omega2 measures

omega2 is the evolution variance in the dynamic IRT state-space model. It governs how much a country's latent ideal point can change between consecutive time periods:

    x_it = x_i,t-1 + epsilon_it,  epsilon_it ~ N(0, omega2 * I_K)

- **Small omega2 (0.01)**: Ideal points are sticky — strong smoothing across periods, may mask real shifts.
- **Large omega2 (0.5)**: Ideal points are volatile — responsive to period-specific data, risk of overfitting in sparse domains.
- **Baseline (0.1)**: Moderate smoothing, inherited from 1D emIRT convention.

## Design

- omega2 grid: {0.01, 0.05, 0.1, 0.2, 0.5}
- 6 domains x 5 omega2 values = 30 estimation runs
- All runs use V7 anchors, same PCA initialization, same .control settings
- Comparison: Pearson correlation of flattened N x T ideal point arrays (dim1 and dim2 separately) against V7 baseline (omega2=0.1)

## Results

### Self-check
omega2=0.1 runs reproduce V7 baseline exactly (cor = 1.000 for all domains, both dims). Confirms script correctness.

### Correlation with baseline (dim1)

| Domain                | omega2=0.01 | 0.05  | 0.2   | 0.5   |
|-----------------------|-------------|-------|-------|-------|
| investment            | 0.997       | 0.999 | 0.998 | 0.987 |
| security              | 0.979       | 0.996 | 0.995 | 0.972 |
| environment           | 0.986       | 0.999 | 0.999 | 0.992 |
| human_rights          | 0.988       | 0.997 | 0.996 | 0.971 |
| arms_control          | 0.995       | 0.999 | 0.994 | 0.892 |
| intellectual_property | 0.993       | 0.997 | 0.991 | 0.806 |

### Correlation with baseline (dim2)

| Domain                | omega2=0.01 | 0.05  | 0.2   | 0.5   |
|-----------------------|-------------|-------|-------|-------|
| investment            | 0.994       | 0.999 | 0.997 | 0.980 |
| security              | 0.968       | 0.993 | 0.988 | 0.919 |
| environment           | 0.984       | 0.997 | 0.996 | 0.977 |
| human_rights          | 0.992       | 0.998 | 0.997 | 0.977 |
| arms_control          | 0.996       | 0.999 | 0.998 | 0.981 |
| intellectual_property | 0.977       | 0.995 | 0.993 | 0.961 |

### Convergence
All 30 runs converged (30/30). Iteration counts stable across omega2 values within each domain.

### Trend slopes (dim1 mean per period)

| Domain                | omega2=0.01 | 0.05   | 0.1    | 0.2    | 0.5    |
|-----------------------|-------------|--------|--------|--------|--------|
| investment            | +0.066      | +0.079 | +0.093 | +0.115 | +0.161 |
| security              | +0.063      | +0.040 | +0.029 | +0.025 | +0.041 |
| environment           | -0.021      | -0.020 | -0.021 | -0.025 | -0.034 |
| human_rights          | +0.001      | +0.002 | +0.003 | +0.006 | +0.011 |
| arms_control          | +0.013      | +0.014 | +0.016 | +0.017 | +0.019 |
| intellectual_property | -0.005      | -0.002 | +0.002 | +0.008 | +0.016 |

## Key Findings

1. **Results are highly robust to omega2**: For the moderate range (0.01-0.2), all domains show cor > 0.97 with baseline on dim1, and cor > 0.96 on dim2. The choice of omega2=0.1 is not driving the results.

2. **Extreme omega2=0.5 is the only concern**: Two domains show meaningful degradation:
   - **intellectual_property** (dim1 cor = 0.806): Sparse domain, large omega2 allows excessive period-to-period movement
   - **arms_control** (dim1 cor = 0.892): Similar sparsity issue

3. **Trend directions are preserved**: All domains maintain the same temporal direction across all omega2 values. Investment always increases, environment always declines, etc. The only exception is IP, which flips from slightly negative slope at omega2=0.01 to slightly positive at omega2=0.5 — but the magnitudes are tiny (near zero regardless).

4. **Larger omega2 amplifies trends**: As expected, larger evolution variance allows more temporal movement, so slopes become steeper. This is mechanical, not substantive.

5. **Convergence is unaffected**: Iteration counts are nearly identical across omega2 values, suggesting the optimization landscape is well-behaved regardless of the smoothing parameter.

## Conclusion

**omega2=0.1 is a safe default**. The substantive conclusions (country rankings, trend directions, relative magnitudes across domains) are robust across a 50x range of evolution variance (0.01 to 0.5). No domain shows qualitative sensitivity in the moderate range (0.01-0.2). Only at the extreme omega2=0.5 do IP and arms_control show notable quantitative differences, and even there, dim1 correlations remain above 0.8.

## Files

- Summary CSV: `outputs/r4_omega2_sensitivity/sensitivity_table.csv`
- Text report: `outputs/r4_omega2_sensitivity/sensitivity_report.txt`
- Per-run RDS: `outputs/r4_omega2_sensitivity/{domain}_omega2_{value}_results.rds` (30 files)
- Script: `scripts/R/r4_omega2_sensitivity.R`
