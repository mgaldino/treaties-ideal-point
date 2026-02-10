# R2 Robustness Check: Alternative Country Anchor Sensitivity

This robustness check evaluates sensitivity of the 2D dynamic IRT estimates to the *choice of anchor countries* (with tight priors, sigma=0.01), relative to the V7 baseline (3 country anchors per domain).

## Summary Table

| domain | alt_set | overall_cor_dim1 | overall_cor_dim2 | min_period_cor_dim1 | mean_period_cor_dim1 | trend_cor_dim1 |
|---|---:|---:|---:|---:|---:|---:|
| arms_control | Alt1 | 0.712 | 0.558 | 0.686 | 0.738 | 0.935 |
| arms_control | Alt2 | 0.537 | -0.587 | -0.147 | 0.189 | 0.985 |
| environment | Alt1 | 0.929 | 0.917 | 0.923 | 0.929 | 0.816 |
| environment | Alt2 | 0.980 | 0.977 | 0.968 | 0.979 | 0.993 |
| human_rights | Alt1 | -0.872 | 0.925 | -0.953 | -0.852 | -0.334 |
| human_rights | Alt2 | 0.926 | 0.946 | 0.876 | 0.923 | 0.874 |
| intellectual_property | Alt1 | 0.980 | 0.939 | 0.978 | 0.981 | 0.241 |
| intellectual_property | Alt2 | 0.954 | 0.959 | 0.941 | 0.953 | 0.935 |
| investment | Alt1 | 0.996 | 0.969 | 0.992 | 0.996 | 1.000 |
| investment | Alt2 | 0.991 | 0.961 | 0.984 | 0.991 | 1.000 |
| security | Alt1 | 0.988 | 0.947 | 0.988 | 0.989 | 0.968 |
| security | Alt2 | 0.956 | 0.926 | 0.952 | 0.956 | 0.054 |

## Key Findings

- **investment**: dim1 overall correlation ranges from 0.991 (set=Alt2) to 0.996 (set=Alt1). Classification: Alt1=robust, Alt2=robust.
- **security**: dim1 overall correlation ranges from 0.956 (set=Alt2) to 0.988 (set=Alt1). Classification: Alt1=robust, Alt2=sensitive.
- **environment**: dim1 overall correlation ranges from 0.929 (set=Alt1) to 0.980 (set=Alt2). Classification: Alt1=sensitive, Alt2=robust.
- **human_rights**: dim1 overall correlation ranges from -0.872 (set=Alt1) to 0.926 (set=Alt2). Classification: Alt1=sensitive, Alt2=sensitive.
- **arms_control**: dim1 overall correlation ranges from 0.537 (set=Alt2) to 0.712 (set=Alt1). Classification: Alt1=sensitive, Alt2=sensitive.
- **intellectual_property**: dim1 overall correlation ranges from 0.954 (set=Alt2) to 0.980 (set=Alt1). Classification: Alt1=sensitive, Alt2=mostly_robust.

## Interpretation

High correlations indicate that cross-sectional ordering and aggregate trends are stable under alternative anchoring choices. Lower correlations suggest that the latent scale is more sensitive to identification choices (particularly on dim2, where anchoring is often harder).

Artifacts produced by this script:
- `outputs/r2_alt_anchors/comparison_table.csv`
- `outputs/r2_alt_anchors/comparison_report.txt`
- `outputs/r2_alt_anchors/R2_findings.md`
