# Stock Coding — Phase 2: Estimation Template

**Usage**: Same as V7 estimation template, but uses stock matrices instead of flow matrices.

Replace `{DOMAIN}`, `{DIM1_POS}`, `{DIM1_NEG}`, `{DIM2_ANCHOR}` with the same values as V7.

## Prompt (copy and parametrize per domain)

**Do NOT ask questions. Just execute.**

Working directory: `cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"`

Estimate 2D dynamic IRT for **{DOMAIN}** using **stock-coded** flow matrix.

### Create and run `scripts/R/v7_stock_{DOMAIN}.R`:

This script is identical to the V7 country-anchor script EXCEPT:
- Loads `data/processed/{DOMAIN}_stock_matrix.rds` instead of `{DOMAIN}_flow_matrix.rds`
- Saves to `outputs/v7_stock_country_anchors/{DOMAIN}_results.rds`
- Logs to `logs/v7_stock_{DOMAIN}_*.log`

Same anchors as V7:

| Domain | Dim1+ | Dim1- | Dim2 |
|--------|-------|-------|------|
| investment | DNK (+2,0) | IRN (-2,0) | CHN (0,-2) |
| security | DNK (+2,0) | IRN (-2,0) | UKR (0,-2) |
| environment | DNK (+2,0) | SAU (-2,0) | AUS (0,-2) |
| human_rights | DNK (+2,0) | PRK (-2,0) | USA (0,-2) |
| arms_control | NZL (+2,0) | ISR (-2,0) | IND (0,-2) |
| intellectual_property | DNK (+2,0) | AGO (-2,0) | BRA (0,-2) |

Same settings: K=2, maxit=5000, thresh_loglik=0.01, loglik_patience=5, ncores=4, Rcpp.

Same PCA initialization, same aggregate statistics computation.

**Report**: converged? iters? runtime? aggregate trend table (mean_dim1 per period). Any errors.

**Constraints**: Do NOT modify existing scripts. Do NOT ask questions. Fall back to pure R if Rcpp fails.
