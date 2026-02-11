# Stan v7 Full Report: Hierarchical Joint IRT

**Date**: 2026-02-10
**Status**: CONVERGED

## Model Specification

Hierarchical Joint IRT with common factor mu and domain-specific deviations gamma:

- **theta_{i,d,t}** = mu_{i,t} + gamma_{i,d}
- **y_{ij}** ~ Bernoulli_logit(alpha_j + beta_j * theta)
- **mu_{i,1}** ~ N(0, 1)
- **mu_{i,t}** ~ N(mu_{i,t-1}, omega_fixed) [random walk, omega=0.25 fixed]
- **V-Dem as predictor**: soft prior mu_{i,t} ~ N(b_v * VDem_std_{i,t}, sigma_reg) [sigma_reg=1.0 fixed]
- **gamma_{i,d}** = delta_d * Z_{i,d} + sigma_d * gamma_raw_{i,d} [non-centered]

### Identification
- **USA** fixed at +2 (all periods) — ILO leader
- **CUB** fixed at -2 (all periods) — ILO skeptic

### Domain predictors (Z)
- investment: tax_haven (binary, 32/165 countries)
- human_rights: none (zero vector)
- arms_control: nuclear_armed (binary, 7/165 countries)
- intellectual_property: none (zero vector)

## Data
- N=165 countries, J=550 items, T=7 periods, D=4 domains
- N_obs=70,715 observations, y=1 rate: 9.2%
- V-Dem coverage: 997/1141 cells (87.4%)

## MCMC Configuration
- 4 chains, 1500 warmup + 2000 sampling = 3500 iter/chain
- adapt_delta=0.95, max_treedepth=12
- Total time: 24.4 min (mean chain: 1047.6s)

## Convergence Diagnostics

| Metric | Value | Threshold | Status |
|--------|-------|-----------|--------|
| Divergences | 0 | 0 | PASS |
| Max treedepth hits | 0 | 0 | PASS |
| Max Rhat | 1.0097 | < 1.01 | PASS |
| % Rhat > 1.01 | 0.0% | < 5% | PASS |
| ESS bulk min | 999 | > 400 | PASS |
| ESS bulk median | 3663 | > 1000 | PASS |

## Parameter Estimates

### V-Dem predictor
- b_v = 0.352 (0.053) — positive, well-identified (>6 SE from zero)

### Domain-specific variance (sigma_d)
| Domain | sigma_d (SE) | Pooling factor | Interpretation |
|--------|-------------|----------------|----------------|
| investment | 3.34 (0.21) | 0.006 | Very idiosyncratic |
| human_rights | 0.75 (0.09) | 0.100 | Moderate pooling |
| arms_control | 0.60 (0.11) | 0.148 | Moderate pooling |
| IP | 1.15 (0.13) | 0.045 | Moderate-low pooling |

Pooling = omega^2 / (omega^2 + sigma_d^2)

### Domain predictors (delta)
| Domain | delta (SE) | Signal | Interpretation |
|--------|-----------|--------|----------------|
| investment | 1.08 (0.53) | ~2 SE | Tax havens ratify more investment treaties |
| human_rights | 0.00 (0.99) | — | No predictor |
| arms_control | **-1.02 (0.37)** | **~3 SE** | Nuclear states ratify fewer arms control treaties |
| IP | -0.00 (1.00) | — | No predictor |

## Rankings (mu_mean = aggregate ILO support)

### Top 15
| Rank | Country | mu_mean |
|------|---------|---------|
| 1 | USA | 2.000 (anchor) |
| 2 | SVN | 0.825 |
| 3 | MNE | 0.798 |
| 4 | ESP | 0.756 |
| 5 | DEU | 0.744 |
| 6 | GBR | 0.733 |
| 7 | BEL | 0.725 |
| 8 | HRV | 0.679 |
| 9 | SVK | 0.679 |
| 10 | URY | 0.626 |
| 11 | CZE | 0.621 |
| 12 | ITA | 0.597 |
| 13 | DNK | 0.590 |
| 14 | NZL | 0.571 |
| 15 | LTU | 0.560 |

### Bottom 15
| Rank | Country | mu_mean |
|------|---------|---------|
| 1 | MHL | -2.080 |
| 2 | CUB | -2.000 (anchor) |
| 3 | TON | -1.640 |
| 4 | KIR | -1.446 |
| 5 | TUV | -1.330 |
| 6 | NIU | -1.308 |
| 7 | FSM | -1.122 |
| 8 | NRU | -1.104 |
| 9 | BRN | -1.014 |
| 10 | MMR | -0.933 |
| 11 | KNA | -0.887 |
| 12 | WSM | -0.867 |
| 13 | COK | -0.853 |
| 14 | ERI | -0.800 |
| 15 | ATG | -0.779 |

## External Validation

| Validation | Correlation (n) |
|------------|----------------|
| UNGA ideal points | r = 0.587 (n=162) |
| V7 dim1 investment | r = 0.853 (n=165) |
| V7 dim1 human_rights | r = 0.940 (n=165) |
| V7 dim1 arms_control | r = 0.474 (n=165) |
| V7 dim1 IP | r = 0.837 (n=158) |

## Model Evolution (v4 → v7)

| Version | Key change | Rhat | UNGA r | Status |
|---------|-----------|------|--------|--------|
| v4 | V-Dem as measurement, DNK/IRN anchors | 3.19 | — | FAILED |
| v5 | Fixed tau=1, sigma_v lower bound | 2.51 | — | FAILED |
| v6 | Fixed omega=0.25, tighter sigma_d | 1.62 | 0.658 | PARTIAL |
| **v7** | **V-Dem as predictor, USA/CUB anchors** | **1.010** | **0.587** | **CONVERGED** |

## Files
- Model: `stan/hierarchical_joint_irt_v7.stan`
- Script: `scripts/R/stan_full_v7.R`
- Fit object: `outputs/stan_v7/fit_v7.rds` (334 MB, not committed)
- Stan data: `outputs/stan_v7/stan_data.rds` (620 KB)
- Summary: `outputs/stan_v7/v7_report.txt`
