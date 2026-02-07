# Rcpp Optimization Plan for dynIRT_KD (Codex)

## Phase 1: Baseline Understanding and Profiling
- Read core R sources (`da_step.R`, `kalman.R`, `m_step.R`, `dynIRT_KD.R`).
- Capture priors/starts setup from `scripts/R/v5_per_domain_2d.R`.
- Create and run a profiling script using `Rprof` for 50 iterations on the investment dataset.
- Record timing breakdown for DA / Kalman / M-step.

## Phase 2: Rcpp Implementation (DA + M-step)
- Implement `da_step_rcpp.cpp` with numerically stable truncated normal moments.
- Implement `m_step_rcpp.cpp` with sufficient statistics aggregation.
- Create `dynIRT_KD_rcpp.R` wrapper to compile and swap R functions.

## Phase 3: Validation and Benchmarks
- Run comparison scripts to ensure Rcpp matches R within tolerance (max abs diff < 1e-10).
- Run existing tests against the Rcpp versions (via a custom runner).
- Benchmark R vs Rcpp for 50 iterations (ncores=4) with per-step timing.
- Save results and log files to `outputs/` and `logs/`.
