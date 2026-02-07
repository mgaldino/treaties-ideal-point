# Task: Rcpp Optimization of dynIRT_KD — Codex Implementation

**IMPORTANT: Do NOT ask clarifying questions. Make reasonable choices and proceed. Use `scripts/R/v5_per_domain_2d.R` for priors/starts reference. Implement da_step and m_step at minimum. Use Rprof for profiling. Keep mclapply parallelism with ncores=4 in benchmarks. Just do the work.**

## Context

We have a K-dimensional dynamic probit IRT estimator implemented in pure R (`dynIRT_KD`). It works correctly but is too slow for large datasets (J > 800 items). We need Rcpp versions of the bottleneck functions.

**Working directory**: `/Users/manoelgaldino/Documents/DCP/Papers/Ideal point`

## Current Code (READ THESE FIRST)

The EM algorithm has 3 main steps, each in its own file:

1. `scripts/R/da_step.R` — **Data Augmentation** (E-step part 1): computes truncated normal moments E[y*|y,θ] for the probit link. Loops over items × countries × periods. **THIS IS THE MAIN BOTTLENECK** for large J.

2. `scripts/R/kalman.R` — **Kalman Filter-Smoother** (E-step part 2): forward-backward pass for each country. Already parallelized via `mclapply`. K×K matrix operations (K=2, so matrices are tiny).

3. `scripts/R/m_step.R` — **M-step**: estimates item parameters (α, β) via sufficient statistics aggregation. Loops over items.

4. `scripts/R/dynIRT_KD.R` — **Orchestrator**: EM loop, convergence checks, logging. Sources the 3 files above.

## Existing Tests

- `tests/test_da_step.R`
- `tests/test_kalman.R`
- `tests/test_m_step.R`
- `tests/test_dynIRT_KD.R`

Run with: `Rscript tests/test_da_step.R` etc.

## What To Do

### Step 1: Read and understand the current R code
Read all 4 R source files. Understand the data structures, function signatures, and inner loops.

### Step 2: Profile (quick)
Run 50 iterations of dynIRT_KD on the investment dataset and profile to confirm where time is spent:
```r
source("scripts/R/dynIRT_KD.R")
flow <- readRDS("data/processed/investment_flow_matrix.rds")
# ... set up priors, starts (see scripts/R/v5_per_domain_2d.R for reference)
# Run with maxit=50, verbose=TRUE
```

### Step 3: Implement Rcpp versions
Create Rcpp (.cpp) files in `scripts/R/rcpp_codex/`. Priority order:

1. **da_step_rcpp.cpp** — The truncated normal moments (pnorm/dnorm calls) in C++
2. **m_step_rcpp.cpp** — Sufficient statistics aggregation
3. **kalman_rcpp.cpp** — Only if time permits; the parallel mclapply may already be fast enough

Each .cpp file should export functions with the SAME interface as the R versions, so they're drop-in replacements.

Use `Rcpp::sourceCpp()` for compilation. Use RcppArmadillo for matrix operations if helpful.

### Step 4: Create wrapper
Create `scripts/R/rcpp_codex/dynIRT_KD_rcpp.R` that sources the Rcpp versions and replaces the R functions.

### Step 5: Validate
Run the existing test suite against the Rcpp versions. Results must match the pure R versions to within floating-point tolerance (max absolute diff < 1e-10).

```r
# Comparison: run both R and Rcpp on same data, same seed, compare outputs
```

### Step 6: Benchmark
Run both versions (R and Rcpp) on the investment dataset for 50 iterations. Report:
- Total time (R vs Rcpp)
- Per-step time breakdown (DA, Kalman, M-step)
- Speedup factor

Save benchmark results to `outputs/rcpp_codex_benchmark.rds` and print a summary.

## Output Files

All files in `scripts/R/rcpp_codex/`:
- `da_step_rcpp.cpp`
- `m_step_rcpp.cpp` (if implemented)
- `kalman_rcpp.cpp` (if implemented)
- `dynIRT_KD_rcpp.R` (wrapper)
- `benchmark.R` (benchmark script)

Results in `outputs/`:
- `rcpp_codex_benchmark.rds`

Log in `logs/`:
- `rcpp_codex_benchmark.log`

## Constraints

- Do **NOT** modify the original R files (`scripts/R/da_step.R`, etc.)
- Create all new files in `scripts/R/rcpp_codex/`
- The Rcpp functions must have the SAME interface (same arguments, same return structure)
- Use `RcppArmadillo` if needed (should already be installed)
- Do NOT change the algorithm — only translate R loops to C++
