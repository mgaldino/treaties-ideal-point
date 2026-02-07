# Task: Consolidate Optimal Rcpp Implementation

**CRITICAL: Do NOT ask questions. Make reasonable choices and proceed autonomously. Do NOT stop to ask for clarification. Just do the work.**

## Context

Two independent Rcpp implementations of `dynIRT_KD` were created:
- `scripts/R/rcpp_gemini/` — 1.81x speedup
- `scripts/R/rcpp_codex/` — 1.10x speedup

Analysis showed the performance gap comes from ONE design decision in the M-step: how `P_smooth` (an R List of Lists of K×K matrices) is accessed.

## What Made Gemini Faster

Gemini **pre-flattens P_smooth** into a contiguous C++ vector at the START of `m_step_items_rcpp`:

```cpp
std::vector<arma::mat> P_flat(T * N);
for (int i = 0; i < N; ++i) {
    List Pi = P_smooth[i];
    for (int t = 0; t < T; ++t) {
        P_flat[t * N + i] = as<arma::mat>(Pi[t]);
    }
}
// Then in the item loop: P_flat[t * N + i] — direct C++ access
```

This does N×T = ~1,200 R SEXP accesses once, vs Codex's ~105,000 per iteration (per voter per item).

## What Made Codex Better (Compute)

Codex uses **batch matrix operations** for sufficient statistics:

```cpp
arma::mat X_t = X.slice(t_index).rows(idx); // all voters at once
arma::mat sum_xx = X_t.t() * X_t;           // BLAS matmul
arma::vec sum_yx = X_t.t() * Y;             // BLAS matvec
```

Gemini does scalar accumulation (one voter at a time), which is slower for the compute part.

Codex also has better input validation and supports `voters_by_item`.

## Your Task

Create `scripts/R/rcpp_merged/` with a consolidated implementation that combines:

1. **Gemini's P_smooth pre-flattening** in `m_step_items_rcpp` (the key speedup)
2. **Codex's batch matmul** for sufficient statistics (`X_t.t() * X_t`, `X_t.t() * Y`)
3. **Codex's input validation** (infer dims from x_smooth attributes, check sizes)
4. **Codex's `voters_by_item` support** (optional parameter)
5. **DA step**: take either version (they're functionally identical). Codex's is slightly more robust (dim inference).

### Files to Create

All in `scripts/R/rcpp_merged/`:

- `da_step_rcpp.cpp` — DA step (either version, they're the same algorithm)
- `m_step_rcpp.cpp` — M-step with pre-flattened P_smooth + batch matmul
- `dynIRT_KD_rcpp.R` — Wrapper that sources originals + overrides with Rcpp. Include the macOS gfortran fix from Gemini's version.
- `benchmark.R` — Run R pure vs merged Rcpp on investment dataset, 50 iterations, ncores=4. Report total time and per-step breakdown (DA/Kalman/M). Use Codex's timing wrapper approach (wrap mclapply, wrap da_step, wrap m_step_items).
- `validate.R` — Compare merged Rcpp vs pure R: max absolute diff must be < 1e-10.

### Reference Files to Read

- `scripts/R/rcpp_gemini/m_step_rcpp.cpp` — P_smooth pre-flattening (lines 36-44)
- `scripts/R/rcpp_codex/m_step_rcpp.cpp` — Batch matmul + voters_by_item
- `scripts/R/rcpp_codex/da_step_rcpp.cpp` — DA step with dim inference
- `scripts/R/rcpp_codex/benchmark.R` — Per-step timing approach
- `scripts/R/rcpp_gemini/dynIRT_KD_rcpp.R` — macOS gfortran fix
- `scripts/R/m_step.R` — Original R M-step (to understand the interface)
- `scripts/R/da_step.R` — Original R DA step

### Expected Result

Speedup should be >= 1.81x (at least as good as Gemini, potentially better with batch matmul). If the benchmark shows < 1.5x, something is wrong — debug it.

### Constraints

- Do NOT modify original R files in `scripts/R/`
- Do NOT modify `scripts/R/rcpp_gemini/` or `scripts/R/rcpp_codex/`
- All new files go in `scripts/R/rcpp_merged/`
- Run benchmark and validation. Report results.
- Save benchmark to `outputs/rcpp_merged_benchmark.rds`
