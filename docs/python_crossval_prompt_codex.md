# Task: Implement dynIRT_KD in Python (Cross-Validation)

**Do NOT ask questions. Make reasonable choices and proceed autonomously.**

## Working Directory
```
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
```

## Goal

Implement the K-dimensional dynamic probit IRT algorithm (dynIRT_KD) in Python using numpy/scipy. This is a cross-language validation: you will run the same algorithm on the same data as R and verify that results match numerically.

## Step 1: Read and understand the R implementation

Read these files carefully — they ARE the algorithm specification:

- `scripts/R/da_step.R` — Data augmentation step (Albert-Chib truncated normal moments)
- `scripts/R/kalman.R` — Kalman filter-smoother (forward-backward, RTS smoother)
- `scripts/R/m_step.R` — M-step (penalized WLS for item parameters, Omega update)
- `scripts/R/dynIRT_KD.R` — Main EM loop orchestrating DA + Kalman + M-step

Also read `docs/estimation_plan_2d.md` sections 5-10 for the mathematical specification.

## Step 2: Export validation data from R

Create and run `scripts/R/export_validation_data.R` that:

1. Loads `data/processed/investment_flow_matrix.rds`
2. Sets up anchors (DNK=+2,+2; IRN=-2,-2; CHN=+1,-1) and priors exactly as in the V5 scripts
3. Generates starting values with `set.seed(2026)` (PCA for x, zeros for alpha, `rnorm(J*K,0,0.1)` for beta)
4. Runs dynIRT_KD for 50 iterations (source `scripts/R/dynIRT_KD.R`) with:
   - `thresh=1e-10` (never converge early — force exactly 50 iters)
   - `estimate_omega=FALSE`, `omega=0.1*diag(K)`
   - `ncores=1` (deterministic)
5. Saves everything to `data/processed/python_validation/`:
   - `rc.csv` — N x J integer matrix
   - `startlegis.csv`, `endlegis.csv` — N-vectors (0-indexed)
   - `bill_session.csv` — J-vector (0-indexed)
   - `metadata.csv` — one row with N, J, T, K
   - `country_codes.csv`
   - `x_mu0.csv` — N x K prior means
   - `x_Sigma0_diag.csv` — N x K diagonal prior variances
   - `alpha_start.csv` — J-vector
   - `beta_start.csv` — J x K matrix
   - `x_start_t{1..T}.csv` — T files, each N x K (starting ideal points per period)
   - `beta_mu.csv`, `beta_sigma.csv`, `omega.csv` — prior parameters
   - Reference R results after 50 iterations:
     - `ref_alpha.csv`, `ref_beta.csv`, `ref_x_t{1..T}.csv`
     - `ref_loglik_trace.csv` — 50-element vector

## Step 3: Implement in Python

Create all files in `scripts/python/dynIRT_KD_codex/`:

### `da_step.py`
- `truncnorm_moments(mu, y)` → (y_star, y_star_var)
  - Use log-space computation: `scipy.stats.norm.logpdf`, `scipy.stats.norm.logcdf`
  - For y=+1: E[y*] = mu + lambda, where lambda = phi(mu)/Phi(mu) (inverse Mills ratio)
  - For y=-1: E[y*] = mu - lambda, where lambda = phi(mu)/(1-Phi(mu))
  - Clamp variance to (1e-12, 1.0]
- `da_step(rc, alpha, beta, x_smooth, bill_session)` → (y_star, y_star_var)
  - Process by period for efficiency
  - mu = x_t @ beta[j_idx].T + alpha[j_idx]
  - Apply truncnorm_moments to non-missing entries (rc != 0)

### `kalman.py`
- `kalman_smoother_country(y_star_i, item_indices, alpha, beta, bill_session, mu0, Sigma0, Omega, start_t, end_t, T_total)`
  - Forward pass: prediction (random walk) + update (block or single-item)
  - Block update: P_post = (P_pred^{-1} + H'H)^{-1}, use Cholesky when possible
  - Backward pass: RTS smoother with gain L_t = P_{t|t} @ inv(P_{t+1|t})
  - Lag-one cross-covariance: P_lag[s-1] = L[s-1] @ P_smooth[s]
  - Return x_smooth (K x T_active), P_smooth (list of K x K), P_lag (list of K x K)

### `m_step.py`
- `m_step_items(y_star, rc, x_smooth, P_smooth, bill_session, beta_mu, beta_sigma, voters_by_item=None)`
  - For each item j: accumulate sufficient statistics (sum_xx = X'X, sum_yx = X'y, P_sum)
  - Build (K+1) x (K+1) system: Sigma_zz + Sigma_beta_inv
  - Solve for gamma_hat = [alpha_j, beta_j]
- `m_step_Omega(x_smooth, P_smooth, P_lag, startlegis, endlegis, diagonal_only=False, ridge=1e-6)` (optional, for completeness)

### `dynIRT_KD.py`
- `compute_loglik(rc, alpha, beta, x_smooth, bill_session)` — observed-data log-likelihood
- `dynIRT_KD(data, starts, priors, control, K=2)` — main EM loop
  - Same interface as R version
  - Sequential Kalman (no parallelism needed for validation)
  - Support `thresh_loglik` stopping criterion
  - Return means (x, alpha, beta), loglik_trace, iters, conv

### `validate.py`
- Load R reference data from `data/processed/python_validation/`
- Load Python starting values (same as R)
- Run dynIRT_KD for exactly 50 iterations
- Compare against R reference:
  - `max|alpha_py - alpha_R|` — should be < 1e-8
  - `max|beta_py - beta_R|` — should be < 1e-8
  - `max|x_py - x_R|` — should be < 1e-8
  - `max|loglik_py[t] - loglik_R[t]|` for each iteration — should be < 1e-6
- Print per-iteration comparison table (iter, loglik_R, loglik_py, diff)
- PASS if all diffs < 1e-6, FAIL otherwise

## Step 4: Run validation

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript scripts/R/export_validation_data.R
python scripts/python/dynIRT_KD_codex/validate.py
```

## What To Report

1. Did the R export succeed? How many files?
2. Did the Python validation PASS? Show the comparison table.
3. If any diffs > 1e-6, investigate and fix.
4. Show `max|alpha_diff|`, `max|beta_diff|`, `max|x_diff|` after 50 iterations.

## Constraints

- All Python files go in `scripts/python/dynIRT_KD_codex/`
- The R export script goes in `scripts/R/export_validation_data.R`
- Do NOT modify any existing R files
- Use only numpy, scipy, pandas (standard scientific Python)
- Do NOT use rpy2 or pyreadr
- Do NOT ask questions
