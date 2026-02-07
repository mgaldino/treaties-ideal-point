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

## Step 2: Use exported validation data

The validation data has already been exported by another agent to `data/processed/python_validation/`. It contains:

- `rc.csv` — N x J integer matrix (votes: +1, -1, 0=missing)
- `startlegis.csv`, `endlegis.csv` — N-vectors (0-indexed period boundaries)
- `bill_session.csv` — J-vector (0-indexed period per item)
- `metadata.csv` — one row with N, J, T, K
- `country_codes.csv`
- `x_mu0.csv` — N x K prior means
- `x_Sigma0_diag.csv` — N x K diagonal prior variances
- `alpha_start.csv` — J-vector of starting intercepts
- `beta_start.csv` — J x K matrix of starting discriminations
- `x_start_t{1..T}.csv` — T files, each N x K (starting ideal points per period)
- `beta_mu.csv`, `beta_sigma.csv`, `omega.csv` — prior parameters
- Reference R results after 50 iterations:
  - `ref_alpha.csv`, `ref_beta.csv`, `ref_x_t{1..T}.csv`
  - `ref_loglik_trace.csv` — 50-element log-likelihood trace

If these files do NOT exist yet, create and run this R export script first:

```r
#!/usr/bin/env Rscript
# Export validation data for Python cross-validation
set.seed(2026)

source("scripts/R/dynIRT_KD.R")

domain <- "investment"
flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T; K <- 2L

anchor_iso <- c("DNK", "IRN", "CHN")
anchor_idx <- match(anchor_iso, flow$country_codes)
anchor_positions <- rbind(c(+2, +2), c(-2, -2), c(+1, -1))

x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA; rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) { m <- mean(rc_num[,j], na.rm=TRUE); if(is.nan(m)) m <- 0.5; rc_num[is.na(rc_num[,j]),j] <- m }
pca <- prcomp(rc_num, center=TRUE, scale.=FALSE)
x_pca <- pca$x[,1:K]; for(k in 1:K) x_pca[,k] <- as.numeric(scale(x_pca[,k]))
x_start <- array(NA_real_, dim=c(N,K,T_periods)); for(t in 1:T_periods) x_start[,,t] <- x_pca

alpha_start <- numeric(J)
beta_start <- matrix(rnorm(J*K, 0, 0.1), J, K)

# Run R for 50 iterations (no early stopping)
res <- dynIRT_KD(
  .data = list(rc=flow$rc, startlegis=matrix(as.integer(flow$startlegis),ncol=1),
               endlegis=matrix(as.integer(flow$endlegis),ncol=1),
               bill.session=matrix(as.integer(flow$bill.session),ncol=1), T=T_periods),
  .starts = list(alpha=alpha_start, beta=beta_start, x=x_start),
  .priors = list(x.mu0=x_mu0, x.Sigma0=x_Sigma0, beta.mu=rep(0,K+1),
                 beta.sigma=25*diag(K+1), omega=0.1*diag(K)),
  .control = list(verbose=TRUE, thresh=1e-10, maxit=50L, checkfreq=1L,
                  estimate_omega=FALSE, ncores=1L),
  K=K
)

# Save
outdir <- "data/processed/python_validation"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

write.csv(flow$rc, file.path(outdir, "rc.csv"), row.names=FALSE)
write.csv(data.frame(startlegis=as.integer(flow$startlegis)), file.path(outdir, "startlegis.csv"), row.names=FALSE)
write.csv(data.frame(endlegis=as.integer(flow$endlegis)), file.path(outdir, "endlegis.csv"), row.names=FALSE)
write.csv(data.frame(bill_session=as.integer(flow$bill.session)), file.path(outdir, "bill_session.csv"), row.names=FALSE)
write.csv(data.frame(N=N, J=J, T_periods=T_periods, K=K), file.path(outdir, "metadata.csv"), row.names=FALSE)
write.csv(data.frame(country_codes=flow$country_codes), file.path(outdir, "country_codes.csv"), row.names=FALSE)
write.csv(x_mu0, file.path(outdir, "x_mu0.csv"), row.names=FALSE)
write.csv(x_Sigma0, file.path(outdir, "x_Sigma0_diag.csv"), row.names=FALSE)
write.csv(data.frame(alpha=alpha_start), file.path(outdir, "alpha_start.csv"), row.names=FALSE)
write.csv(beta_start, file.path(outdir, "beta_start.csv"), row.names=FALSE)
for (t in 1:T_periods) write.csv(x_start[,,t], file.path(outdir, sprintf("x_start_t%d.csv", t)), row.names=FALSE)
write.csv(data.frame(beta_mu=rep(0,K+1)), file.path(outdir, "beta_mu.csv"), row.names=FALSE)
write.csv(25*diag(K+1), file.path(outdir, "beta_sigma.csv"), row.names=FALSE)
write.csv(0.1*diag(K), file.path(outdir, "omega.csv"), row.names=FALSE)

# Reference results
write.csv(data.frame(alpha=res$means$alpha), file.path(outdir, "ref_alpha.csv"), row.names=FALSE)
write.csv(res$means$beta, file.path(outdir, "ref_beta.csv"), row.names=FALSE)
for (t in 1:T_periods) write.csv(res$means$x[,,t], file.path(outdir, sprintf("ref_x_t%d.csv", t)), row.names=FALSE)
write.csv(data.frame(loglik=res$runtime$loglik_trace), file.path(outdir, "ref_loglik_trace.csv"), row.names=FALSE)

cat(sprintf("Exported: N=%d, J=%d, T=%d, K=%d\n", N, J, T_periods, K))
```

## Step 3: Implement in Python

Create all files in `scripts/python/dynIRT_KD_gemini/`:

### `da_step.py`
- `truncnorm_moments(mu, y)` → (y_star, y_star_var)
  - Use log-space: `scipy.stats.norm.logpdf` + `scipy.stats.norm.logcdf`
  - y=+1: E[y*] = mu + phi(mu)/Phi(mu) (inverse Mills ratio)
  - y=-1: E[y*] = mu - phi(mu)/(1-Phi(mu))
  - Var clamped to (1e-12, 1.0]
- `da_step(rc, alpha, beta, x_smooth, bill_session)` → (y_star, y_star_var)
  - Process by period. mu = x_t @ beta[j_idx].T + alpha[j_idx]

### `kalman.py`
- `kalman_smoother_country(y_star_i, item_indices, alpha, beta, bill_session, mu0, Sigma0, Omega, start_t, end_t, T_total)`
  - Forward: prediction (random walk) + block update (P_post = inv(inv(P_pred) + H'H))
  - Backward: RTS smoother, L_t = P_filt[t] @ inv(P_pred[t+1])
  - P_lag[s-1] = L[s-1] @ P_smooth[s]
  - Returns x_smooth (K x T_active), P_smooth, P_lag

### `m_step.py`
- `m_step_items(y_star, rc, x_smooth, P_smooth, bill_session, beta_mu, beta_sigma, voters_by_item=None)`
  - Per item: sum_xx = X'X, sum_yx = X'y, P_sum = sum of P_smooth[i][t_j]
  - Sigma_zz (K+1 x K+1), solve (Sigma_beta_inv + Sigma_zz)^{-1} (Sigma_beta_inv @ mu + Sigma_zy)
- `m_step_Omega(...)` for completeness

### `dynIRT_KD.py`
- `compute_loglik(rc, alpha, beta, x_smooth, bill_session)` — log Phi(mu) for pos, log Phi(-mu) for neg
- `dynIRT_KD(data, starts, priors, control, K=2)` — main EM loop
  - Sequential Kalman (no multiprocessing)
  - Support thresh_loglik stopping
  - Return means, loglik_trace, runtime info

### `validate.py`
- Load data from `data/processed/python_validation/`
- Run Python dynIRT_KD for 50 iterations with same starts
- Compare:
  - `max|alpha_py - alpha_R|` < 1e-8
  - `max|beta_py - beta_R|` < 1e-8
  - `max|x_py - x_R|` < 1e-8
  - Per-iteration loglik diff < 1e-6
- Print comparison table
- Report PASS/FAIL

## Step 4: Run validation

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
# Only run the R export if data/processed/python_validation/ doesn't exist
[ -d data/processed/python_validation ] || Rscript scripts/R/export_validation_data.R
python scripts/python/dynIRT_KD_gemini/validate.py
```

## What To Report

1. Did validation data exist or did you need to run the R export?
2. Did the Python validation PASS? Show the comparison table.
3. Show `max|alpha_diff|`, `max|beta_diff|`, `max|x_diff|` after 50 iterations.

## Constraints

- All Python files go in `scripts/python/dynIRT_KD_gemini/`
- Do NOT modify any existing R files or the export script if it already exists
- Use only numpy, scipy, pandas
- Do NOT use rpy2 or pyreadr
- Do NOT ask questions
