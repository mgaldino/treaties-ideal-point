# V7 Estimation Prompt Template

**Usage**: Replace `{DOMAIN}`, `{DIM1_POS}`, `{DIM1_NEG}`, `{DIM2_ANCHOR}`, `{DIM2_POS}` with domain-specific values from the table below.

## Domain Parameters

| Domain | {DOMAIN} | {DIM1_POS} | {DIM1_NEG} | {DIM2_ANCHOR} | {DIM2_POS} |
|--------|----------|------------|------------|---------------|------------|
| Investment | investment | DNK | IRN | CHN | c(0,-2) |
| Security | security | DNK | IRN | UKR | c(0,-2) |
| Environment | environment | DNK | SAU | AUS | c(0,-2) |
| Human Rights | human_rights | DNK | PRK | USA | c(0,-2) |
| Arms Control | arms_control | NZL | ISR | IND | c(0,-2) |
| IP | intellectual_property | DNK | AGO | BRA | c(0,-2) |

---

## Prompt (copy and parametrize)

**Do NOT ask questions. Just execute.**

Working directory: `cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"`

You will estimate a 2-dimensional dynamic IRT model for the **{DOMAIN}** domain using two identification strategies: (A) country anchors with substantive dim2 selection, and (B) item anchors. Then compare.

### PART A — Country-Anchor Estimation

Create and run `scripts/R/v7_{DOMAIN}_country_anchors.R`:

```r
#!/usr/bin/env Rscript
set.seed(2026)
source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")

K <- 2L
domain <- "{DOMAIN}"
flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

# V7 substantive anchors
anchor_iso <- c("{DIM1_POS}", "{DIM1_NEG}", "{DIM2_ANCHOR}")
anchor_idx <- match(anchor_iso, flow$country_codes)
if (any(is.na(anchor_idx))) {
  missing <- anchor_iso[is.na(anchor_idx)]
  stop(sprintf("Anchor countries not found in flow matrix: %s", paste(missing, collapse=", ")))
}
anchor_positions <- rbind(
  c(+2,  0),   # {DIM1_POS}: dim1 positive (pro-ILO)
  c(-2,  0),   # {DIM1_NEG}: dim1 negative (anti-ILO)
  {DIM2_POS}   # {DIM2_ANCHOR}: dim2 anchor (confounder axis)
)

x_mu0 <- matrix(0, nrow=N, ncol=K)
x_Sigma0 <- matrix(1, nrow=N, ncol=K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

# PCA initialization
rc_num <- matrix(as.numeric(flow$rc), nrow=N, ncol=J)
rc_num[rc_num == 0] <- NA; rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) {
  m <- mean(rc_num[,j], na.rm=TRUE)
  if (is.nan(m)) m <- 0.5
  rc_num[is.na(rc_num[,j]), j] <- m
}
# Drop zero-variance columns before PCA
col_var <- apply(rc_num, 2, var)
rc_pca <- if (any(col_var == 0)) rc_num[, col_var > 0] else rc_num
pca <- prcomp(rc_pca, center=TRUE, scale.=FALSE)
x_pca <- pca$x[, 1:K]
for (k in 1:K) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
x_start <- array(NA_real_, dim=c(N, K, T_periods))
for (t in 1:T_periods) x_start[,,t] <- x_pca

dir.create("outputs/v7_country_anchors", recursive=TRUE, showWarnings=FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="UTC")
log_file <- sprintf("logs/v7_%s_country_%s.log", domain, ts)
sink(log_file, split=TRUE)
cat(sprintf("V7 Country-Anchor: %s | N=%d, J=%d, T=%d, K=%d\n", domain, N, J, T_periods, K))
cat(sprintf("Anchors: %s (%s)\n", paste(anchor_iso, collapse=", "),
    paste(apply(anchor_positions, 1, function(r) sprintf("(%+.0f,%+.0f)", r[1], r[2])), collapse=", ")))
cat(sprintf("Rationale: dim1=ILO support, dim2=confounder separation\n\n"))

t0 <- proc.time()
res <- dynIRT_KD(
  .data = list(rc=flow$rc,
               startlegis=matrix(as.integer(flow$startlegis), ncol=1),
               endlegis=matrix(as.integer(flow$endlegis), ncol=1),
               bill.session=matrix(as.integer(flow$bill.session), ncol=1),
               T=T_periods),
  .starts = list(alpha=numeric(J),
                 beta=matrix(rnorm(J*K, 0, 0.1), J, K),
                 x=x_start),
  .priors = list(x.mu0=x_mu0, x.Sigma0=x_Sigma0,
                 beta.mu=rep(0, K+1), beta.sigma=25*diag(K+1),
                 omega=0.1*diag(K)),
  .control = list(verbose=TRUE, thresh=1e-4, maxit=5000L, checkfreq=50L,
                  estimate_omega=FALSE, thresh_loglik=0.01, loglik_patience=5L,
                  ncores=4L),
  K=K
)
elapsed_A <- (proc.time() - t0)["elapsed"]
cat(sprintf("\nPart A done: %.1fs | Iters: %d | Conv: %d\n",
    elapsed_A, res$runtime$iters, res$runtime$conv))

# Compute mean ideal points across active periods
sl <- as.integer(flow$startlegis); el <- as.integer(flow$endlegis)
x_mean <- matrix(NA_real_, N, K)
for (i in 1:N) {
  s <- sl[i] + 1; e <- el[i] + 1
  if (e >= s && s >= 1 && e <= T_periods) {
    x_mean[i,] <- if (s == e) res$means$x[i,,s] else rowMeans(res$means$x[i,,s:e])
  }
}
rownames(x_mean) <- flow$country_codes

# Aggregate statistics per period (for paper)
agg <- data.frame(period = 1:T_periods,
                  mean_dim1 = sapply(1:T_periods, function(t) mean(res$means$x[,,t][,1], na.rm=TRUE)),
                  sd_dim1   = sapply(1:T_periods, function(t) sd(res$means$x[,,t][,1], na.rm=TRUE)),
                  mean_dim2 = sapply(1:T_periods, function(t) mean(res$means$x[,,t][,2], na.rm=TRUE)),
                  sd_dim2   = sapply(1:T_periods, function(t) sd(res$means$x[,,t][,2], na.rm=TRUE)))
if (!is.null(flow$period_labels)) agg$period_label <- flow$period_labels
cat("\nAggregate trends:\n"); print(agg)

out_A <- list(domain=domain, strategy="country_anchors_v7",
              ideal_points=res$means$x, ideal_points_mean=x_mean,
              alpha=res$means$alpha, beta=res$means$beta,
              country_codes=flow$country_codes, item_labels=flow$item_labels,
              period_labels=flow$period_labels,
              anchors=list(iso=anchor_iso, positions=anchor_positions,
                           rationale="dim1=ILO, dim2=confounder"),
              aggregate=agg,
              runtime=list(seconds=elapsed_A, iters=res$runtime$iters,
                           conv=res$runtime$conv,
                           loglik_trace=res$runtime$loglik_trace))
saveRDS(out_A, sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
cat(sprintf("Saved: outputs/v7_country_anchors/%s_results.rds\n", domain))
sink()
```

Run: `Rscript scripts/R/v7_{DOMAIN}_country_anchors.R`

### PART B — Item-Anchor Estimation

After Part A completes, create and run `scripts/R/v7_{DOMAIN}_item_anchors.R`.

This script must:

1. Load Part A results from `outputs/v7_country_anchors/{DOMAIN}_results.rds`
2. Select 3 anchor items from estimated betas:
   - Item A: highest |beta_1| / (|beta_2| + 0.01) → dim1 specialist
   - Item B: highest |beta_2| / (|beta_1| + 0.01) → dim2 specialist
   - Item C: highest |beta_1| * |beta_2| (excluding A and B) → both-loader
3. Source components: `scripts/R/rcpp_merged/dynIRT_KD_rcpp.R`, `scripts/R/da_step.R`, `scripts/R/kalman.R`, `scripts/R/m_step.R`. If `compute_loglik` is not available, also source `scripts/R/dynIRT_KD.R`.
4. Run a custom EM loop:
   - Diffuse priors for ALL countries (x_Sigma0 = 1, no country anchors)
   - Warm-start alpha/beta from Part A
   - After each M-step, **overwrite anchor item betas and alphas** back to their fixed Part A values
   - Convergence: thresh_loglik=0.01, patience=5, checkfreq=50, maxit=5000
5. Compute mean ideal points and aggregate statistics
6. Save to `outputs/v7_item_anchors/{DOMAIN}_results.rds`

**Important**: The custom EM loop calls `da_step()`, then Kalman smoother per country (using `kalman_smoother_country()` or the parallel `.kalman_one_country()` helper if available), then `m_step_items()`, then enforces fixed betas. Check what functions are actually exported by the sourced files and adapt accordingly. If `.kalman_one_country` is not found, implement the Kalman loop manually over countries.

Run: `Rscript scripts/R/v7_{DOMAIN}_item_anchors.R`

### PART C — Comparison

After both parts, create and run `scripts/R/v7_{DOMAIN}_compare.R`:

1. Load both results
2. Compute dim1/dim2 ideal point correlations between A and B (handle sign flips: max of |r| and |-r|)
3. Compute cross-dimension correlations (dim1_A vs dim2_B and vice versa) to check for rotation
4. Compute alpha and beta correlations
5. List top-5 and bottom-5 countries on dim1 and dim2 (country-anchor version)
6. Save report to `outputs/v7_comparison/{DOMAIN}_report.txt`

Run: `Rscript scripts/R/v7_{DOMAIN}_compare.R`

### What To Report

1. Part A: converged? iterations? runtime? aggregate trend table
2. Part B: which anchor items selected (names + betas)? converged? iterations? runtime?
3. Part C: full comparison report text
4. Any errors

### Constraints

- Do NOT modify files in `scripts/R/rcpp_merged/`, `scripts/R/da_step.R`, `scripts/R/kalman.R`, `scripts/R/m_step.R`, or `scripts/R/dynIRT_KD.R`
- Do NOT ask questions
- If Rcpp compilation fails, fall back to `source("scripts/R/dynIRT_KD.R")` (pure R, slower but functional)
- Verify anchor countries exist in the flow matrix before proceeding
