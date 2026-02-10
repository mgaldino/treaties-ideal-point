# Task: Extend treaty data to 2024 and re-estimate 5 domains

Do NOT ask questions. Execute everything silently.

## Context

The project estimates 2D dynamic IRT ideal points for countries using treaty ratification data. Currently, data is truncated at 2018 (due to ATOP security dataset limit). We now want to extend the other 5 domains to 2024.

Working directory: `/Users/manoelgaldino/Documents/DCP/Papers/Ideal point`

## Step 1: Create `scripts/R/prepare_data_extended.R`

This script should be a modified version of `scripts/R/01_prepare_data.R` with these changes:

1. Change temporal parameters:
```r
year_min <- 1990
year_max <- 2024
period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)  # right boundary exclusive
period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024")
```

2. Only process 5 domains (SKIP trade and security):
```r
issue_areas <- c("investment", "environment", "human_rights", "arms_control", "intellectual_property")
```

3. Save output files with `_extended` suffix:
```r
saveRDS(flow, file.path(output_dir, paste0(area, "_flow_matrix_extended.rds")))
```

4. Keep ALL the same data loading functions (build_unctad_iia, build_ieadb, build_un_hr_treaties, build_arms_control, build_wipo_treaties). They reference `year_min` and `year_max` as globals, so changing those variables is sufficient.

5. Skip build_desta(), build_wto_rta(), build_wto_accession(), build_atop() entirely.

6. IMPORTANT: The IEADB data path uses `data/processed/ieadb/db_members.csv` and `data/processed/ieadb/db_treaties.csv` (semicolon-delimited, skip=3). These files exist and are current.

7. Also save extended baseline_events and item_codebook:
```r
readr::write_csv(all_events, file.path(output_dir, "baseline_events_extended.csv"))
readr::write_csv(all_items, file.path(output_dir, "item_codebook_extended.csv"))
```

## Step 2: Run the data preparation

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript scripts/R/prepare_data_extended.R
```

Verify that 5 flow matrices were created:
```bash
ls -la data/processed/*_flow_matrix_extended.rds
```

Print dimensions for each:
```r
for (d in c("investment","environment","human_rights","arms_control","intellectual_property")) {
  f <- file.path("data/processed", paste0(d, "_flow_matrix_extended.rds"))
  m <- readRDS(f)
  cat(sprintf("%s: N=%d, J=%d, T=%d, periods=%s\n", d, nrow(m$rc), ncol(m$rc), m$T, paste(m$period_labels, collapse=", ")))
}
```

## Step 3: Create `scripts/R/v8_estimate_extended.R`

This script estimates 2D dynamic IRT for the 5 extended domains. Pattern it after existing estimation scripts (e.g., `scripts/R/v7_investment_country_anchors.R`).

Key parameters:
- K = 2 (two dimensions)
- Use the SAME V7 anchors per domain:
  - investment: DNK(+2,0), IRN(-2,0), CHN(0,-2)
  - environment: DNK(+2,0), SAU(-2,0), AUS(0,-2)
  - human_rights: DNK(+2,0), PRK(-2,0), USA(0,-2)
  - arms_control: NZL(+2,0), ISR(-2,0), IND(0,-2)
  - intellectual_property: DNK(+2,0), AGO(-2,0), BRA(0,-2)
- Anchor sigma = 0.01 (tight prior)
- Load from `data/processed/{domain}_flow_matrix_extended.rds`
- T will now be 7 (not 6)

Estimation settings (same as V7):
```r
.priors = list(
  x.mu0 = x_mu0,
  x.Sigma0 = x_Sigma0,
  beta.mu = rep(0, K + 1),
  beta.sigma = 25 * diag(K + 1),
  omega = 0.1 * diag(K)
),
.control = list(
  verbose = TRUE,
  thresh = 1e-4,
  maxit = 5000L,
  checkfreq = 50L,
  estimate_omega = FALSE,
  thresh_loglik = 0.01,
  loglik_patience = 5L,
  ncores = 4L
),
K = K
```

PCA initialization (same pattern as V7):
```r
rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA
rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) {
  m <- mean(rc_num[, j], na.rm = TRUE)
  if (is.nan(m)) m <- 0.5
  rc_num[is.na(rc_num[, j]), j] <- m
}
col_var <- apply(rc_num, 2, var)
rc_pca <- if (any(col_var == 0, na.rm = TRUE)) rc_num[, col_var > 0, drop = FALSE] else rc_num
pca <- prcomp(rc_pca, center = TRUE, scale. = FALSE)
npc <- min(K, ncol(pca$x))
x_pca <- pca$x[, seq_len(npc), drop = FALSE]
if (npc < K) x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
x_start <- array(NA_real_, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) x_start[, , t] <- x_pca
```

Source the model:
```r
rcpp_ok <- tryCatch({
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
  TRUE
}, error = function(e) {
  cat("Rcpp failed, falling back to pure R\n")
  source("scripts/R/dynIRT_KD.R")
  FALSE
})
```

Save results to `outputs/v8_extended/{domain}_results.rds` with this structure:
```r
out <- list(
  domain = domain,
  strategy = "v8_extended_2024",
  ideal_points = res$means$x,        # N x K x T array
  ideal_points_mean = x_mean,         # N x K matrix (mean across active periods)
  alpha = res$means$alpha,
  beta = res$means$beta,
  country_codes = flow$country_codes,
  item_labels = flow$item_labels,
  period_labels = flow$period_labels,
  anchors = list(iso = anchor_iso, positions = anchor_positions, sigma = 0.01),
  aggregate = agg,                    # data.frame with period, mean_dim1, sd_dim1, mean_dim2, sd_dim2, period_label
  runtime = list(seconds = elapsed, iters = res$runtime$iters, conv = res$runtime$conv, loglik_trace = res$runtime$loglik_trace)
)
```

Compute x_mean the same way:
```r
sl <- as.integer(flow$startlegis)
el <- as.integer(flow$endlegis)
x_mean <- matrix(NA_real_, N, K)
for (i in seq_len(N)) {
  s <- sl[i] + 1L
  e <- el[i] + 1L
  if (e >= s && s >= 1 && e <= T_periods) {
    x_mean[i, ] <- if (s == e) res$means$x[i, , s] else rowMeans(res$means$x[i, , s:e])
  }
}
rownames(x_mean) <- flow$country_codes
colnames(x_mean) <- paste0("dim", seq_len(K))
```

Loop over all 5 domains. Print convergence info after each.

## Step 4: Run the estimation

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript scripts/R/v8_estimate_extended.R
```

## Step 5: Compare V7 (T=6) vs V8 (T=7) trends

After estimation, create `scripts/R/v8_compare_trends.R` that:
1. Loads V7 results from `outputs/v7_country_anchors/{domain}_results.rds`
2. Loads V8 results from `outputs/v8_extended/{domain}_results.rds`
3. For each domain, computes correlation of dim1 ideal points between V7 and V8 for the overlapping periods (periods 1-6)
4. Prints a comparison table showing:
   - Domain
   - V7 trend slope (dim1 mean across periods 1-6)
   - V8 trend slope (dim1 mean across periods 1-7)
   - Correlation for overlapping periods
   - V8 period 7 (2020-2024) mean_dim1 value
5. Saves the comparison to `outputs/v8_extended/v7_vs_v8_comparison.csv`

Run this script:
```bash
Rscript scripts/R/v8_compare_trends.R
```

## Output

Print a final summary showing for each domain:
- Convergence status
- Number of iterations
- Time elapsed
- V7 vs V8 correlation for overlapping periods
- The new 2020-2024 period mean dim1 value
- Whether the trend continues, reverses, or accelerates
