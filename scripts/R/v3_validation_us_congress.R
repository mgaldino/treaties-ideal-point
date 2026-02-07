# Phase V3: US Congress Benchmark (External Validation)
# Comparison with DW-NOMINATE
# K=2 dimensions

# Redirect output to log
sink("logs/v3_validation.log", split = TRUE)

suppressPackageStartupMessages({
  library(vegan)      # For Procrustes
  library(ggplot2)
  library(data.table)
})

# Source model code
source("scripts/R/dynIRT_KD.R")

set.seed(2026)

# ---- 1. Load Data ----
data_path <- "data/processed/us_congress_v3.rds"
if (!file.exists(data_path)) stop("Data not found: ", data_path)
dat <- readRDS(data_path)

rc <- dat$rc
N <- nrow(rc)
J <- ncol(rc)
T_periods <- dat$T
K <- 2L

cat(sprintf("Loaded US Congress data: N=%d, J=%d, T=%d
", N, J, T_periods))
cat("Anchors:
")
print(dat$anchor_names)

# ---- 2. Setup Priors and Starts ----

# A. Priors
# Anchors: tight prior (var=0.01) at their prescribed positions
# Others: diffuse prior (var=1.0) at origin
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K) # Diagonal variances

anchor_idxs <- dat$anchor_leg_idx
anchor_pos  <- dat$anchor_positions

for (k in seq_along(anchor_idxs)) {
  idx <- anchor_idxs[k]
  x_mu0[idx, ]    <- anchor_pos[k, ]
  x_Sigma0[idx, ] <- 0.01
}

beta_mu <- rep(0, K + 1)
beta_sigma <- 25 * diag(K + 1)
omega_val <- 0.1 * diag(K)

# B. Starting Values (via PCA)
# Impute missing with 0 for PCA initialization
rc_pca <- rc
rc_pca[rc == 0] <- 0 # Assuming 0 is missing, but PCA needs numeric.
# Actually rc is -1, 0, 1. 0 is missing/abstain.
# For PCA, let's treat NA as 0 (mean imputation of centered data).
# But rc is integer. Convert to numeric.
rc_num <- matrix(as.numeric(rc), nrow=N, ncol=J)
# In dynIRT, 0 is missing.
# Let's simple-impute 0s with row means? Or just leave as 0 (neutral)?
# Leaving as 0 is equivalent to mean imputation if data is centered.
pca <- prcomp(rc_num, center = TRUE, scale. = FALSE)
x_start_pca <- pca$x[, 1:K]

# Rotate PCA start to align with anchors (heuristic)
# This helps convergence but isn't strictly necessary as priors will pull it.
# Skipping explicit rotation for simplicity, letting priors do the work.

# Replicate across time T
x_start <- array(NA, dim = c(N, K, T_periods))
for (t in seq_len(T_periods)) {
  x_start[, , t] <- x_start_pca
}

# Item starts
alpha_start <- numeric(J)
beta_start  <- matrix(rnorm(J * K, 0, 0.1), nrow = J, ncol = K)

# ---- 3. Run dynIRT_KD ----
cat("
Starting dynIRT_KD (K=2)...
")
t0 <- proc.time()

res <- dynIRT_KD(
  .data = list(
    rc = rc,
    startlegis = matrix(as.integer(dat$startlegis), ncol=1),
    endlegis   = matrix(as.integer(dat$endlegis), ncol=1),
    bill.session = matrix(as.integer(dat$bill.session), ncol=1),
    T = T_periods
  ),
  .starts = list(
    alpha = alpha_start,
    beta  = beta_start,
    x     = x_start
  ),
  .priors = list(
    x.mu0    = x_mu0,
    x.Sigma0 = x_Sigma0,
    beta.mu  = beta_mu,
    beta.sigma = beta_sigma,
    omega    = omega_val
  ),
  .control = list(
    threads   = 1L,
    verbose   = TRUE,
    thresh    = 1e-5,    # Slightly relaxed for V3 pilot
    maxit     = 500L,
    checkfreq = 25L,
    estimate_omega = FALSE,
    ncores    = 8L       # Parallel Kalman
  ),
  K = K
)

runtime <- (proc.time() - t0)["elapsed"]
cat(sprintf("
Estimation complete in %.1f seconds. Converged: %d
", runtime, res$runtime$conv))

# ---- 4. Compare with DW-NOMINATE ----

# Extract mean ideal point (averaged over active periods for each legislator)
x_est <- res$means$x # N x K x T
x_mean <- matrix(NA, nrow = N, ncol = K)

for (i in seq_len(N)) {
  # Average over periods where legislator was active
  s <- dat$startlegis[i] + 1
  e <- dat$endlegis[i] + 1
  if (e >= s) {
    # Take mean of x_est[i, , s:e]
    # Handle K=2 dimension drop carefully
    if (s == e) {
      x_mean[i, ] <- x_est[i, , s]
    } else {
      x_sub <- x_est[i, , s:e]
      # x_sub is K x (e-s+1) matrix
      x_mean[i, ] <- rowMeans(x_sub)
    }
  }
}

# Benchmark
dw <- dat$dw_benchmark # N x 2

# Filter out NAs if any (shouldn't be, but safety first)
valid_idx <- which(complete.cases(x_mean) & complete.cases(dw))
x_mean_valid <- x_mean[valid_idx, ]
dw_valid     <- dw[valid_idx, ]

cat(sprintf("
Comparison N=%d legislators
", length(valid_idx)))

# Procrustes Alignment
# Rotate OUR estimates (x_mean) to match DW-NOMINATE (dw)
proc_res <- vegan::procrustes(X = dw_valid, Y = x_mean_valid, scale = TRUE, symmetric = FALSE)
x_aligned <- proc_res$Yrot

# Correlations
cor_d1 <- cor(x_aligned[, 1], dw_valid[, 1])
cor_d2 <- cor(x_aligned[, 2], dw_valid[, 2])

cat(sprintf("
Correlations after Procrustes alignment:
"))
cat(sprintf("Dimension 1: %.4f (Target > 0.85)
", cor_d1))
cat(sprintf("Dimension 2: %.4f (Target > 0.50)
", cor_d2))

# ---- 5. Visuals & Save ----
output_dir <- "outputs/v3_validation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Data frame for plotting
df_plot <- data.frame(
  id = valid_idx,
  DW_Dim1 = dw_valid[, 1],
  DW_Dim2 = dw_valid[, 2],
  Est_Dim1 = x_aligned[, 1],
  Est_Dim2 = x_aligned[, 2],
  Party = as.factor(dat$party_codes[valid_idx])
)

# Plot Dim 1
p1 <- ggplot(df_plot, aes(x = DW_Dim1, y = Est_Dim1, color = Party)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Dimension 1: dynIRT vs DW-NOMINATE",
       subtitle = sprintf("Correlation = %.4f", cor_d1),
       x = "DW-NOMINATE Dim 1", y = "dynIRT Dim 1 (Aligned)")

ggsave(file.path(output_dir, "v3_dim1_comparison.png"), p1, width = 6, height = 5)

# Plot Dim 2
p2 <- ggplot(df_plot, aes(x = DW_Dim2, y = Est_Dim2, color = Party)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Dimension 2: dynIRT vs DW-NOMINATE",
       subtitle = sprintf("Correlation = %.4f", cor_d2),
       x = "DW-NOMINATE Dim 2", y = "dynIRT Dim 2 (Aligned)")

ggsave(file.path(output_dir, "v3_dim2_comparison.png"), p2, width = 6, height = 5)

# Save numerical results
results <- list(
  cor_d1 = cor_d1,
  cor_d2 = cor_d2,
  proc_rmse = proc_res$ss, # Sum of squares
  runtime = res$runtime,
  estimates = x_mean,
  aligned = x_aligned,
  benchmark = dw
)
saveRDS(results, file.path(output_dir, "v3_results.rds"))

cat(sprintf("
Results saved to %s
", output_dir))
sink()
