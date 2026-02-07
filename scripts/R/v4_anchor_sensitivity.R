# Phase V4: Anchor Sensitivity
# Comparison of different anchor sets on US Congress data

# Redirect output to log
sink("logs/v4_sensitivity.log", split = TRUE)

suppressPackageStartupMessages({
  library(vegan)
  library(ggplot2)
})

source("scripts/R/dynIRT_KD.R")

set.seed(2026)

# ---- 1. Setup Data ----
dat <- readRDS("data/processed/us_congress_v3.rds")
rc <- dat$rc
N <- nrow(rc); J <- ncol(rc); T_periods <- dat$T; K <- 2L

# Helper to find index by name (partial match)
find_idx <- function(pattern) grep(pattern, dat$legislator_names)

# Define Anchor Sets
anchor_sets <- list(
  baseline = list(
    names = c("WELLSTONE", "COBURN", "CHAFEE, Lincoln"),
    pos   = rbind(c(-2, -1.3), c(2, 0.2), c(-0.3, -2))
  ),
  alt1 = list(
    names = c("WELLSTONE", "COBURN", "NELSON, Earl Benjamin"),
    pos   = rbind(c(-2, -1.3), c(2, 0.2), c(0, 2)) # Nelson is on the other side of D2
  ),
  alt2 = list(
    names = c("FEINGOLD", "HELMS", "HOLLINGS"),
    pos   = rbind(c(-2, -1), c(2, 1), c(-0.5, 2)) # Southern Dem Hollings as 3rd anchor
  )
)

# Run Estimation for each set
run_results <- list()

for (s_name in names(anchor_sets)) {
  cat(sprintf("
--- Running Anchor Set: %s ---
", s_name))
  aset <- anchor_sets[[s_name]]
  
  idxs <- sapply(aset$names, find_idx)
  # Check if we got exactly one match for each
  if (any(lengths(idxs) != 1)) {
    print(idxs)
    stop("Ambiguous or missing anchor name in set ", s_name)
  }
  idxs <- as.integer(idxs)
  
  x_mu0 <- matrix(0, N, K)
  x_Sigma0 <- matrix(1, N, K)
  for (k in 1:3) {
    x_mu0[idxs[k], ] <- aset$pos[k, ]
    x_Sigma0[idxs[k], ] <- 0.01
  }
  
  # Starts (simple 0 start for robustness test)
  x_start <- array(0, dim = c(N, K, T_periods))
  
  res <- dynIRT_KD(
    .data = list(rc=rc, startlegis=dat$startlegis, endlegis=dat$endlegis, bill.session=dat$bill.session, T=T_periods),
    .starts = list(alpha=numeric(J), beta=matrix(rnorm(J*K, 0, 0.1), J, K), x=x_start),
    .priors = list(x.mu0=x_mu0, x.Sigma0=x_Sigma0, beta.mu=rep(0, K+1), beta.sigma=25*diag(K+1), omega=0.1*diag(K)),
    .control = list(verbose=FALSE, thresh=5e-5, maxit=300L, ncores=8L),
    K = K
  )
  
  # Extract mean x
  x_mean <- matrix(NA, N, K)
  for (i in 1:N) {
    s <- dat$startlegis[i]+1; e <- dat$endlegis[i]+1
    if (e >= s) x_mean[i, ] <- if(s==e) res$means$x[i,,s] else rowMeans(res$means$x[i,,s:e])
  }
  
  run_results[[s_name]] <- list(x_mean = x_mean, iters = res$runtime$iters)
  cat(sprintf("Done in %d iterations.
", res$runtime$iters))
}

# ---- 2. Comparisons ----
cat("
===== SENSITIVITY ANALYSIS (Procrustes) =====
")

compare_runs <- function(base_res, comp_res, base_name, comp_name) {
  valid <- which(complete.cases(base_res) & complete.cases(comp_res))
  p <- vegan::procrustes(X = base_res[valid, ], Y = comp_res[valid, ], scale = TRUE)
  
  # Rotated correlation
  cor_d1 <- cor(p$Yrot[, 1], base_res[valid, 1])
  cor_d2 <- cor(p$Yrot[, 2], base_res[valid, 2])
  
  cat(sprintf("
%s vs %s:
", base_name, comp_name))
  cat(sprintf("  Procrustes R^2 (1 - SS/var): %.4f
", 1 - p$ss))
  cat(sprintf("  Cor D1: %.4f
", cor_d1))
  cat(sprintf("  Cor D2: %.4f
", cor_d2))
  
  return(list(r2 = 1 - p$ss, cor1 = cor_d1, cor2 = cor_d2))
}

c1 <- compare_runs(run_results$baseline$x_mean, run_results$alt1$x_mean, "Baseline", "Alt1")
c2 <- compare_runs(run_results$baseline$x_mean, run_results$alt2$x_mean, "Baseline", "Alt2")
c3 <- compare_runs(run_results$alt1$x_mean, run_results$alt2$x_mean, "Alt1", "Alt2")

# ---- 3. Conclusions ----
pass <- (c1$cor1 > 0.85 && c2$cor1 > 0.85)
cat(sprintf("
Final Verdict: %s
", if(pass) "V4 PASSED (Robust)" else "V4 FAILED (Sensitive)"))

output_dir <- "outputs/v4_sensitivity"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(results = run_results, comparisons = list(c1, c2, c3)), file.path(output_dir, "v4_results.rds"))

sink()
