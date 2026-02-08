#!/usr/bin/env Rscript
set.seed(2026)

# Source Rcpp-merged version (loads dynIRT_KD.R internally, overrides da_step & m_step_items with Rcpp)
source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")

K <- 2L
domain <- "security"
flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

# Load Part A results to select anchor items
res_A <- readRDS(sprintf("outputs/v6_country_anchors/%s_results.rds", domain))
beta_A <- res_A$beta  # J x K

# Select 3 anchor items:
# Item with highest |beta_1|/|beta_2| ratio -> dim1 loader
# Item with highest |beta_2|/|beta_1| ratio -> dim2 loader
# Item with highest |beta_1|*|beta_2| -> both-dimension loader
ratio1 <- abs(beta_A[,1]) / (abs(beta_A[,2]) + 0.01)
ratio2 <- abs(beta_A[,2]) / (abs(beta_A[,1]) + 0.01)
product <- abs(beta_A[,1]) * abs(beta_A[,2])

anchor_items <- c(
  which.max(ratio1),   # dim1 specialist
  which.max(ratio2),   # dim2 specialist
  which.max(product)   # both-dimension loader
)
# Ensure all unique
if(length(unique(anchor_items)) < 3) {
  # fallback: top 3 by total |beta|
  total_beta <- rowSums(abs(beta_A))
  anchor_items <- order(total_beta, decreasing=TRUE)[1:3]
}
fixed_betas <- beta_A[anchor_items, , drop=FALSE]
fixed_alpha <- res_A$alpha[anchor_items]

cat(sprintf("Anchor items: %s\n", paste(anchor_items, collapse=", ")))
cat(sprintf("Item labels: %s\n", paste(flow$item_labels[anchor_items], collapse=", ")))
cat(sprintf("Fixed betas:\n"))
print(fixed_betas)

# PCA starts (same as Part A)
rc_num <- matrix(as.numeric(flow$rc), nrow = N, ncol = J)
rc_num[rc_num == 0] <- NA; rc_num[rc_num == -1] <- 0
for (j in seq_len(J)) { m <- mean(rc_num[,j], na.rm=TRUE); if(is.nan(m)) m <- 0.5; rc_num[is.na(rc_num[,j]),j] <- m }
pca <- prcomp(rc_num, center=TRUE, scale.=FALSE)
x_pca <- pca$x[,1:K]; for(k in 1:K) x_pca[,k] <- as.numeric(scale(x_pca[,k]))
x_start <- array(NA_real_, dim=c(N,K,T_periods)); for(t in 1:T_periods) x_start[,,t] <- x_pca

# Diffuse priors for ALL countries (no country anchors)
x_mu0 <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
# Convert to list of K x K diagonal matrices (as expected by .kalman_one_country)
x_Sigma0_list <- vector("list", N)
for (i in seq_len(N)) x_Sigma0_list[[i]] <- diag(x_Sigma0[i, ], nrow = K)

# Initialize from Part A warm start
alpha <- res_A$alpha
beta <- res_A$beta
x_smooth <- x_start
Omega <- 0.1 * diag(K)

# Data setup
rc <- flow$rc
startlegis <- as.integer(flow$startlegis)
endlegis <- as.integer(flow$endlegis)
bill_session <- as.integer(flow$bill.session)

# Precompute voters per item (static)
voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

dir.create("outputs/v6_item_anchors", recursive=TRUE, showWarnings=FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="UTC")
log_file <- sprintf("logs/v6_%s_item_%s.log", domain, ts)
sink(log_file, split=TRUE)
cat(sprintf("V6 Item-Anchor: %s | N=%d, J=%d, T=%d, K=%d\n", domain, N, J, T_periods, K))
cat(sprintf("Anchor items: %d, %d, %d\n", anchor_items[1], anchor_items[2], anchor_items[3]))
cat(sprintf("Anchor item labels: %s\n\n", paste(flow$item_labels[anchor_items], collapse=", ")))

maxit <- 5000L
loglik_trace <- numeric(maxit)
prev_ll <- -Inf
patience <- 0L
ncores <- 4L

t0 <- proc.time()
converged <- FALSE
final_iter <- maxit

for (iter in 1:maxit) {
  # Save old params for convergence check
  alpha_old <- alpha
  beta_old <- beta
  x_old <- x_smooth

  # ==== E-STEP ====

  # (a) Data Augmentation
  da <- da_step(rc, alpha, beta, x_smooth, bill_session)

  # (b) Kalman smoother per country (using internal helper)
  P_smooth_all <- vector("list", N)
  x_prev <- x_smooth

  ks_results <- parallel::mclapply(seq_len(N), function(i) {
    .kalman_one_country(
      i = i,
      da_y_star = da$y_star,
      rc = rc,
      alpha = alpha,
      beta = beta,
      bill.session = bill_session,
      x_mu0 = x_mu0,
      x_Sigma0 = x_Sigma0_list,
      Omega = Omega,
      startlegis = startlegis,
      endlegis = endlegis,
      T_total = T_periods,
      K = K,
      x_prev_i = matrix(x_prev[i, , ], nrow = K, ncol = T_periods)
    )
  }, mc.cores = ncores)

  for (i in seq_len(N)) {
    res_i <- ks_results[[i]]
    x_smooth[i, , ] <- res_i$x_smooth_i
    P_smooth_all[[i]] <- res_i$P_smooth_i
  }

  # ==== M-STEP ====
  m_items <- m_step_items(
    y_star       = da$y_star,
    rc           = rc,
    x_smooth     = x_smooth,
    P_smooth     = P_smooth_all,
    bill.session = bill_session,
    beta_mu      = rep(0, K+1),
    beta_sigma   = 25*diag(K+1),
    voters_by_item = voters_by_item
  )
  alpha <- m_items$alpha
  beta <- m_items$beta

  # ENFORCE item anchors: reset fixed items after M-step
  alpha[anchor_items] <- fixed_alpha
  beta[anchor_items, ] <- fixed_betas

  # ==== Convergence check ====
  delta <- param_change(alpha_old, alpha, beta_old, beta, x_old, x_smooth)

  if (iter == 1 || iter %% 50 == 0 || iter == maxit || delta < 1e-4) {
    ll <- compute_loglik(rc, alpha, beta, x_smooth, bill_session)
    loglik_trace[iter] <- ll
    cat(sprintf("Iter %4d | LL = %.4f | delta = %.2e\n", iter, ll, delta))

    if (iter > 1 && abs(ll - prev_ll) / (1 + abs(prev_ll)) < 0.01) {
      patience <- patience + 1L
      if (patience >= 5L) {
        cat(sprintf("Converged at iter %d (LL rel_change < 0.01 for 5 checks)\n", iter))
        converged <- TRUE
        final_iter <- iter
        break
      }
    } else {
      patience <- 0L
    }
    prev_ll <- ll
  } else {
    loglik_trace[iter] <- prev_ll
  }

  if (delta < 1e-4) {
    cat(sprintf("Converged at iter %d (delta = %.2e)\n", iter, delta))
    converged <- TRUE
    final_iter <- iter
    break
  }
}
elapsed_B <- (proc.time()-t0)["elapsed"]
loglik_trace <- loglik_trace[1:final_iter]
cat(sprintf("\nPart B done: %.1fs | Iters: %d | Conv: %d\n", elapsed_B, final_iter, as.integer(converged)))

# Save mean ideal points
sl <- startlegis; el <- endlegis
x_mean <- matrix(NA_real_, N, K)
for(i in 1:N) { s<-sl[i]+1; e<-el[i]+1; if(e>=s&&s>=1&&e<=T_periods) x_mean[i,] <- if(s==e) x_smooth[i,,s] else rowMeans(x_smooth[i,,s:e,drop=FALSE]) }
rownames(x_mean) <- flow$country_codes

out_B <- list(domain=domain, strategy="item_anchors", ideal_points=x_smooth,
              ideal_points_mean=x_mean, alpha=alpha, beta=beta,
              country_codes=flow$country_codes, item_labels=flow$item_labels,
              anchor_items=anchor_items, anchor_item_labels=flow$item_labels[anchor_items],
              fixed_betas=fixed_betas, fixed_alpha=fixed_alpha,
              runtime=list(seconds=elapsed_B, iters=final_iter, conv=as.integer(converged),
                           loglik_trace=loglik_trace))
saveRDS(out_B, sprintf("outputs/v6_item_anchors/%s_results.rds", domain))
cat(sprintf("Saved: outputs/v6_item_anchors/%s_results.rds\n", domain))
sink()
