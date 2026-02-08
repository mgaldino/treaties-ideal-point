#!/usr/bin/env Rscript
# V6 Part A: Intellectual Property - Country Anchor Estimation (2D)
# Anchors: DNK(+2,0), AGO(-2,0), CHE(0,+2)
set.seed(2026)
tryCatch({
  source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")
  cat("Loaded Rcpp-optimized dynIRT_KD
")
}, error = function(e) {
  cat(sprintf("Rcpp failed: %s
Falling back to pure R
", e$message))
  source("scripts/R/dynIRT_KD.R")
})
if (!exists("compute_loglik", mode = "function")) source("scripts/R/dynIRT_KD.R")

K <- 2L
domain <- "intellectual_property"
flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
if (!file.exists(flow_path)) stop("Missing: ", flow_path)
flow <- readRDS(flow_path)
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
sl <- as.integer(flow$startlegis); el <- as.integer(flow$endlegis); bs <- as.integer(flow$bill.session)
cat(sprintf("Domain: %s | N=%d, J=%d, T=%d, K=%d
", domain, N, J, T_periods, K))
if (J < 50) cat(sprintf("WARNING: only %d items - K=2 may be weakly identified
", J))

anchor_iso <- c("DNK", "AGO", "CHE")
anchor_positions <- rbind(c(+2, 0), c(-2, 0), c(0, +2))
anchor_idx <- match(anchor_iso, flow$country_codes)
if (any(is.na(anchor_idx))) stop("Anchor(s) not in data: ", paste(anchor_iso[is.na(anchor_idx)], collapse=", "))
cat(sprintf("Anchors: %s (idx %s)
", paste(anchor_iso, collapse="/"), paste(anchor_idx, collapse="/")))

x_mu0 <- matrix(0, nrow=N, ncol=K); x_Sigma0 <- matrix(1, nrow=N, ncol=K)
for (a in seq_along(anchor_idx)) { x_mu0[anchor_idx[a],] <- anchor_positions[a,]; x_Sigma0[anchor_idx[a],] <- 0.01 }

rc_num <- matrix(as.numeric(flow$rc), nrow=N, ncol=J); rc_num[rc_num==0] <- NA; rc_num[rc_num==-1] <- 0
for (j in seq_len(J)) { m <- mean(rc_num[,j], na.rm=TRUE); if(is.nan(m)) m <- 0.5; rc_num[is.na(rc_num[,j]),j] <- m }
pca <- prcomp(rc_num, center=TRUE, scale.=FALSE)
npc <- min(K, ncol(pca$x)); x_pca <- pca$x[, seq_len(npc), drop=FALSE]
if (npc < K) x_pca <- cbind(x_pca, matrix(rnorm(N*(K-npc), 0, 0.01), nrow=N))
for (k in seq_len(K)) x_pca[,k] <- as.numeric(scale(x_pca[,k]))
x_start <- array(NA_real_, dim=c(N,K,T_periods)); for (t in seq_len(T_periods)) x_start[,,t] <- x_pca

dir.create("logs", recursive=TRUE, showWarnings=FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="UTC")
log_file <- sprintf("logs/v6_ip_country_anchors_%s.log", ts)
sink(log_file, split=TRUE)
cat(sprintf("V6 Part A: %s | Country Anchors: %s
", domain, paste(anchor_iso, collapse="/")))
for (a in seq_along(anchor_iso)) cat(sprintf("  %s -> (%.1f, %.1f)
", anchor_iso[a], anchor_positions[a,1], anchor_positions[a,2]))
cat(sprintf("N=%d, J=%d, T=%d, K=%d

", N, J, T_periods, K))

t0 <- proc.time()
res <- dynIRT_KD(
  .data = list(rc=flow$rc, startlegis=matrix(sl,ncol=1), endlegis=matrix(el,ncol=1), bill.session=matrix(bs,ncol=1), T=T_periods),
  .starts = list(alpha=numeric(J), beta=matrix(rnorm(J*K,0,0.1),J,K), x=x_start),
  .priors = list(x.mu0=x_mu0, x.Sigma0=x_Sigma0, beta.mu=rep(0,K+1), beta.sigma=25*diag(K+1), omega=0.1*diag(K)),
  .control = list(verbose=TRUE, thresh=1e-4, maxit=5000L, checkfreq=50L, estimate_omega=FALSE, thresh_loglik=0.01, loglik_patience=5L, ncores=4L),
  K=K)
elapsed <- (proc.time()-t0)["elapsed"]
cat(sprintf("
Done: %.1fs | Iters: %d | Conv: %d
", elapsed, res$runtime$iters, res$runtime$conv))

x_est <- res$means$x; x_mean <- matrix(NA_real_, nrow=N, ncol=K)
for (i in seq_len(N)) { s<-sl[i]+1L; e<-el[i]+1L; if(e>=s&&s>=1&&e<=T_periods) x_mean[i,] <- if(s==e) x_est[i,,s] else rowMeans(x_est[i,,s:e]) }
rownames(x_mean) <- flow$country_codes; colnames(x_mean) <- paste0("dim", seq_len(K))

cat("
--- Dim 1 Top 10 ---
"); ord1 <- order(x_mean[,1], decreasing=TRUE, na.last=TRUE)
for (r in head(ord1, 10)) cat(sprintf("  %s: %.3f
", flow$country_codes[r], x_mean[r,1]))
cat("
--- Dim 1 Bottom 10 ---
")
for (r in tail(ord1[!is.na(x_mean[ord1,1])], 10)) cat(sprintf("  %s: %.3f
", flow$country_codes[r], x_mean[r,1]))
cat("
--- Dim 2 Top 10 ---
"); ord2 <- order(x_mean[,2], decreasing=TRUE, na.last=TRUE)
for (r in head(ord2, 10)) cat(sprintf("  %s: %.3f
", flow$country_codes[r], x_mean[r,2]))

cat("
--- Item Parameters (alpha, beta1, beta2) ---
")
for (j in seq_len(J)) {
  lbl <- if (!is.null(flow$item_labels)) flow$item_labels[j] else paste0("item_", j)
  cat(sprintf("  [%2d] %-50s alpha=%6.3f  beta1=%6.3f  beta2=%6.3f
", j, substr(lbl,1,50), res$means$alpha[j], res$means$beta[j,1], res$means$beta[j,2]))
}

out_dir <- "outputs/v6_country_anchors"; dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out <- list(domain=domain, ideal_points=x_est, ideal_points_mean=x_mean, alpha=res$means$alpha, beta=res$means$beta,
            country_codes=flow$country_codes, period_labels=flow$period_labels, item_labels=flow$item_labels,
            anchors=list(iso=anchor_iso, positions=anchor_positions),
            runtime=list(seconds=elapsed, iters=res$runtime$iters, conv=res$runtime$conv, loglik_trace=res$runtime$loglik_trace),
            omega=res$omega)
out_path <- file.path(out_dir, "intellectual_property_results.rds"); saveRDS(out, out_path)
cat(sprintf("
Saved: %s
", out_path))
sink(); cat(sprintf("Log: %s
", log_file))
