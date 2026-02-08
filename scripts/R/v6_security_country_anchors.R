#!/usr/bin/env Rscript
set.seed(2026)
source("scripts/R/rcpp_merged/dynIRT_KD_rcpp.R")

K <- 2L
domain <- "security"
flow <- readRDS(file.path("data/processed", paste0(domain, "_flow_matrix.rds")))
N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T

anchor_iso <- c("DNK", "IRN", "UKR")
anchor_idx <- match(anchor_iso, flow$country_codes)
stopifnot(!any(is.na(anchor_idx)))
anchor_positions <- rbind(c(+2, 0), c(-2, 0), c(0, -2))

x_mu0 <- matrix(0, nrow=N, ncol=K)
x_Sigma0 <- matrix(1, nrow=N, ncol=K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ] <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

rc_num <- matrix(as.numeric(flow$rc), nrow=N, ncol=J)
rc_num[rc_num==0] <- NA; rc_num[rc_num==-1] <- 0
for (j in seq_len(J)) { m <- mean(rc_num[,j], na.rm=TRUE); if(is.nan(m)) m <- 0.5; rc_num[is.na(rc_num[,j]),j] <- m }
pca <- prcomp(rc_num, center=TRUE, scale.=FALSE)
x_pca <- pca$x[,1:K]; for(k in 1:K) x_pca[,k] <- as.numeric(scale(x_pca[,k]))
x_start <- array(NA_real_, dim=c(N,K,T_periods)); for(t in 1:T_periods) x_start[,,t] <- x_pca

dir.create("outputs/v6_country_anchors", recursive=TRUE, showWarnings=FALSE)
ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="UTC")
log_file <- sprintf("logs/v6_%s_country_%s.log", domain, ts)
sink(log_file, split=TRUE)
cat(sprintf("V6 Country-Anchor: %s | N=%d, J=%d, T=%d, K=%d
", domain, N, J, T_periods, K))
cat(sprintf("Anchors: %s

", paste(anchor_iso, collapse=", ")))

t0 <- proc.time()
res <- dynIRT_KD(
  .data = list(rc=flow$rc, startlegis=matrix(as.integer(flow$startlegis),ncol=1),
               endlegis=matrix(as.integer(flow$endlegis),ncol=1),
               bill.session=matrix(as.integer(flow$bill.session),ncol=1), T=T_periods),
  .starts = list(alpha=numeric(J), beta=matrix(rnorm(J*K,0,0.1),J,K), x=x_start),
  .priors = list(x.mu0=x_mu0, x.Sigma0=x_Sigma0, beta.mu=rep(0,K+1),
                 beta.sigma=25*diag(K+1), omega=0.1*diag(K)),
  .control = list(verbose=TRUE, thresh=1e-4, maxit=5000L, checkfreq=50L,
                  estimate_omega=FALSE, thresh_loglik=0.01, loglik_patience=5L, ncores=4L),
  K=K
)
elapsed_A <- (proc.time()-t0)["elapsed"]
cat(sprintf("
Part A done: %.1fs | Iters: %d | Conv: %d
", elapsed_A, res$runtime$iters, res$runtime$conv))

sl <- as.integer(flow$startlegis); el <- as.integer(flow$endlegis)
x_mean <- matrix(NA_real_, N, K)
for(i in 1:N) { s<-sl[i]+1; e<-el[i]+1; if(e>=s&&s>=1&&e<=T_periods) x_mean[i,] <- if(s==e) res$means$x[i,,s] else rowMeans(res$means$x[i,,s:e]) }
rownames(x_mean) <- flow$country_codes

out_A <- list(domain=domain, strategy="country_anchors", ideal_points=res$means$x,
              ideal_points_mean=x_mean, alpha=res$means$alpha, beta=res$means$beta,
              country_codes=flow$country_codes, item_labels=flow$item_labels,
              anchors=list(iso=anchor_iso, positions=anchor_positions),
              runtime=list(seconds=elapsed_A, iters=res$runtime$iters, conv=res$runtime$conv,
                           loglik_trace=res$runtime$loglik_trace))
saveRDS(out_A, sprintf("outputs/v6_country_anchors/%s_results.rds", domain))
cat(sprintf("Saved: outputs/v6_country_anchors/%s_results.rds
", domain))
sink()
