# Helpers to run emIRT and dynIRT_KD on a training rc

build_starts <- function(rc_train, T_periods) {
  rc_pca <- rc_train
  rc_pca[rc_pca == 0] <- NA
  rc_pca[rc_pca == -1] <- 0
  for (j in seq_len(ncol(rc_pca))) {
    col_mean <- mean(rc_pca[, j], na.rm = TRUE)
    if (is.nan(col_mean)) col_mean <- 0
    rc_pca[is.na(rc_pca[, j]), j] <- col_mean
  }
  col_sd <- apply(rc_pca, 2, sd)
  keep <- which(is.finite(col_sd) & col_sd > 0)
  if (length(keep) == 0) {
    pc1 <- rnorm(nrow(rc_train), 0, 1)
  } else {
    pca <- prcomp(rc_pca[, keep, drop = FALSE], center = TRUE, scale. = TRUE)
    pc1 <- as.numeric(scale(pca$x[, 1]))
  }

  x_start_2d <- matrix(rep(pc1, times = T_periods), nrow = nrow(rc_train), ncol = T_periods)
  alpha_start <- matrix(rnorm(ncol(rc_train), 0, 0.1), ncol = 1)
  beta_start  <- matrix(rnorm(ncol(rc_train), 0, 0.5), ncol = 1)

  list(x_start_2d = x_start_2d, alpha_start = alpha_start, beta_start = beta_start)
}

run_emirt <- function(flow, rc_train, starts, priors, control) {
  res <- emIRT::dynIRT(
    .data = list(
      rc           = rc_train,
      startlegis   = flow$startlegis,
      endlegis     = flow$endlegis,
      bill.session = flow$bill.session,
      T            = flow$T
    ),
    .starts = list(
      x     = starts$x_start_2d,
      alpha = starts$alpha_start,
      beta  = starts$beta_start
    ),
    .priors = list(
      x.mu0      = priors$x_mu0,
      x.sigma0   = priors$x_sigma0,
      beta.mu    = priors$beta_mu,
      beta.sigma = priors$beta_sigma,
      omega2     = priors$omega2
    ),
    .control = control
  )
  res
}

run_dynirt_kd <- function(flow, rc_train, starts, priors, control, K = 1L) {
  # Source dependencies
  source("scripts/R/da_step.R")
  source("scripts/R/kalman.R")
  source("scripts/R/m_step.R")
  source("scripts/R/dynIRT_KD.R")

  # Convert x_start to N x K x T
  x_start_3d <- array(NA_real_, dim = c(nrow(rc_train), K, flow$T))
  for (t in seq_len(flow$T)) {
    x_start_3d[, 1, t] <- starts$x_start_2d[, t]
  }

  res <- dynIRT_KD(
    .data = list(
      rc           = rc_train,
      startlegis   = matrix(as.integer(flow$startlegis), ncol = 1),
      endlegis     = matrix(as.integer(flow$endlegis), ncol = 1),
      bill.session = matrix(as.integer(flow$bill.session), ncol = 1),
      T            = flow$T
    ),
    .starts = list(
      x     = x_start_3d,
      alpha = as.numeric(starts$alpha_start),
      beta  = starts$beta_start
    ),
    .priors = list(
      x.mu0      = priors$x_mu0,
      x.Sigma0   = priors$x_sigma0,
      beta.mu    = as.numeric(priors$beta_mu),
      beta.sigma = priors$beta_sigma,
      omega      = matrix(priors$omega2_val, nrow = K, ncol = K)
    ),
    .control = control,
    K = K
  )

  res
}
