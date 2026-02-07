#!/usr/bin/env Rscript

# Benchmark R vs Rcpp for dynIRT_KD

set.seed(2026)

# ---- Data and priors (from v5_per_domain_2d.R) ----
K <- 2L
domain <- "investment"
flow_path <- file.path("data/processed", paste0(domain, "_flow_matrix.rds"))
if (!file.exists(flow_path)) stop("Missing: ", flow_path)
flow <- readRDS(flow_path)

anchor_config <- list(
  investment = list(primary = c("DNK", "IRN", "CHN"),
                    fallback = c("DNK", "IRN", "CHN"))
)
anchor_positions <- rbind(
  c(+2, +2),
  c(-2, -2),
  c(+1, -1)
)

build_pca_starts_2d <- function(rc, T_periods, K = 2L) {
  N <- nrow(rc); J <- ncol(rc)
  rc_num <- matrix(as.numeric(rc), nrow = N, ncol = J)
  rc_num[rc_num == 0] <- NA
  rc_num[rc_num == -1] <- 0
  for (j in seq_len(J)) {
    m <- mean(rc_num[, j], na.rm = TRUE)
    if (is.nan(m)) m <- 0.5
    rc_num[is.na(rc_num[, j]), j] <- m
  }
  pca <- prcomp(rc_num, center = TRUE, scale. = FALSE)
  npc <- min(K, ncol(pca$x))
  x_pca <- pca$x[, seq_len(npc), drop = FALSE]
  if (npc < K) {
    x_pca <- cbind(x_pca, matrix(rnorm(N * (K - npc), 0, 0.01), nrow = N))
  }
  for (k in seq_len(K)) x_pca[, k] <- as.numeric(scale(x_pca[, k]))
  x_start <- array(NA_real_, dim = c(N, K, T_periods))
  for (t in seq_len(T_periods)) x_start[, , t] <- x_pca
  list(
    x     = x_start,
    alpha = numeric(J),
    beta  = matrix(rnorm(J * K, 0, 0.1), nrow = J, ncol = K)
  )
}

N <- nrow(flow$rc); J <- ncol(flow$rc); T_periods <- flow$T
sl <- as.integer(flow$startlegis)
el <- as.integer(flow$endlegis)
bs <- as.integer(flow$bill.session)

cfg <- anchor_config[[domain]]
anchor_iso <- cfg$primary
anchor_idx <- match(anchor_iso, flow$country_codes)
if (any(is.na(anchor_idx))) {
  anchor_iso <- cfg$fallback
  anchor_idx <- match(anchor_iso, flow$country_codes)
  if (any(is.na(anchor_idx))) stop("Anchor(s) not in data")
}

x_mu0    <- matrix(0, nrow = N, ncol = K)
x_Sigma0 <- matrix(1, nrow = N, ncol = K)
for (a in seq_along(anchor_idx)) {
  x_mu0[anchor_idx[a], ]    <- anchor_positions[a, ]
  x_Sigma0[anchor_idx[a], ] <- 0.01
}

starts <- build_pca_starts_2d(flow$rc, T_periods, K)

.data <- list(
  rc           = flow$rc,
  startlegis   = matrix(sl, ncol = 1),
  endlegis     = matrix(el, ncol = 1),
  bill.session = matrix(bs, ncol = 1),
  T            = T_periods
)

.priors <- list(
  x.mu0      = x_mu0,
  x.Sigma0   = x_Sigma0,
  beta.mu    = rep(0, K + 1),
  beta.sigma = 25 * diag(K + 1),
  omega      = 0.1 * diag(K)
)

.control <- list(
  verbose        = FALSE,
  thresh         = 1e-4,
  maxit          = 50L,
  checkfreq      = 25L,
  estimate_omega = FALSE,
  thresh_aitken  = 1e-4,
  ncores         = 4L
)

# ---- Timing helpers ----
make_timer <- function() {
  env <- new.env(parent = emptyenv())
  env$da <- 0
  env$m <- 0
  env$kalman <- 0
  env
}

wrap_mclapply <- function(timings_env) {
  ns <- asNamespace("parallel")
  original <- get("mclapply", envir = ns)
  wrapper <- function(X, FUN, ..., mc.cores = getOption("mc.cores", 1L), mc.preschedule = TRUE,
                      mc.set.seed = TRUE, mc.silent = FALSE, mc.cleanup = TRUE, mc.allow.recursive = TRUE) {
    t0 <- proc.time()
    res <- original(X, FUN, ..., mc.cores = mc.cores, mc.preschedule = mc.preschedule,
                    mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cleanup = mc.cleanup,
                    mc.allow.recursive = mc.allow.recursive)
    timings_env$kalman <- timings_env$kalman + (proc.time() - t0)["elapsed"]
    res
  }

  lock_ok <- bindingIsLocked("mclapply", ns)
  if (lock_ok) unlockBinding("mclapply", ns)
  assign("mclapply", wrapper, envir = ns)
  if (lock_ok) lockBinding("mclapply", ns)

  list(original = original, locked = lock_ok)
}

restore_mclapply <- function(state) {
  ns <- asNamespace("parallel")
  if (state$locked) unlockBinding("mclapply", ns)
  assign("mclapply", state$original, envir = ns)
  if (state$locked) lockBinding("mclapply", ns)
}

run_model <- function(use_rcpp = FALSE) {
  if (use_rcpp) {
    source("scripts/R/rcpp_codex/dynIRT_KD_rcpp.R")
  } else {
    source("scripts/R/da_step.R")
    source("scripts/R/kalman.R")
    source("scripts/R/m_step.R")
    source("scripts/R/dynIRT_KD.R")
  }

  timings_env <- make_timer()

  env_fun <- environment(dynIRT_KD)

  da_step_orig <- get("da_step", envir = env_fun)
  assign("da_step", function(...) {
    t0 <- proc.time()
    out <- da_step_orig(...)
    timings_env$da <- timings_env$da + (proc.time() - t0)["elapsed"]
    out
  }, envir = env_fun)

  m_step_items_orig <- get("m_step_items", envir = env_fun)
  assign("m_step_items", function(...) {
    t0 <- proc.time()
    out <- m_step_items_orig(...)
    timings_env$m <- timings_env$m + (proc.time() - t0)["elapsed"]
    out
  }, envir = env_fun)

  mcl_state <- wrap_mclapply(timings_env)

  t0 <- proc.time()
  res <- dynIRT_KD(
    .data = .data,
    .starts = list(alpha = starts$alpha, beta = starts$beta, x = starts$x),
    .priors = .priors,
    .control = .control,
    K = K
  )
  total_time <- (proc.time() - t0)["elapsed"]

  restore_mclapply(mcl_state)

  # restore originals
  assign("da_step", da_step_orig, envir = env_fun)
  assign("m_step_items", m_step_items_orig, envir = env_fun)

  list(
    result = res,
    timings = list(
      da = as.numeric(timings_env$da),
      kalman = as.numeric(timings_env$kalman),
      m = as.numeric(timings_env$m)
    ),
    total_time = as.numeric(total_time)
  )
}

log_file <- "logs/rcpp_codex_benchmark.log"
sink(log_file, split = TRUE)

cat("Benchmark dynIRT_KD: R vs Rcpp\n")
cat(sprintf("Domain: %s | N=%d, J=%d, T=%d\n", domain, N, J, T_periods))
cat("Maxit: 50 | ncores: 4\n\n")

cat("--- Running R version ---\n")
bench_r <- run_model(use_rcpp = FALSE)
cat(sprintf("R total time: %.2fs\n", bench_r$total_time))
cat(sprintf("R timings (DA/Kalman/M): %.2fs / %.2fs / %.2fs\n",
            bench_r$timings$da, bench_r$timings$kalman, bench_r$timings$m))

cat("\n--- Running Rcpp version ---\n")
bench_rcpp <- run_model(use_rcpp = TRUE)
cat(sprintf("Rcpp total time: %.2fs\n", bench_rcpp$total_time))
cat(sprintf("Rcpp timings (DA/Kalman/M): %.2fs / %.2fs / %.2fs\n",
            bench_rcpp$timings$da, bench_rcpp$timings$kalman, bench_rcpp$timings$m))

speedup <- bench_r$total_time / bench_rcpp$total_time
cat(sprintf("\nSpeedup (total): %.2fx\n", speedup))

out <- list(
  meta = list(domain = domain, N = N, J = J, T = T_periods, maxit = 50L, ncores = 4L),
  R = list(total_time = bench_r$total_time, timings = bench_r$timings,
           runtime = bench_r$result$runtime),
  Rcpp = list(total_time = bench_rcpp$total_time, timings = bench_rcpp$timings,
              runtime = bench_rcpp$result$runtime),
  speedup = speedup
)

saveRDS(out, file = "outputs/rcpp_codex_benchmark.rds")

sink()
cat(sprintf("Benchmark log written to: %s\n", log_file))
