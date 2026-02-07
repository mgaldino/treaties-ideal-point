# Integration tests for dynIRT_KD_fast.R
# Run: testthat::test_file("tests/test_dynIRT_KD_fast.R")

library(testthat)

# Source all modules (individual sources first, then dynIRT_KD_fast skips re-sourcing)
.kd_dir <- file.path("..", "scripts", "R")
source(file.path(.kd_dir, "da_step.R"))
source(file.path(.kd_dir, "kalman.R"))
source(file.path(.kd_dir, "m_step.R"))
source(file.path(.kd_dir, "dynIRT_KD.R"))
source(file.path(.kd_dir, "dynIRT_KD_fast.R"))

# ============================================================
# Helper: generate simulated dataset (provided template, with K=1 support)
# ============================================================

simulate_test_data <- function(seed, N = 50, J = 100, T_sim = 4, K = 2) {
  set.seed(seed)
  library(MASS)
  Omega_true <- 0.1 * diag(K)
  x_true <- array(NA, dim = c(N, K, T_sim))
  x_true[, , 1] <- mvrnorm(N, mu = rep(0, K), Sigma = diag(K))
  for (t in 2:T_sim) {
    x_true[, , t] <- x_true[, , t - 1] +
      mvrnorm(N, mu = rep(0, K), Sigma = Omega_true)
  }
  alpha_true <- rnorm(J, 0, 1)
  beta_true  <- matrix(rnorm(J * K, 0, 0.7), nrow = J, ncol = K)
  bill_session <- sample(0:(T_sim - 1), J, replace = TRUE)
  rc <- matrix(0L, nrow = N, ncol = J)
  for (j in 1:J) {
    t_j <- bill_session[j] + 1L
    x_tj <- matrix(x_true[, , t_j], nrow = N, ncol = K)
    mu_star <- alpha_true[j] + x_tj %*% beta_true[j, ]
    rc[, j] <- ifelse(rnorm(N) < mu_star, 1L, -1L)
  }
  missing_mask <- matrix(rbinom(N * J, 1, 0.2), nrow = N, ncol = J)
  rc[missing_mask == 1] <- 0L

  # Anchors
  x_t1 <- matrix(x_true[, , 1], nrow = N, ncol = K)
  if (K == 1) {
    targets <- matrix(c(2, -2, 1), ncol = 1)
  } else {
    targets <- rbind(c(2, 2), c(-2, -2), c(1, -1))
  }
  anchor_idx <- integer(3)
  for (a in 1:3) {
    dists <- rowSums(sweep(x_t1, 2, targets[a, ])^2)
    dists[anchor_idx[anchor_idx > 0]] <- Inf
    anchor_idx[a] <- which.min(dists)
  }

  x_mu0 <- matrix(0, nrow = N, ncol = K)
  x_Sigma0 <- matrix(1, nrow = N, ncol = K)
  for (a in 1:3) {
    x_mu0[anchor_idx[a], ] <- targets[a, ]
    x_Sigma0[anchor_idx[a], ] <- 0.01
  }

  x_start <- x_true + array(rnorm(N * K * T_sim, 0, 0.5), dim = c(N, K, T_sim))
  for (a in 1:3) {
    for (t in 1:T_sim) x_start[anchor_idx[a], , t] <- targets[a, ]
  }

  list(
    .data = list(
      rc = rc,
      startlegis = matrix(rep(0L, N), ncol = 1),
      endlegis = matrix(rep(T_sim - 1L, N), ncol = 1),
      bill.session = matrix(bill_session, ncol = 1),
      T = T_sim
    ),
    .starts = list(
      alpha = rnorm(J, 0, 0.5),
      beta = matrix(rnorm(J * K, 0, 0.3), nrow = J, ncol = K),
      x = x_start
    ),
    .priors = list(
      x.mu0 = x_mu0, x.Sigma0 = x_Sigma0,
      beta.mu = rep(0, K + 1), beta.sigma = 25 * diag(K + 1),
      omega = 0.1 * diag(K)
    ),
    x_true = x_true, alpha_true = alpha_true, beta_true = beta_true,
    anchor_idx = anchor_idx
  )
}

# ============================================================
# Helpers
# ============================================================

run_pair <- function(seed, K, control_common, control_fast, control_vanilla) {
  sim <- simulate_test_data(seed = seed, K = K)

  .data <- sim$.data
  .starts <- sim$.starts
  .priors <- sim$.priors

  ctrl_v <- modifyList(control_common, control_vanilla)
  ctrl_f <- modifyList(control_common, control_fast)

  res_v <- dynIRT_KD(.data, .starts, .priors, ctrl_v, K = K)
  res_f <- dynIRT_KD_fast(.data, .starts, .priors, ctrl_f, K = K)

  list(sim = sim, vanilla = res_v, fast = res_f)
}

corr_vec <- function(a, b) {
  stats::cor(as.vector(a), as.vector(b))
}

corr_items <- function(res_a, res_b) {
  v1 <- c(res_a$means$alpha, as.vector(res_a$means$beta))
  v2 <- c(res_b$means$alpha, as.vector(res_b$means$beta))
  stats::cor(v1, v2)
}

# Simple cache to avoid repeated long runs
.cache <- new.env(parent = emptyenv())
get_cached <- function(key, expr) {
  if (!exists(key, envir = .cache)) {
    assign(key, eval(expr), envir = .cache)
  }
  get(key, envir = .cache)
}

# ============================================================
# A. Equivalence of results (critical)
# ============================================================

test_that("dynIRT_KD_fast: equivalence with vanilla EM", {
  res <- get_cached("equiv_k2_seed2", quote({
    run_pair(
      seed = 2,
      K = 2,
      control_common = list(
        verbose = FALSE,
        thresh = 1e-6,
        maxit = 120L,
        checkfreq = 50L,
        estimate_omega = FALSE
      ),
      control_fast = list(
        use_squarem = TRUE,
        thresh_aitken = 1e-4,
        ll_drop_tol = 1e-6,
        clamp_limit = 6
      ),
      control_vanilla = list()
    )
  }))

  r_x <- corr_vec(res$fast$means$x, res$vanilla$means$x)
  r_items <- corr_items(res$fast, res$vanilla)

  expect_true(r_x > 0.99)
  expect_true(r_items > 0.99)
})

# ============================================================
# B. Speedup (critical)
# ============================================================

test_that("dynIRT_KD_fast: speedup in EM evaluations or time", {
  res <- get_cached("speed_k2_seed202", quote({
    run_pair(
      seed = 202,
      K = 2,
      control_common = list(
        verbose = FALSE,
        thresh = 0,
        maxit = 200L,
        checkfreq = 50L,
        estimate_omega = FALSE
      ),
      control_fast = list(
        use_squarem = TRUE,
        thresh_aitken = 1e-3,
        ll_drop_tol = 1e-6,
        clamp_limit = 6
      ),
      control_vanilla = list()
    )
  }))

  em_v <- res$vanilla$runtime$iters
  em_f <- res$fast$runtime$em_evals
  time_v <- res$vanilla$runtime$seconds
  time_f <- res$fast$runtime$seconds

  em_ratio <- em_f / em_v
  time_ratio <- time_f / time_v

  expect_true(em_ratio <= 0.5 || time_ratio <= 0.5,
              info = sprintf("em_ratio=%.2f, time_ratio=%.2f", em_ratio, time_ratio))
})

# ============================================================
# C. Quality of solution (critical)
# ============================================================

test_that("dynIRT_KD_fast: solution quality and log-lik", {
  res <- get_cached("equiv_k2_seed2", quote({
    run_pair(
      seed = 2,
      K = 2,
      control_common = list(
        verbose = FALSE,
        thresh = 1e-6,
        maxit = 120L,
        checkfreq = 50L,
        estimate_omega = FALSE
      ),
      control_fast = list(
        use_squarem = TRUE,
        thresh_aitken = 1e-4,
        ll_drop_tol = 1e-6,
        clamp_limit = 6
      ),
      control_vanilla = list()
    )
  }))

  ll_fast <- tail(res$fast$runtime$loglik_trace, 1)
  ll_van <- tail(res$vanilla$runtime$loglik_trace, 1)
  expect_true(ll_fast >= ll_van - 1e-2)

  r_x <- corr_vec(res$fast$means$x, res$sim$x_true)
  r_items <- stats::cor(
    c(res$fast$means$alpha, as.vector(res$fast$means$beta)),
    c(res$sim$alpha_true, as.vector(res$sim$beta_true))
  )

  expect_true(r_x >= 0.93)
  expect_true(r_items >= 0.90)
})

# ============================================================
# D. Robustness (3 seeds, K=1 and K=2)
# ============================================================

test_that("dynIRT_KD_fast: robustness across seeds and dimensions", {
  seeds <- c(11, 22, 33)

  for (seed in seeds) {
    res_k2 <- run_pair(
      seed = seed,
      K = 2,
      control_common = list(
        verbose = FALSE,
        thresh = 1e-6,
        maxit = 80L,
        estimate_omega = FALSE
      ),
      control_fast = list(
        use_squarem = TRUE,
        thresh_aitken = 1e-4,
        ll_drop_tol = 1e-6,
        clamp_limit = 6
      ),
      control_vanilla = list()
    )

    expect_true(is.finite(tail(res_k2$fast$runtime$loglik_trace, 1)))
    expect_equal(dim(res_k2$fast$means$x), dim(res_k2$sim$x_true))

    if (length(res_k2$fast$runtime$events) > 0) {
      fallback_events <- vapply(res_k2$fast$runtime$events, function(e) e$type, "")
      if (any(fallback_events == "squarem_fallback")) {
        expect_true(res_k2$fast$runtime$conv %in% c(0L, 1L))
      }
    }
  }

  res_k1 <- run_pair(
    seed = 77,
    K = 1,
    control_common = list(
      verbose = FALSE,
      thresh = 1e-6,
      maxit = 80L,
      estimate_omega = FALSE
    ),
    control_fast = list(
      use_squarem = TRUE,
      thresh_aitken = 1e-4,
      ll_drop_tol = 1e-6,
      clamp_limit = 6
    ),
    control_vanilla = list()
  )

  expect_equal(dim(res_k1$fast$means$x), dim(res_k1$sim$x_true))
})

# ============================================================
# E. Aitken stopping (important)
# ============================================================

test_that("dynIRT_KD_fast: Aitken stops before maxit without harming recovery", {
  res <- get_cached("aitken_k2_seed303", quote({
    run_pair(
      seed = 303,
      K = 2,
      control_common = list(
        verbose = FALSE,
        thresh = 0,
        maxit = 200L,
        estimate_omega = FALSE
      ),
      control_fast = list(
        use_squarem = TRUE,
        thresh_aitken = 1e-3,
        ll_drop_tol = 1e-6,
        clamp_limit = 6
      ),
      control_vanilla = list()
    )
  }))

  expect_true(!is.na(res$fast$runtime$aitken_iter))
  expect_true(res$fast$runtime$aitken_iter < 200L)

  r_fast_x <- corr_vec(res$fast$means$x, res$sim$x_true)
  r_van_x  <- corr_vec(res$vanilla$means$x, res$sim$x_true)

  r_fast_items <- stats::cor(
    c(res$fast$means$alpha, as.vector(res$fast$means$beta)),
    c(res$sim$alpha_true, as.vector(res$sim$beta_true))
  )
  r_van_items <- stats::cor(
    c(res$vanilla$means$alpha, as.vector(res$vanilla$means$beta)),
    c(res$sim$alpha_true, as.vector(res$sim$beta_true))
  )

  expect_true(r_fast_x >= r_van_x - 0.01)
  expect_true(r_fast_items >= r_van_items - 0.01)
})
