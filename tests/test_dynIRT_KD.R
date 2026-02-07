# Integration tests for dynIRT_KD.R
# Run: testthat::test_file("tests/test_dynIRT_KD.R")

library(testthat)

# Source all modules (individual sources first, then dynIRT_KD skips re-sourcing)
.kd_dir <- file.path("..", "scripts", "R")
source(file.path(.kd_dir, "da_step.R"))
source(file.path(.kd_dir, "kalman.R"))
source(file.path(.kd_dir, "m_step.R"))
source(file.path(.kd_dir, "dynIRT_KD.R"))

# ============================================================
# Helper: generate a small simulated dataset
# ============================================================

simulate_small_data <- function(N = 20, J = 30, T_total = 3, K = 2, seed = 42) {
  set.seed(seed)

  # True ideal points (random walk)
  Omega_true <- 0.1 * diag(K)
  x_true <- array(NA, dim = c(N, K, T_total))
  x_true[, , 1] <- MASS::mvrnorm(N, mu = rep(0, K), Sigma = diag(K))
  for (t in 2:T_total) {
    x_true[, , t] <- x_true[, , t - 1] +
      MASS::mvrnorm(N, mu = rep(0, K), Sigma = Omega_true)
  }

  # True item parameters
  alpha_true <- rnorm(J, 0, 0.5)
  beta_true  <- matrix(rnorm(J * K, 0, 0.7), nrow = J, ncol = K)

  # Assign items to periods
  bill.session <- sample(0:(T_total - 1), J, replace = TRUE)

  # Generate votes (probit)
  rc <- matrix(0L, nrow = N, ncol = J)
  for (j in 1:J) {
    t_j <- bill.session[j] + 1L
    x_tj <- matrix(x_true[, , t_j], nrow = N, ncol = K)
    mu_star <- alpha_true[j] + x_tj %*% beta_true[j, ]
    rc[, j] <- ifelse(rnorm(N) < mu_star, 1L, -1L)
  }

  # Introduce ~20% missing
  missing_mask <- matrix(rbinom(N * J, 1, 0.2), nrow = N, ncol = J)
  rc[missing_mask == 1] <- 0L

  # All countries active for all periods

  startlegis <- rep(0L, N)
  endlegis   <- rep(T_total - 1L, N)

  list(
    rc = rc,
    startlegis = matrix(startlegis, ncol = 1),
    endlegis   = matrix(endlegis, ncol = 1),
    bill.session = matrix(bill.session, ncol = 1),
    T = T_total,
    x_true = x_true,
    alpha_true = alpha_true,
    beta_true = beta_true,
    K = K,
    N = N,
    J = J
  )
}

# ============================================================
# Tests for compute_loglik()
# ============================================================

test_that("compute_loglik: returns finite negative value", {
  sim <- simulate_small_data()

  ll <- compute_loglik(sim$rc, sim$alpha_true,
                       sim$beta_true, sim$x_true,
                       as.integer(sim$bill.session))
  expect_true(is.finite(ll))
  expect_true(ll < 0)  # log-likelihood of binary data is always negative
})

test_that("compute_loglik: better params give higher log-likelihood", {
  sim <- simulate_small_data()
  bs <- as.integer(sim$bill.session)

  # True params should give higher LL than random params
  ll_true <- compute_loglik(sim$rc, sim$alpha_true, sim$beta_true,
                            sim$x_true, bs)

  alpha_rand <- rnorm(sim$J)
  beta_rand  <- matrix(rnorm(sim$J * sim$K), nrow = sim$J)
  x_rand     <- array(rnorm(sim$N * sim$K * sim$T), dim = c(sim$N, sim$K, sim$T))

  ll_rand <- compute_loglik(sim$rc, alpha_rand, beta_rand, x_rand, bs)

  expect_true(ll_true > ll_rand)
})

# ============================================================
# Tests for dynIRT_KD() integration
# ============================================================

test_that("dynIRT_KD: runs without error on small simulated data (K=2)", {
  sim <- simulate_small_data(N = 15, J = 20, T_total = 3, K = 2, seed = 1)
  K <- 2

  # Simple starting values
  x_start <- array(0, dim = c(sim$N, K, sim$T))
  alpha_start <- rep(0, sim$J)
  beta_start  <- matrix(rnorm(sim$J * K, 0, 0.1), nrow = sim$J, ncol = K)

  .data <- list(
    rc = sim$rc,
    startlegis = sim$startlegis,
    endlegis   = sim$endlegis,
    bill.session = sim$bill.session,
    T = sim$T
  )

  .starts <- list(
    alpha = alpha_start,
    beta  = beta_start,
    x     = x_start
  )

  .priors <- list(
    x.mu0    = matrix(0, nrow = sim$N, ncol = K),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),  # diagonal variances
    beta.mu    = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega      = 0.1 * diag(K)
  )

  .control <- list(
    threads = 1L,
    verbose = FALSE,
    thresh  = 1e-4,
    maxit   = 20L,
    checkfreq = 5L,
    estimate_omega = FALSE
  )

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)

  # Check output structure
  expect_true(is.list(result))
  expect_true("means" %in% names(result))
  expect_true("vars" %in% names(result))
  expect_true("omega" %in% names(result))
  expect_true("runtime" %in% names(result))

  # Check dimensions
  expect_equal(dim(result$means$x), c(sim$N, K, sim$T))
  expect_length(result$means$alpha, sim$J)
  expect_equal(dim(result$means$beta), c(sim$J, K))

  # Check runtime info
  expect_true(result$runtime$iters > 0)
  expect_true(result$runtime$seconds >= 0)
  expect_length(result$runtime$loglik_trace, result$runtime$iters)
})

test_that("dynIRT_KD: log-likelihood is non-decreasing (EM guarantee)", {
  sim <- simulate_small_data(N = 15, J = 20, T_total = 3, K = 2, seed = 2)
  K <- 2

  x_start <- sim$x_true + array(rnorm(sim$N * K * sim$T, 0, 0.5),
                                  dim = c(sim$N, K, sim$T))
  alpha_start <- sim$alpha_true + rnorm(sim$J, 0, 0.2)
  beta_start  <- sim$beta_true + matrix(rnorm(sim$J * K, 0, 0.2),
                                         nrow = sim$J, ncol = K)

  .data <- list(rc = sim$rc, startlegis = sim$startlegis,
                endlegis = sim$endlegis, bill.session = sim$bill.session,
                T = sim$T)
  .starts <- list(alpha = alpha_start, beta = beta_start, x = x_start)
  .priors <- list(
    x.mu0 = matrix(0, nrow = sim$N, ncol = K),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  )
  .control <- list(verbose = FALSE, thresh = 1e-6, maxit = 50L,
                   estimate_omega = FALSE)

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)

  ll <- result$runtime$loglik_trace
  n_iters <- length(ll)

  if (n_iters >= 2) {
    # Allow tiny numerical noise (1e-6)
    diffs <- diff(ll)
    expect_true(all(diffs >= -1e-6),
                info = sprintf("LL decreased at iter %d: diff = %.2e",
                               which.min(diffs) + 1, min(diffs)))
  }
})

test_that("dynIRT_KD: K=1 runs correctly (smoke test)", {
  sim <- simulate_small_data(N = 10, J = 15, T_total = 3, K = 1, seed = 3)
  K <- 1

  .data <- list(rc = sim$rc, startlegis = sim$startlegis,
                endlegis = sim$endlegis, bill.session = sim$bill.session,
                T = sim$T)
  .starts <- list(
    alpha = rep(0, sim$J),
    beta  = matrix(rnorm(sim$J, 0, 0.5), nrow = sim$J, ncol = 1),
    x     = array(0, dim = c(sim$N, 1, sim$T))
  )
  .priors <- list(
    x.mu0 = matrix(0, nrow = sim$N, ncol = 1),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = 1),
    beta.mu = rep(0, 2),
    beta.sigma = 25 * diag(2),
    omega = matrix(0.1, 1, 1)
  )
  .control <- list(verbose = FALSE, thresh = 1e-4, maxit = 30L,
                   estimate_omega = FALSE)

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = 1L)

  expect_equal(dim(result$means$x), c(sim$N, 1, sim$T))
  expect_equal(dim(result$means$beta), c(sim$J, 1))
  expect_true(result$runtime$iters > 0)
})

test_that("dynIRT_KD: anchor priors constrain ideal points", {
  sim <- simulate_small_data(N = 15, J = 20, T_total = 3, K = 2, seed = 4)
  K <- 2

  # Anchor countries 1 and 2 with tight priors
  anchor_pos <- rbind(c(2, 2), c(-2, -2))
  x_mu0 <- matrix(0, nrow = sim$N, ncol = K)
  x_mu0[1, ] <- anchor_pos[1, ]
  x_mu0[2, ] <- anchor_pos[2, ]

  # Tight priors for anchors, diffuse for others
  x_Sigma0 <- matrix(1, nrow = sim$N, ncol = K)
  x_Sigma0[1, ] <- 0.01
  x_Sigma0[2, ] <- 0.01

  .data <- list(rc = sim$rc, startlegis = sim$startlegis,
                endlegis = sim$endlegis, bill.session = sim$bill.session,
                T = sim$T)
  .starts <- list(
    alpha = rep(0, sim$J),
    beta = matrix(rnorm(sim$J * K, 0, 0.1), nrow = sim$J, ncol = K),
    x = array(0, dim = c(sim$N, K, sim$T))
  )
  .priors <- list(
    x.mu0 = x_mu0,
    x.Sigma0 = x_Sigma0,
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  )
  .control <- list(verbose = FALSE, thresh = 1e-4, maxit = 30L,
                   estimate_omega = FALSE)

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)

  # Anchor 1 at t=1 should be close to (2, 2)
  x1_t1 <- result$means$x[1, , 1]
  expect_equal(x1_t1, c(2, 2), tolerance = 0.5)

  # Anchor 2 at t=1 should be close to (-2, -2)
  x2_t1 <- result$means$x[2, , 1]
  expect_equal(x2_t1, c(-2, -2), tolerance = 0.5)
})

test_that("dynIRT_KD: Aitken stopping triggers before maxit", {
  sim <- simulate_small_data(N = 20, J = 30, T_total = 3, K = 2, seed = 7)
  K <- 2

  x_start <- sim$x_true + array(rnorm(sim$N * K * sim$T, 0, 0.5),
                                  dim = c(sim$N, K, sim$T))

  .data <- list(rc = sim$rc, startlegis = sim$startlegis,
                endlegis = sim$endlegis, bill.session = sim$bill.session,
                T = sim$T)
  .starts <- list(
    alpha = sim$alpha_true + rnorm(sim$J, 0, 0.2),
    beta  = sim$beta_true + matrix(rnorm(sim$J * K, 0, 0.2), nrow = sim$J, ncol = K),
    x     = x_start
  )
  .priors <- list(
    x.mu0 = matrix(0, nrow = sim$N, ncol = K),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  )

  # Run with Aitken enabled and a generous maxit
  .control <- list(verbose = FALSE, thresh = 0, maxit = 200L,
                   estimate_omega = FALSE, thresh_aitken = 1e-3)

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)

  # Aitken should have stopped before 200 iterations
  expect_true(!is.na(result$runtime$aitken_iter))
  expect_true(result$runtime$aitken_iter < 200L)
  expect_equal(result$runtime$conv, 1L)
})

test_that("dynIRT_KD: Aitken disabled by default (aitken_iter is NA)", {
  sim <- simulate_small_data(N = 10, J = 15, T_total = 3, K = 2, seed = 8)
  K <- 2

  .data <- list(rc = sim$rc, startlegis = sim$startlegis,
                endlegis = sim$endlegis, bill.session = sim$bill.session,
                T = sim$T)
  .starts <- list(
    alpha = rep(0, sim$J),
    beta = matrix(rnorm(sim$J * K, 0, 0.1), nrow = sim$J, ncol = K),
    x = array(0, dim = c(sim$N, K, sim$T))
  )
  .priors <- list(
    x.mu0 = matrix(0, nrow = sim$N, ncol = K),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  )
  .control <- list(verbose = FALSE, thresh = 1e-4, maxit = 20L,
                   estimate_omega = FALSE)

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)

  # aitken_iter should exist in runtime and be NA
  expect_true("aitken_iter" %in% names(result$runtime))
  expect_true(is.na(result$runtime$aitken_iter))
})

test_that("dynIRT_KD: estimate_omega option works", {
  sim <- simulate_small_data(N = 15, J = 20, T_total = 3, K = 2, seed = 5)
  K <- 2

  .data <- list(rc = sim$rc, startlegis = sim$startlegis,
                endlegis = sim$endlegis, bill.session = sim$bill.session,
                T = sim$T)
  .starts <- list(
    alpha = rep(0, sim$J),
    beta = matrix(rnorm(sim$J * K, 0, 0.1), nrow = sim$J, ncol = K),
    x = array(0, dim = c(sim$N, K, sim$T))
  )
  .priors <- list(
    x.mu0 = matrix(0, nrow = sim$N, ncol = K),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),
    beta.mu = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega = 0.1 * diag(K)
  )
  .control <- list(verbose = FALSE, thresh = 1e-4, maxit = 10L,
                   estimate_omega = TRUE, diagonal_omega = FALSE)

  result <- dynIRT_KD(.data, .starts, .priors, .control, K = K)

  # Omega should have changed from the initial 0.1 * I
  expect_false(isTRUE(all.equal(result$omega, 0.1 * diag(K))))
  # Should still be symmetric PD
  expect_equal(result$omega, t(result$omega), tolerance = 1e-12)
  eigs <- eigen(result$omega, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigs > 0))
})

test_that("dynIRT_KD: parallel ncores matches sequential", {
  skip_on_os("windows")

  sim <- simulate_small_data(N = 10, J = 15, T_total = 3, K = 2, seed = 6)
  K <- 2

  x_start <- array(0, dim = c(sim$N, K, sim$T))
  alpha_start <- rep(0, sim$J)
  beta_start  <- matrix(rnorm(sim$J * K, 0, 0.1), nrow = sim$J, ncol = K)

  .data <- list(
    rc = sim$rc,
    startlegis = sim$startlegis,
    endlegis   = sim$endlegis,
    bill.session = sim$bill.session,
    T = sim$T
  )

  .starts <- list(
    alpha = alpha_start,
    beta  = beta_start,
    x     = x_start
  )

  .priors <- list(
    x.mu0    = matrix(0, nrow = sim$N, ncol = K),
    x.Sigma0 = matrix(1, nrow = sim$N, ncol = K),
    beta.mu    = rep(0, K + 1),
    beta.sigma = 25 * diag(K + 1),
    omega      = 0.1 * diag(K)
  )

  .control_seq <- list(
    verbose = FALSE,
    thresh  = 1e-4,
    maxit   = 10L,
    estimate_omega = FALSE,
    ncores = 1L
  )

  .control_par <- list(
    verbose = FALSE,
    thresh  = 1e-4,
    maxit   = 10L,
    estimate_omega = FALSE,
    ncores = 2L
  )

  res_seq <- dynIRT_KD(.data, .starts, .priors, .control_seq, K = K)
  res_par <- dynIRT_KD(.data, .starts, .priors, .control_par, K = K)

  expect_true(isTRUE(all.equal(res_seq$means$x, res_par$means$x, tolerance = 1e-12)))
  expect_true(isTRUE(all.equal(res_seq$means$alpha, res_par$means$alpha, tolerance = 1e-12)))
  expect_true(isTRUE(all.equal(res_seq$means$beta, res_par$means$beta, tolerance = 1e-12)))
})
