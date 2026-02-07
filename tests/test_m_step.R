# Tests for m_step.R
# Run: testthat::test_file("tests/test_m_step.R")

library(testthat)

source(file.path("..", "scripts", "R", "m_step.R"))

# ============================================================
# Tests for m_step_items()
# ============================================================

test_that("m_step_items: all-missing item returns prior mean", {
  N <- 5; J <- 2; K <- 2; T_total <- 2

  # Item 1: all missing. Item 2: all observed.
  rc <- matrix(0L, nrow = N, ncol = J)
  rc[, 2] <- sample(c(-1L, 1L), N, replace = TRUE)

  y_star <- matrix(NA_real_, nrow = N, ncol = J)
  y_star[, 2] <- rnorm(N)

  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))

  # P_smooth: list of N, each list of T_total K x K matrices
  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) 0.1 * diag(K))
  })

  bill.session <- c(0L, 1L)
  beta_mu <- rep(0, K + 1)
  beta_sigma <- 25 * diag(K + 1)

  res <- m_step_items(y_star, rc, x_smooth, P_smooth, bill.session,
                      beta_mu, beta_sigma)

  # Item 1 (all missing): should return 0 (prior mean with no data)
  expect_equal(res$alpha[1], 0, tolerance = 1e-10)
  expect_equal(res$beta[1, ], rep(0, K), tolerance = 1e-10)

  # Item 2: should be non-zero (informed by data)
  expect_true(any(abs(c(res$alpha[2], res$beta[2, ])) > 1e-6))
})

test_that("m_step_items: recovers known regression coefficients", {
  # Design: 1D (K=1), single period, known true params
  K <- 1; N <- 200; J <- 1; T_total <- 1

  set.seed(42)
  alpha_true <- 0.5
  beta_true <- 1.5

  # Known ideal points (no uncertainty)
  x_vals <- rnorm(N)
  x_smooth <- array(x_vals, dim = c(N, K, T_total))

  # Pseudo-observations from the true model (no noise for MAP recovery)
  y_star <- matrix(alpha_true + beta_true * x_vals, nrow = N, ncol = J)

  rc <- matrix(1L, nrow = N, ncol = J)  # all observed
  bill.session <- 0L

  # Zero uncertainty in ideal points
  P_smooth <- lapply(1:N, function(i) {
    list(matrix(0, K, K))
  })

  # Diffuse prior
  beta_mu <- rep(0, K + 1)
  beta_sigma <- 1000 * diag(K + 1)

  res <- m_step_items(y_star, rc, x_smooth, P_smooth, bill.session,
                      beta_mu, beta_sigma)

  expect_equal(res$alpha[1], alpha_true, tolerance = 0.05)
  expect_equal(res$beta[1, 1], beta_true, tolerance = 0.05)
})

test_that("m_step_items: output dimensions correct for K=2", {
  N <- 10; J <- 5; K <- 2; T_total <- 3

  set.seed(1)
  rc <- matrix(sample(c(-1L, 0L, 1L), N * J, replace = TRUE), nrow = N)
  y_star <- matrix(NA_real_, nrow = N, ncol = J)
  y_star[rc != 0L] <- rnorm(sum(rc != 0L))

  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))
  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) 0.1 * diag(K))
  })

  bill.session <- sample(0:(T_total - 1), J, replace = TRUE)
  beta_mu <- rep(0, K + 1)
  beta_sigma <- 25 * diag(K + 1)

  res <- m_step_items(y_star, rc, x_smooth, P_smooth, bill.session,
                      beta_mu, beta_sigma)

  expect_length(res$alpha, J)
  expect_equal(dim(res$beta), c(J, K))
})

test_that("m_step_items: voters_by_item yields identical results", {
  N <- 30; J <- 7; K <- 2; T_total <- 2

  set.seed(11)
  rc <- matrix(sample(c(-1L, 0L, 1L), N * J, replace = TRUE), nrow = N)
  y_star <- matrix(NA_real_, nrow = N, ncol = J)
  y_star[rc != 0L] <- rnorm(sum(rc != 0L))

  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))
  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) 0.1 * diag(K))
  })

  bill.session <- sample(0:(T_total - 1), J, replace = TRUE)
  beta_mu <- rep(0, K + 1)
  beta_sigma <- 25 * diag(K + 1)

  voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))

  res_default <- m_step_items(y_star, rc, x_smooth, P_smooth, bill.session,
                              beta_mu, beta_sigma)
  res_cached <- m_step_items(y_star, rc, x_smooth, P_smooth, bill.session,
                             beta_mu, beta_sigma,
                             voters_by_item = voters_by_item)

  expect_equal(res_default$alpha, res_cached$alpha, tolerance = 1e-12)
  expect_equal(res_default$beta, res_cached$beta, tolerance = 1e-12)
})

test_that("m_step_items: prior shrinks estimates toward prior mean", {
  # With very tight prior, estimates should be close to prior mean regardless of data
  K <- 2; N <- 20; J <- 1; T_total <- 1

  set.seed(7)
  rc <- matrix(sample(c(-1L, 1L), N, replace = TRUE), nrow = N, ncol = J)
  y_star <- matrix(rnorm(N, mean = 5), nrow = N, ncol = J)  # data far from 0

  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))
  P_smooth <- lapply(1:N, function(i) list(0.1 * diag(K)))

  bill.session <- 0L
  beta_mu <- c(10, 10, 10)  # prior mean far from 0
  beta_sigma <- 0.001 * diag(K + 1)  # very tight prior

  res <- m_step_items(y_star, rc, x_smooth, P_smooth, bill.session,
                      beta_mu, beta_sigma)

  # Should be very close to prior mean
  expect_equal(res$alpha[1], 10, tolerance = 0.5)
  expect_equal(res$beta[1, ], c(10, 10), tolerance = 0.5)
})

# ============================================================
# Tests for m_step_Omega()
# ============================================================

test_that("m_step_Omega: known constant innovations", {
  # If x_t - x_{t-1} = c(1, 0) for all i, t, with no uncertainty,
  # then Omega should be ~ diag(1, 0) + ridge
  K <- 2; N <- 10; T_total <- 4

  x_smooth <- array(0, dim = c(N, K, T_total))
  for (t in 2:T_total) {
    x_smooth[, 1, t] <- x_smooth[, 1, t - 1] + 1.0  # dim1 moves by 1
    x_smooth[, 2, t] <- x_smooth[, 2, t - 1]         # dim2 stays
  }

  # Zero uncertainty
  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) matrix(0, K, K))
  })

  P_lag <- lapply(1:N, function(i) {
    lapply(1:(T_total - 1), function(s) matrix(0, K, K))
  })

  startlegis <- rep(0L, N)
  endlegis   <- rep(T_total - 1L, N)

  Omega <- m_step_Omega(x_smooth, P_smooth, P_lag, startlegis, endlegis,
                        ridge = 0)

  # Should be ~ diag(1, 0)
  expect_equal(Omega[1, 1], 1.0, tolerance = 1e-10)
  expect_equal(Omega[2, 2], 0.0, tolerance = 1e-10)
  expect_equal(Omega[1, 2], 0.0, tolerance = 1e-10)
})

test_that("m_step_Omega: diagonal_only option", {
  K <- 2; N <- 5; T_total <- 3

  set.seed(55)
  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))

  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) 0.1 * diag(K))
  })
  P_lag <- lapply(1:N, function(i) {
    lapply(1:(T_total - 1), function(s) 0.05 * diag(K))
  })

  startlegis <- rep(0L, N)
  endlegis   <- rep(T_total - 1L, N)

  Omega <- m_step_Omega(x_smooth, P_smooth, P_lag, startlegis, endlegis,
                        diagonal_only = TRUE)

  # Off-diagonal should be zero
  expect_equal(Omega[1, 2], 0, tolerance = 1e-12)
  expect_equal(Omega[2, 1], 0, tolerance = 1e-12)
  # Diagonal should be positive
  expect_true(Omega[1, 1] > 0)
  expect_true(Omega[2, 2] > 0)
})

test_that("m_step_Omega: symmetric and positive definite", {
  K <- 3; N <- 8; T_total <- 5

  set.seed(77)
  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))

  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) 0.1 * diag(K))
  })
  P_lag <- lapply(1:N, function(i) {
    lapply(1:(T_total - 1), function(s) 0.05 * diag(K))
  })

  startlegis <- rep(0L, N)
  endlegis   <- rep(T_total - 1L, N)

  Omega <- m_step_Omega(x_smooth, P_smooth, P_lag, startlegis, endlegis)

  # Symmetric
  expect_equal(Omega, t(Omega), tolerance = 1e-12)

  # Positive definite
  eigs <- eigen(Omega, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigs > 0))
})

test_that("m_step_Omega: handles countries with T_active < 2", {
  K <- 2; N <- 3; T_total <- 4

  set.seed(33)
  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))

  P_smooth <- lapply(1:N, function(i) {
    lapply(1:T_total, function(t) 0.1 * diag(K))
  })
  P_lag <- lapply(1:N, function(i) {
    lapply(1:(T_total - 1), function(s) 0.05 * diag(K))
  })

  # Country 3 has only 1 period (no transitions)
  startlegis <- c(0L, 0L, 2L)
  endlegis   <- c(3L, 3L, 2L)

  # Should not error, should use only countries 1 and 2
  Omega <- m_step_Omega(x_smooth, P_smooth, P_lag, startlegis, endlegis)

  expect_equal(dim(Omega), c(K, K))
  expect_true(all(is.finite(Omega)))
})
