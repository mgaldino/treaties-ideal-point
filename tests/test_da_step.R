# Tests for da_step.R
# Run: testthat::test_file("tests/test_da_step.R")

library(testthat)

# Source the module
source(file.path("..", "scripts", "R", "da_step.R"))

# ============================================================
# Tests for truncnorm_moments()
# ============================================================

test_that("truncnorm_moments: mu=0 gives known analytical values", {
  # TN(0, 1, 0, +Inf): E = phi(0)/Phi(0) = dnorm(0)/0.5 = sqrt(2/pi)
  expected_mean_pos <- sqrt(2 / pi)  # ~0.7979
  # Var = 1 - 0*lambda - lambda^2 = 1 - 2/pi

  expected_var_pos <- 1 - 2 / pi     # ~0.3634

  res <- truncnorm_moments(mu = 0, y = 1L)
  expect_equal(res$y_star, expected_mean_pos, tolerance = 1e-10)
  expect_equal(res$y_star_var, expected_var_pos, tolerance = 1e-10)

  # TN(0, 1, -Inf, 0): E = -sqrt(2/pi)
  res_neg <- truncnorm_moments(mu = 0, y = -1L)
  expect_equal(res_neg$y_star, -expected_mean_pos, tolerance = 1e-10)
  expect_equal(res_neg$y_star_var, expected_var_pos, tolerance = 1e-10)
})

test_that("truncnorm_moments: symmetry between y=+1 and y=-1", {
  mu_vals <- c(-2, -1, 0, 0.5, 1, 2)

  for (mu in mu_vals) {
    res_pos <- truncnorm_moments(mu = mu, y = 1L)
    res_neg <- truncnorm_moments(mu = -mu, y = -1L)

    # E[y* | y=+1, mu] = -E[y* | y=-1, -mu] by symmetry
    expect_equal(res_pos$y_star, -res_neg$y_star, tolerance = 1e-10,
                 label = sprintf("mean symmetry at mu=%.1f", mu))
    # Variances should be equal
    expect_equal(res_pos$y_star_var, res_neg$y_star_var, tolerance = 1e-10,
                 label = sprintf("var symmetry at mu=%.1f", mu))
  }
})

test_that("truncnorm_moments: y=+1 always gives positive E[y*]", {
  mu_vals <- c(-5, -3, -1, 0, 1, 3, 5)
  res <- truncnorm_moments(mu = mu_vals, y = rep(1L, length(mu_vals)))
  expect_true(all(res$y_star > 0))
})

test_that("truncnorm_moments: y=-1 always gives negative E[y*]", {
  mu_vals <- c(-5, -3, -1, 0, 1, 3, 5)
  res <- truncnorm_moments(mu = mu_vals, y = rep(-1L, length(mu_vals)))
  expect_true(all(res$y_star < 0))
})

test_that("truncnorm_moments: variance is in (0, 1]", {
  mu_vals <- c(-10, -5, -2, 0, 2, 5, 10)
  for (y_val in c(1L, -1L)) {
    res <- truncnorm_moments(mu = mu_vals, y = rep(y_val, length(mu_vals)))
    expect_true(all(res$y_star_var > 0),
                label = sprintf("var > 0 for y=%d", y_val))
    expect_true(all(res$y_star_var <= 1),
                label = sprintf("var <= 1 for y=%d", y_val))
  }
})

test_that("truncnorm_moments: large mu, numerical stability", {
  # Large positive mu with y=+1: almost no truncation, E[y*] ~ mu
  res <- truncnorm_moments(mu = 10, y = 1L)
  expect_true(res$y_star > 9.9)
  expect_true(is.finite(res$y_star))
  expect_true(is.finite(res$y_star_var))

  # Large negative mu with y=+1: heavy truncation, but should not NaN
  res2 <- truncnorm_moments(mu = -10, y = 1L)
  expect_true(res2$y_star > 0)
  expect_true(is.finite(res2$y_star))
  expect_true(is.finite(res2$y_star_var))

  # Large positive mu with y=-1: heavy truncation
  res3 <- truncnorm_moments(mu = 10, y = -1L)
  expect_true(res3$y_star < 0)
  expect_true(is.finite(res3$y_star))
  expect_true(is.finite(res3$y_star_var))
})

test_that("truncnorm_moments: vectorized input", {
  mu <- c(-1, 0, 1, 2)
  y  <- c(1L, -1L, 1L, -1L)
  res <- truncnorm_moments(mu, y)

  expect_length(res$y_star, 4)
  expect_length(res$y_star_var, 4)

  # Check signs
  expect_true(res$y_star[1] > 0)   # y=+1
  expect_true(res$y_star[2] < 0)   # y=-1
  expect_true(res$y_star[3] > 0)   # y=+1
  expect_true(res$y_star[4] < 0)   # y=-1
})

# ============================================================
# Tests for da_step()
# ============================================================

test_that("da_step: correct dimensions", {
  N <- 5; J <- 8; K <- 2; T_total <- 3

  rc <- matrix(sample(c(-1L, 0L, 1L), N * J, replace = TRUE), nrow = N)
  alpha <- rnorm(J)
  beta <- matrix(rnorm(J * K), nrow = J)
  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))
  bill.session <- sample(0:(T_total - 1), J, replace = TRUE)

  res <- da_step(rc, alpha, beta, x_smooth, bill.session)

  expect_equal(dim(res$y_star), c(N, J))
  expect_equal(dim(res$y_star_var), c(N, J))
})

test_that("da_step: NA where rc == 0, non-NA otherwise", {
  N <- 4; J <- 6; K <- 2; T_total <- 2

  rc <- matrix(c(1L, -1L, 0L, 1L,
                 0L,  1L, -1L, 0L,
                 1L,  0L,  1L, -1L,
                 -1L, 1L,  0L,  1L,
                 0L,  -1L, 1L, 0L,
                 1L,  1L, -1L, -1L), nrow = N, ncol = J)
  alpha <- rnorm(J)
  beta <- matrix(rnorm(J * K), nrow = J)
  x_smooth <- array(rnorm(N * K * T_total), dim = c(N, K, T_total))
  bill.session <- rep(c(0L, 1L), each = J %/% 2)

  res <- da_step(rc, alpha, beta, x_smooth, bill.session)

  # NA exactly where rc == 0
  expect_true(all(is.na(res$y_star[rc == 0L])))
  expect_true(all(!is.na(res$y_star[rc != 0L])))
})

test_that("da_step: sign consistency with votes", {
  N <- 10; J <- 20; K <- 2; T_total <- 3

  # All positive votes
  rc <- matrix(1L, nrow = N, ncol = J)
  alpha <- rep(0, J)
  beta <- matrix(0.5, nrow = J, ncol = K)
  x_smooth <- array(0, dim = c(N, K, T_total))
  bill.session <- sample(0:(T_total - 1), J, replace = TRUE)

  res <- da_step(rc, alpha, beta, x_smooth, bill.session)
  expect_true(all(res$y_star > 0, na.rm = TRUE))

  # All negative votes
  rc_neg <- matrix(-1L, nrow = N, ncol = J)
  res_neg <- da_step(rc_neg, alpha, beta, x_smooth, bill.session)
  expect_true(all(res_neg$y_star < 0, na.rm = TRUE))
})
