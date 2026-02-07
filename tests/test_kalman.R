# Tests for kalman.R
# Run: testthat::test_file("tests/test_kalman.R")

library(testthat)

source(file.path("..", "scripts", "R", "kalman.R"))

# ============================================================
# Helper: create a minimal test scenario
# ============================================================

make_test_data <- function(K = 2, T_total = 3, n_items = 6) {
  alpha <- rnorm(n_items)
  beta  <- matrix(rnorm(n_items * K), nrow = n_items, ncol = K)
  bill.session <- rep(0:(T_total - 1), length.out = n_items)
  mu0 <- rep(0, K)
  Sigma0 <- diag(K)
  Omega <- 0.1 * diag(K)

  list(alpha = alpha, beta = beta, bill.session = bill.session,
       mu0 = mu0, Sigma0 = Sigma0, Omega = Omega,
       K = K, T_total = T_total, n_items = n_items)
}

# Reference implementation: sequential Kalman updates (pre-block refactor)
kalman_smoother_country_seq <- function(y_star_i, item_indices, alpha, beta,
                                        bill.session, mu0, Sigma0, Omega,
                                        start_t, end_t, T_total) {
  K <- length(mu0)
  T_active <- end_t - start_t + 1L
  I_K <- diag(K)

  items_by_period <- vector("list", T_active)
  for (idx in seq_along(item_indices)) {
    j <- item_indices[idx]
    t_item <- bill.session[j]
    local_t <- t_item - start_t + 1L
    if (local_t >= 1L && local_t <= T_active) {
      items_by_period[[local_t]] <- c(items_by_period[[local_t]], idx)
    }
  }

  x_filt <- matrix(0, nrow = K, ncol = T_active)
  P_filt <- vector("list", T_active)
  x_pred_store <- matrix(0, nrow = K, ncol = T_active)
  P_pred_store <- vector("list", T_active)

  for (s in seq_len(T_active)) {
    if (s == 1L) {
      x_hat <- mu0
      P_hat <- Sigma0
    } else {
      x_hat <- x_filt[, s - 1L]
      P_hat <- P_filt[[s - 1L]] + Omega
    }

    x_pred_store[, s] <- x_hat
    P_pred_store[[s]] <- P_hat

    obs_idx <- items_by_period[[s]]
    if (length(obs_idx) > 0) {
      for (idx in obs_idx) {
        j <- item_indices[idx]
        beta_j <- beta[j, ]

        innovation <- y_star_i[idx] - alpha[j] - sum(beta_j * x_hat)
        S <- as.numeric(crossprod(beta_j, P_hat %*% beta_j)) + 1.0
        K_gain <- (P_hat %*% beta_j) / S

        x_hat <- x_hat + as.numeric(K_gain) * innovation

        IKb <- I_K - K_gain %*% t(beta_j)
        P_hat <- IKb %*% P_hat %*% t(IKb) + K_gain %*% t(K_gain)
        P_hat <- (P_hat + t(P_hat)) / 2
      }
    }

    x_filt[, s] <- x_hat
    P_filt[[s]] <- P_hat
  }

  x_smooth <- matrix(0, nrow = K, ncol = T_active)
  P_smooth <- vector("list", T_active)
  L_store  <- vector("list", T_active)

  x_smooth[, T_active] <- x_filt[, T_active]
  P_smooth[[T_active]] <- P_filt[[T_active]]

  if (T_active > 1L) {
    for (s in (T_active - 1L):1L) {
      P_pred_next <- P_pred_store[[s + 1L]]
      L_t <- P_filt[[s]] %*% solve(P_pred_next)
      x_smooth[, s] <- x_filt[, s] +
        L_t %*% (x_smooth[, s + 1L] - x_pred_store[, s + 1L])
      P_smooth[[s]] <- P_filt[[s]] +
        L_t %*% (P_smooth[[s + 1L]] - P_pred_next) %*% t(L_t)
      P_smooth[[s]] <- (P_smooth[[s]] + t(P_smooth[[s]])) / 2
      L_store[[s]] <- L_t
    }
  }

  P_lag <- vector("list", max(T_active - 1L, 0L))
  if (T_active > 1L) {
    for (s in 2L:T_active) {
      P_lag[[s - 1L]] <- L_store[[s - 1L]] %*% P_smooth[[s]]
    }
  }

  list(
    x_smooth = x_smooth,
    P_smooth = P_smooth,
    P_lag    = P_lag,
    x_pred   = x_pred_store,
    P_pred   = P_pred_store
  )
}

# ============================================================
# Tests for kalman_smoother_country()
# ============================================================

test_that("kalman: no observations -> smoothed equals prior propagated by RW", {
  K <- 2; T_total <- 4
  mu0 <- c(1, -1)
  Sigma0 <- diag(K) * 0.5
  Omega <- diag(K) * 0.1

  # No items observed
  alpha <- c(0, 0)
  beta  <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  bill.session <- c(0L, 1L)

  # Empty observations
  ks <- kalman_smoother_country(
    y_star_i     = numeric(0),
    item_indices = integer(0),
    alpha        = alpha,
    beta         = beta,
    bill.session = bill.session,
    mu0          = mu0,
    Sigma0       = Sigma0,
    Omega        = Omega,
    start_t      = 0L,
    end_t        = 3L,
    T_total      = T_total
  )

  # With no observations, smoothed mean at all periods should equal mu0
  # (random walk with no data => smoother pulls back to prior)
  for (s in 1:T_total) {
    expect_equal(ks$x_smooth[, s], mu0, tolerance = 1e-10,
                 label = sprintf("smoothed mean at t=%d", s))
  }

  # Covariances should grow: P_t = Sigma0 + (t-1)*Omega
  for (s in 1:T_total) {
    expected_P <- Sigma0 + (s - 1) * Omega
    expect_equal(ks$P_smooth[[s]], expected_P, tolerance = 1e-10,
                 label = sprintf("smoothed cov at t=%d", s))
  }
})

test_that("kalman: single observation updates state correctly", {
  K <- 2
  mu0 <- c(0, 0)
  Sigma0 <- diag(K)
  Omega <- diag(K) * 0.1

  # One item in period 0
  alpha <- 0
  beta  <- matrix(c(1, 0), nrow = 1, ncol = K)  # loads on dim 1 only
  bill.session <- 0L

  y_star_val <- 2.0  # pseudo-observation

  ks <- kalman_smoother_country(
    y_star_i     = y_star_val,
    item_indices = 1L,
    alpha        = alpha,
    beta         = beta,
    bill.session = bill.session,
    mu0          = mu0,
    Sigma0       = Sigma0,
    Omega        = Omega,
    start_t      = 0L,
    end_t        = 0L,
    T_total      = 1L
  )

  # Analytical Kalman update for single obs:
  # beta_j = c(1, 0), P = I, R = 1
  # S = beta' P beta + 1 = 1 + 1 = 2
  # K_gain = P beta / S = c(1,0)/2 = c(0.5, 0)
  # x_hat = mu0 + K_gain * (y* - alpha - beta'mu0) = c(0,0) + c(0.5,0)*2 = c(1,0)
  expect_equal(ks$x_smooth[, 1], c(1, 0), tolerance = 1e-10)
})

test_that("kalman: output dimensions are correct", {
  td <- make_test_data(K = 2, T_total = 4, n_items = 12)

  # Simulate pseudo-observations
  set.seed(42)
  y_star_i <- rnorm(td$n_items)
  item_indices <- seq_len(td$n_items)

  ks <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = item_indices,
    alpha        = td$alpha,
    beta         = td$beta,
    bill.session = td$bill.session,
    mu0          = td$mu0,
    Sigma0       = td$Sigma0,
    Omega        = td$Omega,
    start_t      = 0L,
    end_t        = 3L,
    T_total      = td$T_total
  )

  # x_smooth: K x T_active
  expect_equal(dim(ks$x_smooth), c(2, 4))

  # P_smooth: list of T_active K x K matrices
  expect_length(ks$P_smooth, 4)
  for (s in 1:4) {
    expect_equal(dim(ks$P_smooth[[s]]), c(2, 2))
  }

  # P_lag: list of (T_active - 1) K x K matrices
  expect_length(ks$P_lag, 3)
  for (s in 1:3) {
    expect_equal(dim(ks$P_lag[[s]]), c(2, 2))
  }
})

test_that("kalman: block update matches sequential within tolerance", {
  td <- make_test_data(K = 2, T_total = 4, n_items = 20)

  set.seed(202)
  y_star_i <- rnorm(td$n_items)
  item_indices <- seq_len(td$n_items)

  ks_block <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = item_indices,
    alpha        = td$alpha,
    beta         = td$beta,
    bill.session = td$bill.session,
    mu0          = td$mu0,
    Sigma0       = td$Sigma0,
    Omega        = td$Omega,
    start_t      = 0L,
    end_t        = 3L,
    T_total      = td$T_total
  )

  ks_seq <- kalman_smoother_country_seq(
    y_star_i     = y_star_i,
    item_indices = item_indices,
    alpha        = td$alpha,
    beta         = td$beta,
    bill.session = td$bill.session,
    mu0          = td$mu0,
    Sigma0       = td$Sigma0,
    Omega        = td$Omega,
    start_t      = 0L,
    end_t        = 3L,
    T_total      = td$T_total
  )

  expect_equal(ks_block$x_smooth, ks_seq$x_smooth, tolerance = 1e-10)
  expect_equal(ks_block$P_smooth, ks_seq$P_smooth, tolerance = 1e-10)
  expect_equal(ks_block$P_lag, ks_seq$P_lag, tolerance = 1e-10)
})

test_that("kalman: P_smooth is symmetric positive definite", {
  td <- make_test_data(K = 3, T_total = 5, n_items = 15)

  set.seed(123)
  y_star_i <- rnorm(td$n_items)
  item_indices <- seq_len(td$n_items)

  ks <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = item_indices,
    alpha        = td$alpha,
    beta         = td$beta,
    bill.session = td$bill.session,
    mu0          = td$mu0,
    Sigma0       = td$Sigma0,
    Omega        = td$Omega,
    start_t      = 0L,
    end_t        = 4L,
    T_total      = td$T_total
  )

  for (s in seq_along(ks$P_smooth)) {
    P <- ks$P_smooth[[s]]
    # Symmetric
    expect_equal(P, t(P), tolerance = 1e-12,
                 label = sprintf("P_smooth symmetry at s=%d", s))
    # Positive definite (all eigenvalues > 0)
    eigs <- eigen(P, symmetric = TRUE, only.values = TRUE)$values
    expect_true(all(eigs > 0),
                label = sprintf("P_smooth PD at s=%d (min eig = %.2e)", s, min(eigs)))
  }
})

test_that("kalman: partial activity window works", {
  td <- make_test_data(K = 2, T_total = 6, n_items = 12)

  set.seed(99)
  # Country active only in periods 2-4 (0-indexed)
  # Items in periods 2, 3, 4
  td$bill.session <- rep(2:4, each = 4)
  y_star_i <- rnorm(td$n_items)
  item_indices <- seq_len(td$n_items)

  ks <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = item_indices,
    alpha        = td$alpha,
    beta         = td$beta,
    bill.session = td$bill.session,
    mu0          = td$mu0,
    Sigma0       = td$Sigma0,
    Omega        = td$Omega,
    start_t      = 2L,
    end_t        = 4L,
    T_total      = td$T_total
  )

  # T_active = 3 periods
  expect_equal(dim(ks$x_smooth), c(2, 3))
  expect_length(ks$P_smooth, 3)
  expect_length(ks$P_lag, 2)
})

test_that("kalman: K=1 scalar case works", {
  K <- 1
  mu0 <- 0
  Sigma0 <- matrix(1, 1, 1)
  Omega <- matrix(0.1, 1, 1)

  alpha <- c(0, 0.5)
  beta  <- matrix(c(1, 0.8), nrow = 2, ncol = 1)
  bill.session <- c(0L, 1L)

  y_star_i <- c(1.5, -0.3)
  item_indices <- 1:2

  ks <- kalman_smoother_country(
    y_star_i     = y_star_i,
    item_indices = item_indices,
    alpha        = alpha,
    beta         = beta,
    bill.session = bill.session,
    mu0          = mu0,
    Sigma0       = Sigma0,
    Omega        = Omega,
    start_t      = 0L,
    end_t        = 1L,
    T_total      = 2L
  )

  expect_equal(dim(ks$x_smooth), c(1, 2))
  expect_true(all(is.finite(ks$x_smooth)))
})
