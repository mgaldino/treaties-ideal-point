#' M-Step for K-Dimensional Dynamic Probit IRT (P=0 variant)
#'
#' Updates item parameters (alpha, beta) via penalized WLS, optionally
#' ignoring P_smooth (mean-field approximation).
#'
#' Reference: Sections 7-8 of docs/estimation_plan_2d.md

#' M-Step: Update item parameters (P=0 optional)
#'
#' @param y_star       N x J numeric matrix. Pseudo-observations from DA step.
#' @param rc           N x J integer matrix. Original votes.
#' @param x_smooth     N x K x T numeric array. Smoothed ideal point means.
#' @param P_smooth     List of length N. Each element is a list of T K x K matrices.
#' @param bill.session Integer vector length J. 0-indexed period for each item.
#' @param beta_mu      Numeric vector length (K+1). Prior mean for (alpha, beta)'.
#' @param beta_sigma   (K+1) x (K+1) numeric matrix. Prior covariance.
#' @param voters_by_item Optional list of voters per item (precomputed).
#' @param use_P_smooth Logical. If FALSE, ignores Var[x|Y] (P=0 approximation).
#'
#' @return List with components:
#'   \item{alpha}{Numeric vector length J. Updated item intercepts.}
#'   \item{beta}{J x K numeric matrix. Updated item discriminations.}
m_step_items_p0 <- function(y_star, rc, x_smooth, P_smooth,
                            bill.session, beta_mu, beta_sigma,
                            voters_by_item = NULL,
                            use_P_smooth = TRUE) {
  N <- nrow(rc)
  J <- ncol(rc)
  K <- dim(x_smooth)[2]
  Kp1 <- K + 1L

  alpha_new <- numeric(J)
  beta_new  <- matrix(0, nrow = J, ncol = K)

  # Prior precision
  Sigma_beta_inv <- solve(beta_sigma)
  Sigma_beta_inv_mu <- Sigma_beta_inv %*% beta_mu

  # Precompute voters per item if not provided
  if (is.null(voters_by_item)) {
    voters_by_item <- lapply(seq_len(J), function(j) which(rc[, j] != 0L))
  }

  for (j in seq_len(J)) {
    voters <- voters_by_item[[j]]
    if (length(voters) == 0) next

    t_j <- bill.session[j] + 1L

    X <- x_smooth[voters, , t_j, drop = FALSE]
    dim(X) <- c(length(voters), K)
    Y <- y_star[voters, j]

    # Sum of P_it over voters (K x K)
    if (use_P_smooth) {
      P_sum <- matrix(0, nrow = K, ncol = K)
      for (i in voters) {
        P_sum <- P_sum + P_smooth[[i]][[t_j]]
      }
    } else {
      P_sum <- matrix(0, nrow = K, ncol = K)
    }

    count  <- length(voters)
    sum_x  <- colSums(X)
    sum_y  <- sum(Y)
    sum_yx <- as.numeric(crossprod(X, Y))
    sum_xx <- crossprod(X)

    Sigma_zz <- matrix(0, nrow = Kp1, ncol = Kp1)
    Sigma_zz[1, 1] <- count
    Sigma_zz[1, 2:Kp1] <- sum_x
    Sigma_zz[2:Kp1, 1] <- sum_x
    Sigma_zz[2:Kp1, 2:Kp1] <- P_sum + sum_xx

    Sigma_zy <- c(sum_y, sum_yx)

    A <- Sigma_beta_inv + Sigma_zz
    b <- Sigma_beta_inv_mu + Sigma_zy
    gamma_hat <- solve(A, b)

    alpha_new[j]  <- gamma_hat[1]
    beta_new[j, ] <- gamma_hat[2:Kp1]
  }

  list(alpha = alpha_new, beta = beta_new)
}

#' M-Step: Update evolution covariance Omega
#'
#' Estimates Omega from the expected innovations E[(x_t - x_{t-1})(x_t - x_{t-1})' | Y].
#'
#' @param x_smooth     N x K x T numeric array. Smoothed ideal point means.
#' @param P_smooth     List of length N. Each element is a list of T K x K matrices.
#' @param P_lag        List of length N. Each element is a list of (T-1) K x K matrices.
#' @param startlegis   Integer vector length N. 0-indexed first active period.
#' @param endlegis     Integer vector length N. 0-indexed last active period.
#' @param diagonal_only Logical. If TRUE, estimate diagonal Omega only.
#' @param ridge        Numeric. Small ridge for positive definiteness (default 1e-6).
#'
#' @return K x K numeric matrix. Updated Omega.
m_step_Omega <- function(x_smooth, P_smooth, P_lag,
                         startlegis, endlegis,
                         diagonal_only = FALSE, ridge = 1e-6) {
  N <- nrow(x_smooth)
  K <- dim(x_smooth)[2]

  Omega_sum <- matrix(0, nrow = K, ncol = K)
  total_transitions <- 0L

  for (i in seq_len(N)) {
    si <- startlegis[i]  # 0-indexed
    ei <- endlegis[i]    # 0-indexed
    T_active_i <- ei - si + 1L

    if (T_active_i < 2L) next

    for (s in 2L:T_active_i) {
      t_curr <- si + s       # 1-indexed
      t_prev <- si + s - 1L

      x_curr <- x_smooth[i, , t_curr]
      x_prev <- x_smooth[i, , t_prev]
      dx <- x_curr - x_prev

      P_curr <- P_smooth[[i]][[t_curr]]
      P_prev <- P_smooth[[i]][[t_prev]]
      P_cross <- P_lag[[i]][[s - 1L]]

      E_innov <- tcrossprod(dx) + P_curr - P_cross - t(P_cross) + P_prev

      Omega_sum <- Omega_sum + E_innov
      total_transitions <- total_transitions + 1L
    }
  }

  if (total_transitions == 0L) {
    return(ridge * diag(K))
  }

  Omega_hat <- Omega_sum / total_transitions

  if (diagonal_only) {
    Omega_hat <- diag(diag(Omega_hat))
  }

  Omega_hat <- Omega_hat + ridge * diag(K)
  Omega_hat <- (Omega_hat + t(Omega_hat)) / 2

  Omega_hat
}
