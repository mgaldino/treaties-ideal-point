#' Kalman Filter-Smoother for K-Dimensional Dynamic IRT
#'
#' Implements sequential Kalman filter (forward) + Rauch-Tung-Striebel smoother
#' (backward) for a single country's ideal point trajectory.
#'
#' State equation:  x_t = x_{t-1} + omega_t,  omega_t ~ N(0, Omega)
#' Obs equation:    y*_ij = alpha_j + beta_j' x_t + eps,  eps ~ N(0, 1)
#'
#' Reference: Section 6 of docs/estimation_plan_2d.md
#'            Rauch, Tung & Striebel (1965), AIAA Journal 3(8).


#' Kalman Filter-Smoother for a single country
#'
#' @param y_star_i     Numeric vector. Pseudo-observations for country i
#'                     (only non-missing entries).
#' @param item_indices Integer vector. Column indices into 1:J for items
#'                     where rc[i,j] != 0 (same order as y_star_i).
#' @param alpha        Numeric vector length J. Item intercepts.
#' @param beta         J x K numeric matrix. Item discrimination vectors.
#' @param bill.session Integer vector length J. 0-indexed period for each item.
#' @param mu0          Numeric vector length K. Prior mean for initial state.
#' @param Sigma0       K x K numeric matrix. Prior covariance for initial state.
#' @param Omega        K x K numeric matrix. Evolution covariance.
#' @param start_t      Integer. First active period (0-indexed).
#' @param end_t        Integer. Last active period (0-indexed).
#' @param T_total      Integer. Total number of periods.
#'
#' @return List with components:
#'   \item{x_smooth}{K x T_active matrix. Smoothed means.}
#'   \item{P_smooth}{List of T_active K x K matrices. Smoothed covariances.}
#'   \item{P_lag}{List of (T_active - 1) K x K matrices. Lag-one cross-covariances
#'         Cov(x_t, x_{t-1} | Y).}
#'   \item{x_pred}{K x T_active matrix. Predicted means (before update).}
#'   \item{P_pred}{List of T_active K x K matrices. Predicted covariances.}
kalman_smoother_country <- function(y_star_i, item_indices, alpha, beta,
                                    bill.session, mu0, Sigma0, Omega,
                                    start_t, end_t, T_total) {
  K <- length(mu0)
  T_active <- end_t - start_t + 1L
  I_K <- diag(K)

  # Pre-group items by period for this country (vectorized)
  # bill.session is 0-indexed; map to local period indices in 1..T_active
  items_by_period <- vector("list", T_active)
  if (length(item_indices) > 0) {
    local_t <- bill.session[item_indices] - start_t + 1L
    valid <- local_t >= 1L & local_t <= T_active
    if (any(!valid)) {
      item_indices <- item_indices[valid]
      y_star_i <- y_star_i[valid]
      local_t <- local_t[valid]
    }
    if (length(item_indices) > 0) {
      tmp <- split(seq_along(item_indices), local_t)
      items_by_period[as.integer(names(tmp))] <- tmp
    }
  }

  # Storage for forward pass
  x_filt <- matrix(0, nrow = K, ncol = T_active)  # filtered means
  P_filt <- vector("list", T_active)                # filtered covariances
  x_pred_store <- matrix(0, nrow = K, ncol = T_active)
  P_pred_store <- vector("list", T_active)

  # ---- Forward Pass (Kalman Filter) ----

  for (s in seq_len(T_active)) {
    if (s == 1L) {
      # Initialize from prior
      x_hat <- mu0
      P_hat <- Sigma0
    } else {
      # Prediction step: random walk
      x_hat <- x_filt[, s - 1L]
      P_hat <- P_filt[[s - 1L]] + Omega
    }

    # Store predicted values (needed for smoother)
    x_pred_store[, s] <- x_hat
    P_pred_store[[s]] <- P_hat

    # Update for all items in this period (block; R = I)
    obs_idx <- items_by_period[[s]]
    if (length(obs_idx) > 0) {
      if (length(obs_idx) == 1L) {
        # Fast path: single observation (identical to sequential update)
        j <- item_indices[obs_idx]
        beta_j <- beta[j, ]  # K x 1 (as vector)

        innovation <- y_star_i[obs_idx] - alpha[j] - sum(beta_j * x_hat)
        S <- as.numeric(crossprod(beta_j, P_hat %*% beta_j)) + 1.0
        K_gain <- (P_hat %*% beta_j) / S

        x_hat <- x_hat + as.numeric(K_gain) * innovation

        IKb <- I_K - K_gain %*% t(beta_j)
        P_hat <- IKb %*% P_hat %*% t(IKb) + K_gain %*% t(K_gain)
        P_hat <- (P_hat + t(P_hat)) / 2
      } else {
        # Block update for all observations in this period (R = I)
        obs_items <- item_indices[obs_idx]
        H <- beta[obs_items, , drop = FALSE]  # n_t x K
        y_t <- y_star_i[obs_idx]
        alpha_t <- alpha[obs_items]
        y_t <- y_t - alpha_t

        # Posterior: P = (P^{-1} + H'H)^{-1}, x = P (P^{-1} x + H' y)
        P_inv <- tryCatch(solve(P_hat), error = function(e) NULL)
        if (is.null(P_inv)) {
          # Regularize with small ridge if P_hat is singular
          P_inv <- solve(P_hat + 1e-6 * diag(nrow(P_hat)))
        }
        A <- P_inv + crossprod(H)
        rhs <- as.numeric(P_inv %*% x_hat + crossprod(H, y_t))

        chol_A <- tryCatch(chol(A), error = function(e) NULL)
        if (!is.null(chol_A)) {
          x_hat <- solve(chol_A, solve(t(chol_A), rhs))
          P_hat <- chol2inv(chol_A)
        } else {
          x_hat <- solve(A, rhs)
          P_hat <- solve(A)
        }

        P_hat <- (P_hat + t(P_hat)) / 2
      }
    }

    x_filt[, s] <- x_hat
    P_filt[[s]] <- P_hat
  }

  # ---- Backward Pass (RTS Smoother, Section 6.2) ----

  x_smooth <- matrix(0, nrow = K, ncol = T_active)
  P_smooth <- vector("list", T_active)
  L_store  <- vector("list", T_active)  # smoother gains

  # Initialize: last period smoothed = filtered
  x_smooth[, T_active] <- x_filt[, T_active]
  P_smooth[[T_active]] <- P_filt[[T_active]]

  if (T_active > 1L) {
    for (s in (T_active - 1L):1L) {
      # Predicted covariance at s+1 (from forward pass)
      P_pred_next <- P_pred_store[[s + 1L]]

      # Smoother gain: L_t = P_{t|t} * P_{t+1|t}^{-1}
      L_t <- P_filt[[s]] %*% solve(P_pred_next)

      # Smoothed mean
      x_smooth[, s] <- x_filt[, s] +
        L_t %*% (x_smooth[, s + 1L] - x_pred_store[, s + 1L])

      # Smoothed covariance
      P_smooth[[s]] <- P_filt[[s]] +
        L_t %*% (P_smooth[[s + 1L]] - P_pred_next) %*% t(L_t)

      # Enforce symmetry
      P_smooth[[s]] <- (P_smooth[[s]] + t(P_smooth[[s]])) / 2

      L_store[[s]] <- L_t
    }
  }

  # ---- Lag-one cross-covariance (Section 6.2) ----
  # Cov(x_t, x_{t-1} | Y) = L_{t-1} P_{t|T}
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
