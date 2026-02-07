# Sequential (item-by-item) Kalman filter-smoother
# Drop-in replacement for kalman_smoother_country in scripts/R/kalman.R

kalman_smoother_country <- function(y_star_i, item_indices, alpha, beta,
                                    bill.session, mu0, Sigma0, Omega,
                                    start_t, end_t, T_total) {
  K <- length(mu0)
  T_active <- end_t - start_t + 1L
  I_K <- diag(K)

  # Pre-group items by period for this country
  items_by_period <- vector("list", T_active)
  for (idx in seq_along(item_indices)) {
    j <- item_indices[idx]
    t_item <- bill.session[j]  # 0-indexed
    local_t <- t_item - start_t + 1L
    if (local_t >= 1L && local_t <= T_active) {
      items_by_period[[local_t]] <- c(items_by_period[[local_t]], idx)
    }
  }

  # Storage for forward pass
  x_filt <- matrix(0, nrow = K, ncol = T_active)
  P_filt <- vector("list", T_active)
  x_pred_store <- matrix(0, nrow = K, ncol = T_active)
  P_pred_store <- vector("list", T_active)

  # ---- Forward Pass (Kalman Filter) ----
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
      # Sequential update: process each observation one at a time
      obs_items <- item_indices[obs_idx]
      for (k in seq_along(obs_items)) {
        j <- obs_items[k]
        beta_j <- beta[j, ]

        innovation <- y_star_i[obs_idx[k]] - alpha[j] - sum(beta_j * x_hat)
        S <- as.numeric(crossprod(beta_j, P_hat %*% beta_j)) + 1.0
        K_gain <- (P_hat %*% beta_j) / S

        x_hat <- x_hat + as.numeric(K_gain) * innovation

        # Joseph form for numerical stability
        IKb <- I_K - K_gain %*% t(beta_j)
        P_hat <- IKb %*% P_hat %*% t(IKb) + K_gain %*% t(K_gain)
        P_hat <- (P_hat + t(P_hat)) / 2
      }
    }

    x_filt[, s] <- x_hat
    P_filt[[s]] <- P_hat
  }

  # ---- Backward Pass (RTS Smoother) ----
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

  # ---- Lag-one cross-covariance ----
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
