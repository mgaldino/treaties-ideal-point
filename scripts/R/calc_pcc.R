#!/usr/bin/env Rscript
# Compute Percent Correctly Classified (PCC) by dimension, a la Poole & Rosenthal
# Vectorized implementation (no nested loops)

domains <- c("investment", "security", "environment", "human_rights", "arms_control", "intellectual_property")

results <- list()

for (domain in domains) {
  res <- readRDS(sprintf("outputs/v7_country_anchors/%s_results.rds", domain))
  flow <- readRDS(sprintf("data/processed/%s_flow_matrix.rds", domain))

  x <- res$ideal_points       # N x K x T
  alpha <- res$alpha           # J-vector
  beta <- res$beta             # J x K
  Y <- flow$rc                 # N x J, values in {-1, 0, 1}
  bill_session <- flow$bill.session  # J-vector, 0-indexed period

  N <- nrow(Y)
  J <- ncol(Y)

  # Find all non-missing observations
  obs <- which(Y != 0, arr.ind = TRUE)  # matrix with columns [row, col] = [i, j]
  i_idx <- obs[, 1]
  j_idx <- obs[, 2]
  n_total <- nrow(obs)

  # Get the period for each observation
  t_idx <- bill_session[j_idx] + 1L

  # Vectorized extraction of ideal points for each observation
  # x[i, , t] for each (i, t) pair
  x_obs <- matrix(NA_real_, nrow = n_total, ncol = ncol(beta))
  for (k in seq_len(ncol(beta))) {
    x_obs[, k] <- x[cbind(i_idx, k, t_idx)]
  }

  # Vectorized computation of linear predictors
  a_obs <- alpha[j_idx]
  b_obs <- beta[j_idx, , drop = FALSE]
  y_obs <- Y[obs]

  eta_both <- a_obs + rowSums(b_obs * x_obs)
  eta_dim1 <- a_obs + b_obs[, 1] * x_obs[, 1]
  eta_dim2 <- a_obs + b_obs[, 2] * x_obs[, 2]

  y_positive <- (y_obs == 1)
  n_correct_both <- sum((eta_both > 0) == y_positive)
  n_correct_dim1 <- sum((eta_dim1 > 0) == y_positive)
  n_correct_dim2 <- sum((eta_dim2 > 0) == y_positive)

  y_nonmissing <- Y[Y != 0]
  modal_pct <- max(mean(y_nonmissing == 1), mean(y_nonmissing == -1))

  results[[domain]] <- data.frame(
    domain = domain,
    n_obs = n_total,
    pcc_dim1 = round(100 * n_correct_dim1 / n_total, 1),
    pcc_dim2 = round(100 * n_correct_dim2 / n_total, 1),
    pcc_both = round(100 * n_correct_both / n_total, 1),
    baseline = round(100 * modal_pct, 1)
  )

  cat(sprintf("%s: dim1=%.1f%%  dim2=%.1f%%  both=%.1f%%  baseline=%.1f%%  (n=%d)\n",
              domain,
              results[[domain]]$pcc_dim1,
              results[[domain]]$pcc_dim2,
              results[[domain]]$pcc_both,
              results[[domain]]$baseline,
              n_total))
}

cat("\n")
df <- do.call(rbind, results)
print(df, row.names = FALSE)
