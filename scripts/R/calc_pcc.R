#!/usr/bin/env Rscript
# Compute Percent Correctly Classified (PCC) by dimension, a la Poole & Rosenthal

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

  n_correct_both <- 0L
  n_correct_dim1 <- 0L
  n_correct_dim2 <- 0L
  n_total <- 0L

  for (j in 1:J) {
    t_idx <- bill_session[j] + 1L
    a_j <- alpha[j]
    b_j <- beta[j, ]  # K-vector

    for (i in 1:N) {
      y_obs <- Y[i, j]
      if (y_obs == 0L) next

      x_it <- x[i, , t_idx]

      # Both dimensions
      eta_both <- a_j + sum(b_j * x_it)
      # Dim 1 only
      eta_dim1 <- a_j + b_j[1] * x_it[1]
      # Dim 2 only
      eta_dim2 <- a_j + b_j[2] * x_it[2]

      n_total <- n_total + 1L
      if ((eta_both > 0) == (y_obs == 1)) n_correct_both <- n_correct_both + 1L
      if ((eta_dim1 > 0) == (y_obs == 1)) n_correct_dim1 <- n_correct_dim1 + 1L
      if ((eta_dim2 > 0) == (y_obs == 1)) n_correct_dim2 <- n_correct_dim2 + 1L
    }
  }

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
