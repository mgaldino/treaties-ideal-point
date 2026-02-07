# Scoring utilities: log-score and Brier for probit predictions

score_metrics <- function(x, alpha, beta, bill_session, test, K = 1L, return_probs = FALSE) {
  i_idx <- test$i
  j_idx <- test$j
  y_val <- test$y

  t_idx <- bill_session[j_idx] + 1L

  # Build x array N x K x T if needed
  if (is.matrix(x) && K == 1L) {
    N <- nrow(x)
    T_periods <- ncol(x)
    x_arr <- array(NA_real_, dim = c(N, 1L, T_periods))
    for (t in seq_len(T_periods)) x_arr[, 1L, t] <- x[, t]
  } else {
    x_arr <- x
  }

  x_it <- x_arr[cbind(i_idx, rep(1L, length(i_idx)), t_idx)]

  # beta: J x K (K=1)
  if (is.matrix(beta)) {
    beta_j <- beta[j_idx, 1L]
  } else {
    beta_j <- beta[j_idx]
  }

  mu <- alpha[j_idx] + beta_j * x_it
  p <- pnorm(mu)

  # Clamp for numerical stability
  eps <- 1e-12
  p <- pmin(pmax(p, eps), 1 - eps)

  y_bin <- as.numeric(y_val == 1L)

  logscore <- sum(y_bin * log(p) + (1 - y_bin) * log(1 - p))
  brier <- mean((p - y_bin)^2)

  out <- list(
    n = length(y_val),
    logscore = logscore,
    logscore_mean = logscore / length(y_val),
    brier = brier
  )
  if (isTRUE(return_probs)) {
    out$p <- p
    out$y <- y_bin
  }
  out
}
