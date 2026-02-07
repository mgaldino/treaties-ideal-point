#' Data Augmentation Step (Albert-Chib) for K-Dimensional Dynamic Probit IRT
#'
#' Computes E[y* | y, x, alpha, beta] for each non-missing vote using
#' truncated normal moments. This is the first sub-step of the E-step.
#'
#' Reference: Section 5 of docs/estimation_plan_2d.md
#'            Albert & Chib (1993), JASA 88(422), 669-679.

#' Compute truncated normal moments with numerical stability
#'
#' For y = +1: y* ~ TN(mu, 1, 0, +Inf)
#' For y = -1: y* ~ TN(mu, 1, -Inf, 0)
#'
#' @param mu Numeric vector of linear predictors (alpha_j + beta_j' x_i,s(j))
#' @param y  Integer vector of observed votes (+1 or -1)
#' @return List with components:
#'   \item{y_star}{Numeric vector E[y* | y]}
#'   \item{y_star_var}{Numeric vector Var[y* | y]}
truncnorm_moments <- function(mu, y) {
  n <- length(mu)
  y_star     <- numeric(n)
  y_star_var <- numeric(n)

  # Log-space computation for numerical stability (Section 5.1)
  log_phi <- dnorm(mu, log = TRUE)

  # Positive votes: y* ~ TN(mu, 1, 0, +Inf)
  pos <- which(y == 1L)
  if (length(pos) > 0) {
    mu_pos       <- mu[pos]
    log_phi_pos  <- log_phi[pos]
    log_Phi_pos  <- pnorm(mu_pos, log.p = TRUE)
    # lambda = phi(mu) / Phi(mu)  (inverse Mills ratio)
    lambda_pos   <- exp(log_phi_pos - log_Phi_pos)
    y_star[pos]     <- mu_pos + lambda_pos
    y_star_var[pos] <- 1 - lambda_pos * (lambda_pos + mu_pos)
  }

  # Negative votes: y* ~ TN(mu, 1, -Inf, 0)
  neg <- which(y == -1L)
  if (length(neg) > 0) {
    mu_neg       <- mu[neg]
    log_phi_neg  <- log_phi[neg]
    # 1 - Phi(mu) = Phi(-mu); use lower.tail=FALSE for stability
    log_1mPhi_neg <- pnorm(mu_neg, lower.tail = FALSE, log.p = TRUE)
    # lambda = phi(mu) / (1 - Phi(mu))
    lambda_neg   <- exp(log_phi_neg - log_1mPhi_neg)
    y_star[neg]     <- mu_neg - lambda_neg
    y_star_var[neg] <- 1 - lambda_neg * (lambda_neg - mu_neg)
  }

  # Clamp variance to (0, 1] to guard against numerical issues
  y_star_var <- pmax(y_star_var, 1e-12)
  y_star_var <- pmin(y_star_var, 1.0)

  list(y_star = y_star, y_star_var = y_star_var)
}


#' Data Augmentation Step
#'
#' For each non-missing vote (rc[i,j] != 0), compute the expected augmented
#' latent utility E[y* | y, x, alpha, beta].
#'
#' @param rc           N x J integer matrix. Votes: +1, -1, 0 (missing).
#' @param alpha        Numeric vector length J. Item intercepts.
#' @param beta         J x K numeric matrix. Item discrimination vectors.
#' @param x_smooth     N x K x T numeric array. Smoothed ideal points.
#' @param bill.session Integer vector length J. 0-indexed period for each item.
#'
#' @return List with components:
#'   \item{y_star}{N x J numeric matrix. E[y* | y, x, alpha, beta].
#'         Entries where rc == 0 are set to NA.}
#'   \item{y_star_var}{N x J numeric matrix. Var[y* | y, x, alpha, beta].
#'         Entries where rc == 0 are set to NA.}
da_step <- function(rc, alpha, beta, x_smooth, bill.session) {
  N <- nrow(rc)
  J <- ncol(rc)
  K <- ncol(beta)

  y_star     <- matrix(NA_real_, nrow = N, ncol = J)
  y_star_var <- matrix(NA_real_, nrow = N, ncol = J)

  # Process by period for efficiency (all items in same period use same x slice)
  periods <- sort(unique(bill.session))

  for (tt in periods) {
    # Items in this period (1-indexed column indices)
    j_idx <- which(bill.session == tt)
    if (length(j_idx) == 0) next

    # Ideal points for this period: N x K matrix
    # bill.session is 0-indexed, array slice is 1-indexed
    x_t <- x_smooth[, , tt + 1L, drop = FALSE]
    dim(x_t) <- c(N, K)  # ensure N x K matrix

    # Linear predictor: N x |j_idx| matrix
    # mu[i, j] = alpha[j] + beta[j, ]' %*% x_t[i, ]
    # = x_t %*% t(beta[j_idx, ]) + outer(1, alpha[j_idx])
    mu_mat <- x_t %*% t(beta[j_idx, , drop = FALSE])
    mu_mat <- sweep(mu_mat, 2, alpha[j_idx], "+")

    # Extract votes for these items
    rc_sub <- rc[, j_idx, drop = FALSE]

    # Vectorized processing for all items in this period
    nonmiss_mask <- rc_sub != 0L
    if (any(nonmiss_mask)) {
      moments <- truncnorm_moments(
        mu = mu_mat[nonmiss_mask],
        y  = rc_sub[nonmiss_mask]
      )

      tmp_ystar <- matrix(NA_real_, nrow = N, ncol = length(j_idx))
      tmp_ystar_var <- matrix(NA_real_, nrow = N, ncol = length(j_idx))

      tmp_ystar[nonmiss_mask]     <- moments$y_star
      tmp_ystar_var[nonmiss_mask] <- moments$y_star_var

      y_star[, j_idx]     <- tmp_ystar
      y_star_var[, j_idx] <- tmp_ystar_var
    }
  }

  list(y_star = y_star, y_star_var = y_star_var)
}
