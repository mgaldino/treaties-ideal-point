#include <RcppArmadillo.h>
#include <Rmath.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

inline void truncnorm_moment_single(double mu, int y,
                                    double &y_star, double &y_star_var) {
  double log_phi = R::dnorm4(mu, 0.0, 1.0, 1);
  if (y == 1) {
    double log_Phi = R::pnorm5(mu, 0.0, 1.0, 1, 1);
    double lambda = std::exp(log_phi - log_Phi);
    y_star = mu + lambda;
    y_star_var = 1.0 - lambda * (lambda + mu);
  } else if (y == -1) {
    double log_1mPhi = R::pnorm5(mu, 0.0, 1.0, 0, 1);
    double lambda = std::exp(log_phi - log_1mPhi);
    y_star = mu - lambda;
    y_star_var = 1.0 - lambda * (lambda - mu);
  } else {
    y_star = NA_REAL;
    y_star_var = NA_REAL;
    return;
  }

  if (!R_finite(y_star_var)) {
    y_star_var = 1e-12;
  }
  if (y_star_var < 1e-12) y_star_var = 1e-12;
  if (y_star_var > 1.0) y_star_var = 1.0;
}

// [[Rcpp::export]]
List truncnorm_moments_rcpp(NumericVector mu, IntegerVector y) {
  R_xlen_t n = mu.size();
  if (y.size() != n) stop("mu and y must have same length");

  NumericVector y_star(n);
  NumericVector y_star_var(n);

  for (R_xlen_t i = 0; i < n; ++i) {
    double ys, yv;
    truncnorm_moment_single(mu[i], y[i], ys, yv);
    y_star[i] = ys;
    y_star_var[i] = yv;
  }

  return List::create(
    _["y_star"] = y_star,
    _["y_star_var"] = y_star_var
  );
}

// [[Rcpp::export]]
List da_step_rcpp(IntegerMatrix rc,
                  NumericVector alpha,
                  NumericMatrix beta,
                  NumericVector x_smooth,
                  IntegerVector bill_session) {
  int N = rc.nrow();
  int J = rc.ncol();

  IntegerVector dims = x_smooth.attr("dim");
  if (dims.size() != 3) stop("x_smooth must be a 3D array");
  if (dims[0] != N) stop("x_smooth first dimension must match N");
  int K = dims[1];
  int T = dims[2];

  if (beta.nrow() != J || beta.ncol() != K) {
    stop("beta must be J x K");
  }
  if (alpha.size() != J) {
    stop("alpha must have length J");
  }
  if (bill_session.size() != J) {
    stop("bill.session must have length J");
  }

  // Map data to Armadillo views (no copy)
  arma::cube X(x_smooth.begin(), N, K, T, false);
  arma::mat Beta(beta.begin(), J, K, false);
  arma::vec Alpha(alpha.begin(), J, false);

  NumericMatrix y_star(N, J);
  NumericMatrix y_star_var(N, J);
  std::fill(y_star.begin(), y_star.end(), NA_REAL);
  std::fill(y_star_var.begin(), y_star_var.end(), NA_REAL);

  std::vector< std::vector<int> > items_by_period(T);
  for (int j = 0; j < J; ++j) {
    int t = bill_session[j];
    if (t >= 0 && t < T) items_by_period[t].push_back(j);
  }

  for (int t = 0; t < T; ++t) {
    const std::vector<int> &j_idx = items_by_period[t];
    if (j_idx.empty()) continue;

    arma::uvec j_uvec(j_idx.size());
    for (size_t idx = 0; idx < j_idx.size(); ++idx) {
      j_uvec[idx] = static_cast<arma::uword>(j_idx[idx]);
    }

    arma::mat X_t = X.slice(t);
    arma::mat Beta_sub = Beta.rows(j_uvec);
    arma::vec Alpha_sub = Alpha.elem(j_uvec);

    arma::mat mu = X_t * Beta_sub.t();
    mu.each_row() += Alpha_sub.t();

    for (size_t col = 0; col < j_idx.size(); ++col) {
      int j = j_idx[col];
      for (int i = 0; i < N; ++i) {
        int y = rc(i, j);
        if (y == 0) continue;
        double ys, yv;
        truncnorm_moment_single(mu(i, col), y, ys, yv);
        y_star(i, j) = ys;
        y_star_var(i, j) = yv;
      }
    }
  }

  return List::create(
    _["y_star"] = y_star,
    _["y_star_var"] = y_star_var
  );
}
