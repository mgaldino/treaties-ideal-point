#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
List m_step_items_rcpp(NumericMatrix y_star,
                       IntegerMatrix rc,
                       NumericVector x_smooth,
                       List P_smooth,
                       IntegerVector bill_session,
                       NumericVector beta_mu,
                       NumericMatrix beta_sigma,
                       Nullable<List> voters_by_item = R_NilValue) {
  int N = rc.nrow();
  int J = rc.ncol();

  IntegerVector dims = x_smooth.attr("dim");
  if (dims.size() != 3) stop("x_smooth must be a 3D array");
  int K = dims[1];
  int T = dims[2];
  int Kp1 = K + 1;

  if (bill_session.size() != J) {
    stop("bill.session must have length J");
  }
  if (beta_mu.size() != Kp1) {
    stop("beta_mu must have length K+1");
  }
  if (beta_sigma.nrow() != Kp1 || beta_sigma.ncol() != Kp1) {
    stop("beta_sigma must be (K+1) x (K+1)");
  }

  arma::cube X(x_smooth.begin(), N, K, T, false);
  arma::mat Sigma_beta(beta_sigma.begin(), Kp1, Kp1, false);
  arma::vec beta_mu_vec(beta_mu.begin(), Kp1, false);

  arma::mat Sigma_beta_inv = arma::inv(Sigma_beta);
  arma::vec Sigma_beta_inv_mu = Sigma_beta_inv * beta_mu_vec;

  NumericVector alpha_new(J);
  NumericMatrix beta_new(J, K);

  std::vector< std::vector<int> > voters_list(J);
  if (voters_by_item.isNotNull()) {
    List vlist(voters_by_item);
    if (vlist.size() != J) stop("voters_by_item must have length J");
    for (int j = 0; j < J; ++j) {
      IntegerVector v = vlist[j];
      voters_list[j].reserve(v.size());
      for (int k = 0; k < v.size(); ++k) {
        int idx = v[k] - 1; // convert to 0-based
        if (idx >= 0 && idx < N) voters_list[j].push_back(idx);
      }
    }
  } else {
    for (int j = 0; j < J; ++j) {
      voters_list[j].reserve(N / 2);
      for (int i = 0; i < N; ++i) {
        if (rc(i, j) != 0) voters_list[j].push_back(i);
      }
    }
  }

  for (int j = 0; j < J; ++j) {
    const std::vector<int> &voters = voters_list[j];
    int count = static_cast<int>(voters.size());
    if (count == 0) continue;

    int t_index = bill_session[j];
    if (t_index < 0 || t_index >= T) continue;

    arma::uvec idx(count);
    for (int k = 0; k < count; ++k) {
      idx[k] = static_cast<arma::uword>(voters[k]);
    }

    arma::mat X_t = X.slice(t_index).rows(idx);
    arma::vec Y(count);
    for (int k = 0; k < count; ++k) {
      Y[k] = y_star(voters[k], j);
    }

    arma::mat P_sum(K, K, arma::fill::zeros);
    for (int k = 0; k < count; ++k) {
      int i = voters[k];
      List P_i = P_smooth[i];
      if (P_i.size() == 0) continue;
      NumericMatrix P_it = P_i[t_index];
      arma::mat P_mat(P_it.begin(), K, K, false);
      P_sum += P_mat;
    }

    double sum_y = arma::accu(Y);
    arma::vec sum_x = arma::sum(X_t, 0).t();
    arma::vec sum_yx = X_t.t() * Y;
    arma::mat sum_xx = X_t.t() * X_t;

    arma::mat Sigma_zz(Kp1, Kp1, arma::fill::zeros);
    Sigma_zz(0, 0) = static_cast<double>(count);
    Sigma_zz(0, arma::span(1, K)) = sum_x.t();
    Sigma_zz(arma::span(1, K), 0) = sum_x;
    Sigma_zz(arma::span(1, K), arma::span(1, K)) = P_sum + sum_xx;

    arma::vec Sigma_zy(Kp1);
    Sigma_zy(0) = sum_y;
    Sigma_zy(arma::span(1, K)) = sum_yx;

    arma::mat A = Sigma_beta_inv + Sigma_zz;
    arma::vec b = Sigma_beta_inv_mu + Sigma_zy;

    arma::vec gamma_hat = arma::solve(A, b);

    alpha_new[j] = gamma_hat[0];
    for (int k = 0; k < K; ++k) {
      beta_new(j, k) = gamma_hat[k + 1];
    }
  }

  return List::create(
    _["alpha"] = alpha_new,
    _["beta"] = beta_new
  );
}
