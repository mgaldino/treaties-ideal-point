#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' M-Step Items (Rcpp)
//'
//' @param y_star NumericMatrix (N x J)
//' @param rc IntegerMatrix (N x J)
//' @param x_smooth NumericVector (N * K * T)
//' @param P_smooth List of N Lists of T matrices
//' @param bill_session IntegerVector (J)
//' @param beta_mu NumericVector (K+1)
//' @param beta_sigma NumericMatrix (K+1 x K+1)
//' 
//' @return List(alpha, beta)
// [[Rcpp::export]]
List m_step_items_rcpp(NumericMatrix y_star, IntegerMatrix rc, 
                       NumericVector x_smooth, List P_smooth,
                       IntegerVector bill_session, 
                       NumericVector beta_mu, NumericMatrix beta_sigma) {
    
    int N = rc.nrow();
    int J = rc.ncol();
    int K = beta_sigma.nrow() - 1;
    
    // Check P_smooth structure
    List p0 = P_smooth[0];
    int T = p0.size();
    
    if (x_smooth.size() != N * K * T) {
        stop("x_smooth dimension mismatch");
    }
    
    // Flatten P_smooth to t-major layout (t * N + i)
    // This ensures that for a fixed t, iterating i is contiguous in the vector
    std::vector<arma::mat> P_flat(T * N);
    for (int i = 0; i < N; ++i) {
        List Pi = P_smooth[i];
        if (Pi.size() != T) stop("Inconsistent T in P_smooth");
        for (int t = 0; t < T; ++t) {
            P_flat[t * N + i] = as<arma::mat>(Pi[t]);
        }
    }
    
    arma::cube X(x_smooth.begin(), N, K, T, false);
    
    NumericVector alpha_new(J);
    NumericMatrix beta_new(J, K);
    
    arma::mat Sigma_beta_inv = inv(as<arma::mat>(beta_sigma));
    arma::vec Sigma_beta_inv_mu = Sigma_beta_inv * as<arma::vec>(beta_mu);
    
    // Group items by period
    std::vector<std::vector<int>> items_by_period(T);
    for (int j = 0; j < J; ++j) {
        int t = bill_session[j];
        if (t >= 0 && t < T) {
            items_by_period[t].push_back(j);
        }
    }
    
    int Kp1 = K + 1;
    
    // Iterate by period
    for (int t = 0; t < T; ++t) {
        if (items_by_period[t].empty()) continue;
        
        // Pre-fetch X slice for this period
        arma::mat Xt = X.slice(t); // N x K
        
        // Iterate items in this period
        for (int j : items_by_period[t]) {
            
            double count = 0.0;
            arma::vec sum_x(K, arma::fill::zeros);
            double sum_y = 0.0;
            arma::vec sum_yx(K, arma::fill::zeros);
            arma::mat sum_xx(K, K, arma::fill::zeros);
            arma::mat P_sum(K, K, arma::fill::zeros);
            
            // Iterate voters
            for (int i = 0; i < N; ++i) {
                // Check if vote exists
                if (rc(i, j) == 0) continue;
                
                // Voter i, period t
                // Xt is column-major. Row i is strided.
                // arma::rowvec is inefficient if used repeatedly?
                // Let's access elements directly or copy to vec
                arma::rowvec xi_row = Xt.row(i);
                arma::vec xi = xi_row.t();
                
                double yi = y_star(i, j);
                
                count += 1.0;
                sum_x += xi;
                sum_y += yi;
                sum_yx += yi * xi;
                sum_xx += xi * xi_row; // outer product
                
                // Access P_flat[t * N + i]
                P_sum += P_flat[t * N + i];
            }
            
            if (count == 0.0) continue;
            
            // Build Sigma_zz
            arma::mat Sigma_zz(Kp1, Kp1, arma::fill::zeros);
            
            Sigma_zz(0, 0) = count;
            Sigma_zz(0, arma::span(1, K)) = sum_x.t();
            Sigma_zz(arma::span(1, K), 0) = sum_x;
            Sigma_zz(arma::span(1, K), arma::span(1, K)) = P_sum + sum_xx;
            
            // Build Sigma_zy
            arma::vec Sigma_zy(Kp1);
            Sigma_zy(0) = sum_y;
            Sigma_zy.rows(1, K) = sum_yx;
            
            // Solve
            arma::mat A = Sigma_beta_inv + Sigma_zz;
            arma::vec b = Sigma_beta_inv_mu + Sigma_zy;
            
            arma::vec gamma_hat;
            bool success = solve(gamma_hat, A, b); 
            if (!success) {
                // Fallback for singular matrix (rare with prior)
                gamma_hat.zeros(Kp1);
            }
            
            alpha_new(j) = gamma_hat(0);
            for (int k = 0; k < K; ++k) {
                beta_new(j, k) = gamma_hat(k + 1);
            }
        }
    }
    
    return List::create(Named("alpha") = alpha_new,
                        Named("beta") = beta_new);
}
