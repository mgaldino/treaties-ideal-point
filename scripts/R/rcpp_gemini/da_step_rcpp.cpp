#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Data Augmentation Step (Rcpp)
//'
//' @param rc IntegerMatrix (N x J)
//' @param alpha NumericVector (J)
//' @param beta NumericMatrix (J x K)
//' @param x_smooth NumericVector (N * K * T) flattened array
//' @param bill_session IntegerVector (J) 0-indexed period
//' @param N Integer
//' @param J Integer
//' @param K Integer
//' @param T Integer
//' 
//' @return List(y_star, y_star_var)
// [[Rcpp::export]]
List da_step_rcpp(IntegerMatrix rc, NumericVector alpha, NumericMatrix beta, 
                  NumericVector x_smooth, IntegerVector bill_session, 
                  int N, int J, int K, int T) {
    
    // Output matrices (N x J)
    // Initialize with NA
    NumericMatrix y_star(N, J);
    std::fill(y_star.begin(), y_star.end(), NA_REAL);
    
    NumericMatrix y_star_var(N, J);
    std::fill(y_star_var.begin(), y_star_var.end(), NA_REAL);
    
    // Map x_smooth to Armadillo cube (N x K x T)
    // Note: R stores arrays as column-major: 
    // dim1 (N) varies fastest, then dim2 (K), then dim3 (T).
    // Armadillo cube constructor takes (ptr, n_rows, n_cols, n_slices).
    // This matches the R memory layout.
    arma::cube X(x_smooth.begin(), N, K, T, false);
    
    // Map alpha and beta
    arma::vec Alpha(alpha.begin(), J, false);
    arma::mat Beta(beta.begin(), J, K, false);
    
    // Identify unique periods to iterate over
    // We can use an std::set or similar, or just vector + sort + unique
    std::vector<int> periods = as<std::vector<int>>(unique(bill_session));
    std::sort(periods.begin(), periods.end());
    
    for (int t : periods) {
        // Identify items belonging to period t
        std::vector<int> j_idx;
        j_idx.reserve(J / periods.size()); // heuristic reserve
        
        for (int j = 0; j < J; ++j) {
            if (bill_session[j] == t) {
                j_idx.push_back(j);
            }
        }
        
        if (j_idx.empty()) continue;
        
        // Extract X for this period (slice t)
        // t is 0-indexed from bill_session, matches slice index
        arma::mat Xt = X.slice(t); // N x K
        
        // Extract Beta subset for these items
        arma::uvec j_uvec = arma::conv_to<arma::uvec>::from(j_idx);
        arma::mat Beta_sub = Beta.rows(j_uvec); // |j_idx| x K
        arma::vec Alpha_sub = Alpha.elem(j_uvec); // |j_idx|
        
        // Calculate mu = X * Beta' + alpha (broadcast)
        // (N x K) * (K x |j_idx|) -> N x |j_idx|
        arma::mat Mu = Xt * Beta_sub.t();
        Mu.each_row() += Alpha_sub.t();
        
        // Iterate over the items in this period
        for (size_t k = 0; k < j_idx.size(); ++k) {
            int j = j_idx[k]; // global item index
            
            for (int i = 0; i < N; ++i) {
                int vote = rc(i, j);
                
                // Skip missing values (0)
                if (vote == 0) continue;
                
                double mu = Mu(i, k);
                double ys = 0.0;
                double ysv = 0.0;
                
                // Truncated normal moments logic
                // log_phi = dnorm(mu)
                double log_phi = R::dnorm(mu, 0.0, 1.0, 1);
                
                if (vote == 1) {
                    // TN(mu, 1, 0, Inf)
                    // log_Phi = pnorm(mu)
                    double log_Phi = R::pnorm(mu, 0.0, 1.0, 1, 1);
                    // lambda = exp(log_phi - log_Phi)
                    double lambda = std::exp(log_phi - log_Phi);
                    ys = mu + lambda;
                    ysv = 1.0 - lambda * (lambda + mu);
                } else {
                    // TN(mu, 1, -Inf, 0)
                    // log_1mPhi = pnorm(mu, lower.tail=false)
                    // Note: R::pnorm(x, m, s, lower, log)
                    double log_1mPhi = R::pnorm(mu, 0.0, 1.0, 0, 1);
                    double lambda = std::exp(log_phi - log_1mPhi);
                    ys = mu - lambda;
                    ysv = 1.0 - lambda * (lambda - mu);
                }
                
                // Clamp variance to (0, 1]
                if (ysv < 1e-12) ysv = 1e-12;
                if (ysv > 1.0) ysv = 1.0;
                
                y_star(i, j) = ys;
                y_star_var(i, j) = ysv;
            }
        }
    }
    
    return List::create(Named("y_star") = y_star,
                        Named("y_star_var") = y_star_var);
}
