# Wrapper to run dynIRT_KD with merged Rcpp-optimized steps

.rcpp_merged_dir <- tryCatch(
  dirname(normalizePath(sys.frame(sys.nframe())$ofile)),
  error = function(e) "scripts/R/rcpp_merged"
)

# --- Fix for macOS gfortran linking issues (from Gemini version) ---
if (Sys.info()["sysname"] == "Darwin") {
  libgfortran <- system("find /opt/homebrew -name libgfortran.dylib 2>/dev/null | head -n 1", intern = TRUE)
  libemutls <- system("find /opt/homebrew -name libemutls_w.a 2>/dev/null | head -n 1", intern = TRUE)

  lib_dirs <- c()
  if (length(libgfortran) > 0 && nzchar(libgfortran)) {
    lib_dirs <- c(lib_dirs, dirname(libgfortran))
  }
  if (length(libemutls) > 0 && nzchar(libemutls)) {
    lib_dirs <- c(lib_dirs, dirname(libemutls))
  }

  lib_dirs <- unique(lib_dirs)

  if (length(lib_dirs) > 0) {
    l_flags <- paste(sprintf("-L%s", lib_dirs), collapse = " ")
    new_flibs <- sprintf("%s -lgfortran -lquadmath -lm", l_flags)
    Sys.setenv(FLIBS = new_flibs)

    current_lp <- Sys.getenv("LIBRARY_PATH")
    new_lp <- paste(c(lib_dirs, current_lp), collapse = ":")
    Sys.setenv(LIBRARY_PATH = new_lp)
  }
}

Rcpp::sourceCpp(file.path(.rcpp_merged_dir, "da_step_rcpp.cpp"))
Rcpp::sourceCpp(file.path(.rcpp_merged_dir, "m_step_rcpp.cpp"))

# Load the original R implementation (defines dynIRT_KD, kalman, etc.)
source(file.path(.rcpp_merged_dir, "..", "dynIRT_KD.R"))

# Override the bottleneck functions with Rcpp versions
truncnorm_moments <- truncnorm_moments_rcpp

da_step <- function(rc, alpha, beta, x_smooth, bill.session) {
  da_step_rcpp(rc, alpha, beta, x_smooth, bill_session = bill.session)
}

m_step_items <- function(y_star, rc, x_smooth, P_smooth, bill.session,
                         beta_mu, beta_sigma, voters_by_item = NULL) {
  m_step_items_rcpp(y_star, rc, x_smooth, P_smooth,
                    bill_session = bill.session,
                    beta_mu = beta_mu, beta_sigma = beta_sigma,
                    voters_by_item = voters_by_item)
}

# Convenience alias

dynIRT_KD_rcpp <- dynIRT_KD
