# Wrapper for Rcpp-optimized dynIRT_KD
# Usage: source("scripts/R/rcpp_gemini/dynIRT_KD_rcpp.R")

library(Rcpp)
library(RcppArmadillo)

# --- Fix for macOS gfortran linking issues ---
if (Sys.info()["sysname"] == "Darwin") {
  # Try to find libgfortran
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
    # Construct -L flags
    l_flags <- paste(sprintf("-L%s", lib_dirs), collapse = " ")
    
    # Override FLIBS
    new_flibs <- sprintf("%s -lgfortran -lquadmath -lm", l_flags)
    Sys.setenv(FLIBS = new_flibs)
    
    # Set LIBRARY_PATH
    current_lp <- Sys.getenv("LIBRARY_PATH")
    new_lp <- paste(c(lib_dirs, current_lp), collapse = ":")
    Sys.setenv(LIBRARY_PATH = new_lp)
  }
}

# Source original R implementation
# We assume we are in project root
if (!file.exists("scripts/R/dynIRT_KD.R")) {
  stop("Please run from project root")
}

source("scripts/R/da_step.R")
source("scripts/R/m_step.R")
source("scripts/R/kalman.R")
source("scripts/R/dynIRT_KD.R")

# Source Rcpp implementations
sourceCpp("scripts/R/rcpp_gemini/da_step_rcpp.cpp")
sourceCpp("scripts/R/rcpp_gemini/m_step_rcpp.cpp")

# Override da_step
da_step <- function(rc, alpha, beta, x_smooth, bill.session) {
  N <- nrow(rc)
  J <- ncol(rc)
  K <- ncol(beta)
  
  # Ensure x_smooth is numeric array
  if (!is.array(x_smooth)) stop("x_smooth must be an array")
  dims <- dim(x_smooth)
  T_periods <- dims[3]
  
  # Rcpp expects x_smooth as a flattened vector (which it is by default when passed as SEXP)
  # But we pass the array object directly, Rcpp handles it as NumericVector.
  
  da_step_rcpp(rc, alpha, beta, x_smooth, bill.session, N, J, K, T_periods)
}

# Override m_step_items
m_step_items <- function(y_star, rc, x_smooth, P_smooth,
                         bill.session, beta_mu, beta_sigma,
                         voters_by_item = NULL) {
  # voters_by_item is not used in Rcpp version
  
  m_step_items_rcpp(y_star, rc, x_smooth, P_smooth,
                    bill.session, beta_mu, beta_sigma)
}

cat("Loaded Rcpp-optimized functions: da_step, m_step_items
")
