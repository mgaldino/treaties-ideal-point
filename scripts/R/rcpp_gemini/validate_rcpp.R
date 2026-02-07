#!/usr/bin/env Rscript
# Validation of Rcpp implementations against R originals

library(testthat)
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
    cat(sprintf("Found libraries in: %s\n", paste(lib_dirs, collapse = ", ")))
    
    # Construct -L flags
    l_flags <- paste(sprintf("-L%s", lib_dirs), collapse = " ")
    
    # Override FLIBS
    new_flibs <- sprintf("%s -lgfortran -lquadmath -lm", l_flags)
    Sys.setenv(FLIBS = new_flibs)
    
    # Set LIBRARY_PATH
    current_lp <- Sys.getenv("LIBRARY_PATH")
    new_lp <- paste(c(lib_dirs, current_lp), collapse = ":")
    Sys.setenv(LIBRARY_PATH = new_lp)
    
    cat(sprintf("Set FLIBS=%s\nSet LIBRARY_PATH=%s\n", new_flibs, new_lp))
  }
}

cat("Validating Rcpp implementations...\n")

# Load original functions (renaming them so we can keep them)
env_orig <- new.env()
sys.source("scripts/R/da_step.R", envir = env_orig)
sys.source("scripts/R/m_step.R", envir = env_orig)

da_step_orig <- env_orig$da_step
m_step_items_orig <- env_orig$m_step_items

# Load Rcpp wrappers (which overwrite global da_step/m_step_items)
source("scripts/R/rcpp_gemini/dynIRT_KD_rcpp.R")

# Note: sourcing dynIRT_KD_rcpp.R overwrites da_step/m_step_items in global env
da_step_new <- da_step
m_step_items_new <- m_step_items

# --- Generate Synthetic Data ---
set.seed(42)
N <- 20
J <- 50
K <- 2
T_periods <- 3

rc <- matrix(sample(c(-1, 0, 1), N * J, replace = TRUE, prob = c(0.4, 0.2, 0.4)), nrow = N, ncol = J)
alpha <- rnorm(J)
beta <- matrix(rnorm(J * K), nrow = J, ncol = K)
x_smooth <- array(rnorm(N * K * T_periods), dim = c(N, K, T_periods))
bill_session <- sample(0:(T_periods - 1), J, replace = TRUE)

# --- Test da_step ---
cat("Testing da_step... ")
res_orig <- da_step_orig(rc, alpha, beta, x_smooth, bill_session)
res_new  <- da_step_new(rc, alpha, beta, x_smooth, bill_session)

# Check structure
expect_type(res_new, "list")
expect_named(res_new, c("y_star", "y_star_var"))

# Check values
# Treat NA as equal
diff_ystar <- max(abs(res_orig$y_star - res_new$y_star), na.rm = TRUE)
diff_ystar_var <- max(abs(res_orig$y_star_var - res_new$y_star_var), na.rm = TRUE)

if (diff_ystar < 1e-10 && diff_ystar_var < 1e-10) {
  cat("PASS (diff < 1e-10)\n")
} else {
  cat(sprintf("FAIL (ystar diff: %e, var diff: %e)\n", diff_ystar, diff_ystar_var))
  stop("da_step mismatch")
}

# --- Test m_step_items ---
# We need y_star for input
y_star <- res_orig$y_star

# P_smooth: List of N lists of T matrices (KxK)
P_smooth <- vector("list", N)
for (i in 1:N) {
  P_smooth[[i]] <- vector("list", T_periods)
  for (t in 1:T_periods) {
    P_smooth[[i]][[t]] <- crossprod(matrix(rnorm(K*K), K, K)) # positive definite
  }
}

beta_mu <- rep(0, K + 1)
beta_sigma <- diag(K + 1)

cat("Testing m_step_items... ")
res_m_orig <- m_step_items_orig(y_star, rc, x_smooth, P_smooth, bill_session, beta_mu, beta_sigma)
res_m_new  <- m_step_items_new(y_star, rc, x_smooth, P_smooth, bill_session, beta_mu, beta_sigma)

diff_alpha <- max(abs(res_m_orig$alpha - res_m_new$alpha))
diff_beta  <- max(abs(res_m_orig$beta - res_m_new$beta))

if (diff_alpha < 1e-10 && diff_beta < 1e-10) {
  cat("PASS (diff < 1e-10)\n")
} else {
  cat(sprintf("FAIL (alpha diff: %e, beta diff: %e)\n", diff_alpha, diff_beta))
  stop("m_step_items mismatch")
}