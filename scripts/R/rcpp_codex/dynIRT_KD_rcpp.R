# Wrapper to run dynIRT_KD with Rcpp-optimized steps

.rcpp_codex_dir <- tryCatch(
  dirname(normalizePath(sys.frame(sys.nframe())$ofile)),
  error = function(e) "scripts/R/rcpp_codex"
)

# Ensure Fortran libs are discoverable (Homebrew gcc vs R FLIBS mismatch)
.flibs <- Sys.getenv("FLIBS", unset = "")
if (!nzchar(.flibs) || grepl("/opt/gfortran", .flibs)) {
  .gcc_lib <- "/opt/homebrew/opt/gcc/lib/gcc/current"
  if (dir.exists(.gcc_lib)) {
    Sys.setenv(FLIBS = paste("-L", .gcc_lib, "-lgfortran -lquadmath"))
  }
}

.makevars_path <- file.path(.rcpp_codex_dir, "Makevars")
if (file.exists(.makevars_path)) {
  Sys.setenv(R_MAKEVARS_USER = .makevars_path)
}

if (exists(".gcc_lib") && dir.exists(.gcc_lib)) {
  .pkg_libs <- Sys.getenv("PKG_LIBS", unset = "")
  if (!grepl(.gcc_lib, .pkg_libs, fixed = TRUE)) {
    Sys.setenv(PKG_LIBS = trimws(paste(.pkg_libs, "-L", .gcc_lib)))
  }
  .gcc_emutls <- file.path(.gcc_lib, "gcc/aarch64-apple-darwin23/15")
  if (dir.exists(.gcc_emutls) && !grepl(.gcc_emutls, Sys.getenv("PKG_LIBS"), fixed = TRUE)) {
    Sys.setenv(PKG_LIBS = trimws(paste(Sys.getenv("PKG_LIBS"), "-L", .gcc_emutls)))
  }
}

Rcpp::sourceCpp(file.path(.rcpp_codex_dir, "da_step_rcpp.cpp"))
Rcpp::sourceCpp(file.path(.rcpp_codex_dir, "m_step_rcpp.cpp"))

# Load the original R implementation (defines dynIRT_KD, kalman, etc.)
source(file.path(.rcpp_codex_dir, "..", "dynIRT_KD.R"))

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
