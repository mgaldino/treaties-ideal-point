# Real-data validation for dynIRT_KD_fast (environment area, K=1)

suppressPackageStartupMessages({
  if (!requireNamespace("SQUAREM", quietly = TRUE)) {
    stop("Package 'SQUAREM' is required. Install it with install.packages('SQUAREM').")
  }
})

source("scripts/R/da_step.R")
source("scripts/R/kalman.R")
source("scripts/R/m_step.R")
source("scripts/R/dynIRT_KD_fast.R")

set.seed(123)

area <- "environment"
flow_path <- file.path("data/processed", paste0(area, "_flow_matrix.rds"))
if (!file.exists(flow_path)) {
  stop("Missing flow matrix: ", flow_path)
}
flow <- readRDS(flow_path)

K <- 1L

build_pca_starts <- function(rc, T_periods, K) {
  rc_pca <- rc
  rc_pca[rc_pca == 0] <- NA
  rc_pca[rc_pca == -1] <- 0
  for (j in seq_len(ncol(rc_pca))) {
    col_mean <- mean(rc_pca[, j], na.rm = TRUE)
    if (is.nan(col_mean)) {
      col_mean <- 0
    }
    rc_pca[is.na(rc_pca[, j]), j] <- col_mean
  }

  pca <- stats::prcomp(rc_pca, center = TRUE, scale. = TRUE)
  pc1 <- as.numeric(scale(pca$x[, 1]))

  x_start <- array(rep(pc1, times = T_periods), dim = c(length(pc1), K, T_periods))
  alpha_start <- rnorm(ncol(rc), 0, 0.1)
  beta_start  <- matrix(rnorm(ncol(rc) * K, 0, 0.5), nrow = ncol(rc), ncol = K)

  list(x = x_start, alpha = alpha_start, beta = beta_start)
}

.data <- list(
  rc = flow$rc,
  startlegis = flow$startlegis,
  endlegis = flow$endlegis,
  bill.session = flow$bill.session,
  T = flow$T
)

.starts <- build_pca_starts(flow$rc, flow$T, K = K)

n_countries <- nrow(flow$rc)

x_mu0 <- matrix(0, nrow = n_countries, ncol = K)
x_Sigma0 <- matrix(1, nrow = n_countries, ncol = K)

pos_anchor <- "DNK"
neg_anchor <- "SAU"

pos_idx <- which(flow$country_codes == pos_anchor)
neg_idx <- which(flow$country_codes == neg_anchor)

if (length(pos_idx) == 0 || length(neg_idx) == 0) {
  stop("Anchor country not found for area: ", area)
}

x_mu0[pos_idx, 1] <- 2.0
x_Sigma0[pos_idx, 1] <- 0.01
x_mu0[neg_idx, 1] <- -2.0
x_Sigma0[neg_idx, 1] <- 0.01

.priors <- list(
  x.mu0 = x_mu0,
  x.Sigma0 = x_Sigma0,
  beta.mu = rep(0, K + 1),
  beta.sigma = 25 * diag(K + 1),
  omega = 0.1 * diag(K)
)

.control <- list(
  threads = 1L,
  verbose = TRUE,
  thresh = 1e-6,
  maxit = 200L,
  checkfreq = 50L,
  estimate_omega = FALSE,
  use_squarem = TRUE,
  thresh_aitken = 1e-4,
  clamp_limit = 6,
  ll_drop_tol = 1e-6,
  squarem_control = list(step.max0 = 4, mstep = 4)
)

result <- dynIRT_KD_fast(.data, .starts, .priors, .control, K = K)

ideal_points <- result$means$x[, 1, ]
rownames(ideal_points) <- flow$country_codes
colnames(ideal_points) <- flow$period_labels

output_dir <- "outputs/dynIRT_KD_fast"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

out_rds <- list(
  ideal_points = ideal_points,
  alpha = result$means$alpha,
  beta = result$means$beta,
  country_codes = flow$country_codes,
  period_labels = flow$period_labels,
  item_labels = flow$item_labels,
  convergence = result$runtime,
  omega_used = .priors$omega,
  anchors = list(pos = pos_anchor, neg = neg_anchor),
  control = .control
)

saveRDS(out_rds, file.path(output_dir, paste0(area, "_ideal_points_fast.rds")))

long_df <- expand.grid(
  country_iso3 = flow$country_codes,
  period = flow$period_labels,
  stringsAsFactors = FALSE
)
long_df$ideal_point <- as.vector(ideal_points)
utils::write.csv(long_df, file.path(output_dir, paste0(area, "_ideal_points_fast.csv")), row.names = FALSE)

item_params <- data.frame(
  item_id = flow$item_labels,
  alpha = as.vector(result$means$alpha),
  beta = as.vector(result$means$beta),
  stringsAsFactors = FALSE
)
utils::write.csv(item_params, file.path(output_dir, paste0(area, "_item_params_fast.csv")), row.names = FALSE)

saveRDS(result$runtime, file.path(output_dir, paste0(area, "_runtime_fast.rds")))

message("Validation complete for area: ", area)
message("Output directory: ", output_dir)
