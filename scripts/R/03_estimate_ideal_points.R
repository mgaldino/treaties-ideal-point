suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("emIRT", quietly = TRUE)) {
    stop("Package 'emIRT' is required. Please install it with install.packages('emIRT').")
  }
  library(dplyr)
  library(readr)
  library(emIRT)
})

set.seed(123)

output_dir <- "outputs/estimates"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

areas <- c("investment", "security", "environment",
           "human_rights", "arms_control", "intellectual_property")

anchor_map <- list(
  investment            = list(pos = "DNK", neg = "IRN"),
  security              = list(pos = "DNK", neg = "IRN"),
  environment           = list(pos = "DNK", neg = "SAU"),
  human_rights          = list(pos = "DNK", neg = "PRK"),
  arms_control          = list(pos = "NZL", neg = "ISR"),
  intellectual_property = list(pos = "DNK", neg = "AGO")
)

build_pca_starts <- function(rc, T_periods) {
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

  pca <- prcomp(rc_pca, center = TRUE, scale. = TRUE)
  pc1 <- as.numeric(scale(pca$x[, 1]))

  x_start <- matrix(rep(pc1, times = T_periods), nrow = length(pc1), ncol = T_periods)
  alpha_start <- matrix(rnorm(ncol(rc), 0, 0.1), ncol = 1)
  beta_start <- matrix(rnorm(ncol(rc), 0, 0.5), ncol = 1)

  list(x = x_start, alpha = alpha_start, beta = beta_start)
}

estimate_area <- function(area) {
  flow_path <- file.path("data/processed", paste0(area, "_flow_matrix.rds"))
  if (!file.exists(flow_path)) {
    stop("Missing flow matrix: ", flow_path)
  }
  flow <- readRDS(flow_path)

  data_list <- list(
    rc = flow$rc,
    startlegis = flow$startlegis,
    endlegis = flow$endlegis,
    bill.session = flow$bill.session,
    T = flow$T
  )

  starts <- build_pca_starts(flow$rc, flow$T)

  # Priors with anchors
  n_countries <- nrow(flow$rc)
  x_mu0 <- matrix(0, nrow = n_countries, ncol = 1)
  x_sigma0 <- matrix(1, nrow = n_countries, ncol = 1)

  pos_anchor <- anchor_map[[area]]$pos
  neg_anchor <- anchor_map[[area]]$neg

  pos_idx <- which(flow$country_codes == pos_anchor)
  neg_idx <- which(flow$country_codes == neg_anchor)

  if (length(pos_idx) == 0 || length(neg_idx) == 0) {
    stop("Anchor country not found in area: ", area)
  }

  x_mu0[pos_idx, 1] <- 2.0
  x_sigma0[pos_idx, 1] <- 0.01
  x_mu0[neg_idx, 1] <- -2.0
  x_sigma0[neg_idx, 1] <- 0.01

  priors <- list(
    x.mu0 = x_mu0,
    x.sigma0 = x_sigma0,
    beta.mu = matrix(c(0, 0), nrow = 2, ncol = 1),
    beta.sigma = matrix(c(25, 0, 0, 25), nrow = 2, ncol = 2),
    omega2 = matrix(0.1, nrow = n_countries, ncol = 1)
  )

  control <- list(
    threads = 1L,
    verbose = TRUE,
    thresh = 1e-6,
    maxit = 500L,
    checkfreq = 50L
  )

  result <- emIRT::dynIRT(
    .data = data_list,
    .starts = starts,
    .priors = priors,
    .control = control
  )

  if (is.null(result$runtime$conv) || result$runtime$conv != 1) {
    stop("dynIRT did not converge for area: ", area)
  }

  ideal_points <- result$means$x
  rownames(ideal_points) <- flow$country_codes
  colnames(ideal_points) <- flow$period_labels

  out_rds <- list(
    ideal_points = ideal_points,
    alpha = result$means$alpha,
    beta = result$means$beta,
    country_codes = flow$country_codes,
    period_labels = flow$period_labels,
    item_labels = flow$item_labels,
    convergence = result$runtime,
    omega2_used = 0.1,
    anchors = list(pos = pos_anchor, neg = neg_anchor)
  )

  saveRDS(out_rds, file.path(output_dir, paste0(area, "_ideal_points.rds")))

  # Export long format CSV
  long_df <- expand.grid(
    country_iso3 = flow$country_codes,
    period = flow$period_labels,
    stringsAsFactors = FALSE
  )
  long_df$ideal_point <- as.vector(ideal_points)
  readr::write_csv(long_df, file.path(output_dir, paste0(area, "_ideal_points.csv")))

  # Export item parameters
  item_params <- tibble::tibble(
    item_id = flow$item_labels,
    alpha = as.vector(result$means$alpha),
    beta = as.vector(result$means$beta)
  )
  readr::write_csv(item_params, file.path(output_dir, paste0(area, "_item_params.csv")))

  invisible(TRUE)
}

for (area in areas) {
  message("Estimating area: ", area)
  estimate_area(area)
}

message("Phase 3 estimation completed for all areas.")
