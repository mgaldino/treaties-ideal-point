#!/usr/bin/env Rscript
invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE))

domains <- c("investment", "environment", "human_rights", "arms_control", "intellectual_property")

trend_slope <- function(y) {
  x <- seq_along(y)
  stats::coef(stats::lm(y ~ x))[["x"]]
}

trend_label <- function(v7_slope, v8_slope) {
  if (is.na(v7_slope) || is.na(v8_slope)) return(NA_character_)
  if (sign(v7_slope) != 0 && sign(v8_slope) != 0 && sign(v7_slope) != sign(v8_slope)) {
    return("reverses")
  }
  if (abs(v8_slope) > (abs(v7_slope) * 1.1 + 1e-9)) {
    return("accelerates")
  }
  "continues"
}

rows <- vector("list", length(domains))

for (i in seq_along(domains)) {
  domain <- domains[i]

  v7_path <- file.path("outputs/v7_country_anchors", paste0(domain, "_results.rds"))
  v8_path <- file.path("outputs/v8_extended", paste0(domain, "_results.rds"))

  v7 <- readRDS(v7_path)
  v8 <- readRDS(v8_path)

  common <- intersect(v7$country_codes, v8$country_codes)
  idx7 <- match(common, v7$country_codes)
  idx8 <- match(common, v8$country_codes)

  x7 <- as.vector(v7$ideal_points[idx7, 1, 1:6])
  x8 <- as.vector(v8$ideal_points[idx8, 1, 1:6])
  cor_overlap <- suppressWarnings(stats::cor(x7, x8, use = "pairwise.complete.obs"))

  v7_means <- v7$aggregate$mean_dim1[1:6]
  v8_means <- v8$aggregate$mean_dim1[1:7]
  v7_slope <- as.numeric(trend_slope(v7_means))
  v8_slope <- as.numeric(trend_slope(v8_means))
  v8_p7 <- as.numeric(v8$aggregate$mean_dim1[7])

  rows[[i]] <- data.frame(
    domain = domain,
    v7_trend_slope = v7_slope,
    v8_trend_slope = v8_slope,
    corr_dim1_overlap_p1_p6 = cor_overlap,
    v8_period7_mean_dim1 = v8_p7,
    trend = trend_label(v7_slope, v8_slope),
    stringsAsFactors = FALSE
  )
}

cmp <- do.call(rbind, rows)
print(cmp)

dir.create("outputs/v8_extended", recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path("outputs/v8_extended", "v7_vs_v8_comparison.csv")
readr::write_csv(cmp, out_csv)
cat(sprintf("\nSaved: %s\n", out_csv))

