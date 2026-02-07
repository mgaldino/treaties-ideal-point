# Quick check for us_congress_v3.rds

dat <- readRDS("data/processed/us_congress_v3.rds")

rc <- dat$rc
N <- nrow(rc)
J <- ncol(rc)

summary_list <- list(
  chamber = dat$chamber,
  congress_min = min(dat$congresses),
  congress_max = max(dat$congresses),
  T_periods = dat$T,
  N = N,
  J = J,
  sparsity = mean(rc == 0),
  startlegis_min = min(dat$startlegis),
  startlegis_max = max(dat$startlegis),
  endlegis_min = min(dat$endlegis),
  endlegis_max = max(dat$endlegis),
  anchors = data.frame(
    anchor = paste0("a", 0:2),
    name = dat$anchor_names,
    icpsr = dat$anchor_icpsr,
    row_index = dat$anchor_leg_idx,
    dim1 = dat$anchor_positions[, 1],
    dim2 = dat$anchor_positions[, 2]
  ),
  triangle_area = 0.5 * abs(
    dat$anchor_positions[1,1] * (dat$anchor_positions[2,2] - dat$anchor_positions[3,2]) +
      dat$anchor_positions[2,1] * (dat$anchor_positions[3,2] - dat$anchor_positions[1,2]) +
      dat$anchor_positions[3,1] * (dat$anchor_positions[1,2] - dat$anchor_positions[2,2])
  ),
  dw_complete = sum(complete.cases(dat$dw_benchmark)),
  dw_total = nrow(dat$dw_benchmark)
)

# Print to console
cat("===== Quick Check: us_congress_v3.rds =====\n")
cat(sprintf("Chamber: %s\n", summary_list$chamber))
cat(sprintf("Congresses: %d-%d (T=%d)\n", summary_list$congress_min,
            summary_list$congress_max, summary_list$T_periods))
cat(sprintf("N=%d, J=%d\n", N, J))
cat(sprintf("Sparsity: %.1f%%\n", summary_list$sparsity * 100))
cat(sprintf("startlegis range: [%d, %d]\n", summary_list$startlegis_min,
            summary_list$startlegis_max))
cat(sprintf("endlegis range: [%d, %d]\n", summary_list$endlegis_min,
            summary_list$endlegis_max))
cat(sprintf("Triangle area: %.3f\n", summary_list$triangle_area))
cat(sprintf("DW-NOMINATE complete: %d / %d\n",
            summary_list$dw_complete, summary_list$dw_total))

# Save CSV summary
summary_dir <- "outputs/v3_congress"
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

summary_df <- data.frame(
  chamber = summary_list$chamber,
  congress_min = summary_list$congress_min,
  congress_max = summary_list$congress_max,
  T_periods = summary_list$T_periods,
  N = N,
  J = J,
  sparsity = summary_list$sparsity,
  startlegis_min = summary_list$startlegis_min,
  startlegis_max = summary_list$startlegis_max,
  endlegis_min = summary_list$endlegis_min,
  endlegis_max = summary_list$endlegis_max,
  triangle_area = summary_list$triangle_area,
  dw_complete = summary_list$dw_complete,
  dw_total = summary_list$dw_total
)

write.csv(summary_df, file.path(summary_dir, "us_congress_v3_summary.csv"),
          row.names = FALSE)
write.csv(summary_list$anchors, file.path(summary_dir, "us_congress_v3_anchors.csv"),
          row.names = FALSE)

cat("Saved: outputs/v3_congress/us_congress_v3_summary.csv\n")
cat("Saved: outputs/v3_congress/us_congress_v3_anchors.csv\n")
