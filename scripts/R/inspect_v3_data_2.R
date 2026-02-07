# Inspect benchmarks and anchors
data <- readRDS("data/processed/us_congress_v3.rds")

cat("
--- Anchors ---
")
print(data$anchor_names)
print(data$anchor_positions)
print(data$anchor_leg_idx)

cat("
--- DW Benchmark ---
")
print(str(data$dw_benchmark))
