# Inspect US Congress data for V3
data <- readRDS("data/processed/us_congress_v3.rds")
cat("Names in RDS:
")
print(names(data))

if ("rc" %in% names(data)) {
  cat("
RC dimensions:", dim(data$rc), "
")
}

if ("nominate" %in% names(data)) {
  cat("
Nominate data preview:
")
  print(head(data$nominate))
}
