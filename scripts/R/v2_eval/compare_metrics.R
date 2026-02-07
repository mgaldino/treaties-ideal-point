# Compare emIRT vs dynIRT_KD metrics and write summary outputs

IN_PATH <- "outputs/v2_eval/summary.csv"
OUT_DELTAS <- "outputs/v2_eval/summary_with_deltas.csv"
OUT_SUMMARY <- "outputs/v2_eval/compare_summary.csv"
OUT_MD <- "outputs/v2_eval/compare_summary.md"

if (!file.exists(IN_PATH)) {
  stop("Input summary not found: ", IN_PATH)
}

df <- read.csv(IN_PATH, stringsAsFactors = FALSE)

# Deltas: positive delta_logscore_mean favors dynIRT_KD (higher is better)
# Negative delta_brier favors dynIRT_KD (lower is better)
df$delta_logscore_mean <- df$logscore_mean_kd - df$logscore_mean_em
df$delta_brier <- df$brier_kd - df$brier_em

df$winner_logscore <- ifelse(df$delta_logscore_mean > 0, "kd",
                             ifelse(df$delta_logscore_mean < 0, "em", "tie"))

df$winner_brier <- ifelse(df$delta_brier < 0, "kd",
                          ifelse(df$delta_brier > 0, "em", "tie"))

df$time_ratio_kd_over_em <- df$time_kd / df$time_em

write.csv(df, OUT_DELTAS, row.names = FALSE)

make_p <- function(x, method = c("t", "wilcox")) {
  method <- match.arg(method)
  if (length(x) < 2) return(NA_real_)
  if (all(!is.finite(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0) return(NA_real_)
  if (method == "t") {
    return(tryCatch(t.test(x)$p.value, error = function(e) NA_real_))
  }
  return(tryCatch(wilcox.test(x)$p.value, error = function(e) NA_real_))
}

split_types <- unique(df$split_type)
rows <- list()

for (st in split_types) {
  sub <- df[df$split_type == st, , drop = FALSE]
  n <- nrow(sub)

  rows[[st]] <- data.frame(
    split_type = st,
    n = n,
    mean_delta_logscore_mean = mean(sub$delta_logscore_mean),
    sd_delta_logscore_mean = if (n > 1) sd(sub$delta_logscore_mean) else NA_real_,
    wins_logscore_kd = sum(sub$winner_logscore == "kd"),
    wins_logscore_em = sum(sub$winner_logscore == "em"),
    wins_logscore_tie = sum(sub$winner_logscore == "tie"),
    mean_delta_brier = mean(sub$delta_brier),
    sd_delta_brier = if (n > 1) sd(sub$delta_brier) else NA_real_,
    wins_brier_kd = sum(sub$winner_brier == "kd"),
    wins_brier_em = sum(sub$winner_brier == "em"),
    wins_brier_tie = sum(sub$winner_brier == "tie"),
    mean_time_ratio_kd_over_em = mean(sub$time_ratio_kd_over_em),
    p_t_logscore = make_p(sub$delta_logscore_mean, "t"),
    p_wilcox_logscore = make_p(sub$delta_logscore_mean, "wilcox"),
    p_t_brier = make_p(sub$delta_brier, "t"),
    p_wilcox_brier = make_p(sub$delta_brier, "wilcox"),
    stringsAsFactors = FALSE
  )
}

summary_df <- do.call(rbind, rows)
write.csv(summary_df, OUT_SUMMARY, row.names = FALSE)

# Write a short markdown summary
lines <- c(
  "# V2 Eval — Comparação de Métricas (emIRT vs dynIRT_KD)",
  "",
  "Direção dos deltas:",
  "- `delta_logscore_mean` > 0 favorece dynIRT_KD",
  "- `delta_brier` < 0 favorece dynIRT_KD",
  "",
  "## Resumo por split_type",
  ""
)

for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  lines <- c(lines,
             sprintf("- %s (n=%d): logscore wins KD=%d, EM=%d, tie=%d; mean Δlogscore=%.6f; Brier wins KD=%d, EM=%d, tie=%d; mean ΔBrier=%.6f; mean time ratio KD/EM=%.2f",
                     r$split_type, r$n,
                     r$wins_logscore_kd, r$wins_logscore_em, r$wins_logscore_tie,
                     r$mean_delta_logscore_mean,
                     r$wins_brier_kd, r$wins_brier_em, r$wins_brier_tie,
                     r$mean_delta_brier,
                     r$mean_time_ratio_kd_over_em))
}

writeLines(lines, OUT_MD)

cat("Wrote:\n")
cat("- ", OUT_DELTAS, "\n", sep = "")
cat("- ", OUT_SUMMARY, "\n", sep = "")
cat("- ", OUT_MD, "\n", sep = "")
