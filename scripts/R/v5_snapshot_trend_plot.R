#!/usr/bin/env Rscript
# Plot convergence diagnostics over snapshots.
# Usage: Rscript scripts/R/v5_snapshot_trend_plot.R [domain] [summary_csv] [output_pdf]
# Example: Rscript scripts/R/v5_snapshot_trend_plot.R investment

args <- commandArgs(trailingOnly = TRUE)

domain <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "investment"
summary_csv <- if (length(args) >= 2 && nzchar(args[2])) {
  args[2]
} else {
  "outputs/v5_per_domain_2d_diag/diagnostic_summary.csv"
}
output_pdf <- if (length(args) >= 3 && nzchar(args[3])) {
  args[3]
} else {
  file.path("outputs/v5_per_domain_2d_diag",
            paste0(domain, "_snapshot_trends.pdf"))
}

if (!file.exists(summary_csv)) stop("Missing: ", summary_csv)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Please install it with install.packages('ggplot2').")
}

df <- read.csv(summary_csv, stringsAsFactors = FALSE)
df <- df[df$domain == domain, , drop = FALSE]
if (nrow(df) == 0) stop("No rows for domain: ", domain)

df$iter_prev <- as.integer(df$iter_prev)
df$iter_curr <- as.integer(df$iter_curr)
df$loglik_delta <- as.numeric(df$loglik_delta)
df$p95_abs_mean <- as.numeric(df$p95_abs_mean)

df <- df[order(df$iter_prev), , drop = FALSE]

plot_df <- rbind(
  data.frame(
    iter = df$iter_curr,
    metric = "Delta da log-verossimilhança",
    value = df$loglik_delta,
    stringsAsFactors = FALSE
  ),
  data.frame(
    iter = df$iter_curr,
    metric = "P95 de |Δx_mean|",
    value = df$p95_abs_mean,
    stringsAsFactors = FALSE
  )
)

p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = iter, y = value)) +
  ggplot2::geom_line(color = "#2C3E50", linewidth = 0.6) +
  ggplot2::geom_point(color = "#2C3E50", size = 1.4) +
  ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  ggplot2::labs(
    title = sprintf("Figura 1 — Tendência de estabilização (%s)", domain),
    subtitle = "Snapshots a cada 500 iterações",
    x = "Iteração (snapshot)",
    y = "Valor",
    caption = sprintf("Fonte: %s; acesso 2026-02-07", summary_csv)
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title.position = "plot",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(output_pdf, plot = p, width = 7, height = 6, units = "in")

cat(sprintf("Saved plot: %s\n", output_pdf))
