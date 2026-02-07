#!/usr/bin/env Rscript
# Generate markdown diagnostics summary for V5 investment convergence.
# Usage: Rscript scripts/R/v5_generate_diagnostics_md.R [domain] [output_md]

args <- commandArgs(trailingOnly = TRUE)

domain <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "investment"
output_md <- if (length(args) >= 2 && nzchar(args[2])) {
  args[2]
} else {
  file.path("docs", paste0("v5_", domain, "_diagnostics.md"))
}

results_path <- file.path("outputs/v5_per_domain_2d", paste0(domain, "_2d_results.rds"))
summary_csv <- "outputs/v5_per_domain_2d_diag/diagnostic_summary.csv"
trend_plot <- file.path("outputs/v5_per_domain_2d_diag",
                        paste0(domain, "_snapshot_trends.pdf"))

if (!file.exists(results_path)) stop("Missing: ", results_path)
if (!file.exists(summary_csv)) stop("Missing: ", summary_csv)

res <- readRDS(results_path)
summary_df <- read.csv(summary_csv, stringsAsFactors = FALSE)
summary_df <- summary_df[summary_df$domain == domain, , drop = FALSE]
if (nrow(summary_df) == 0) stop("No rows for domain: ", domain)

summary_df$iter_prev <- as.integer(summary_df$iter_prev)
summary_df$iter_curr <- as.integer(summary_df$iter_curr)
summary_df$loglik_delta <- as.numeric(summary_df$loglik_delta)
summary_df$p95_abs_mean <- as.numeric(summary_df$p95_abs_mean)
summary_df$max_abs_mean <- as.numeric(summary_df$max_abs_mean)
summary_df$cor_mean_dim1 <- as.numeric(summary_df$cor_mean_dim1)
summary_df$cor_mean_dim2 <- as.numeric(summary_df$cor_mean_dim2)
summary_df <- summary_df[order(summary_df$iter_prev), , drop = FALSE]

# ---- Loglik trace diagnostics ----
ll <- res$runtime$loglik_trace
ll <- ll[is.finite(ll)]
if (length(ll) < 2) stop("Loglik trace is too short")
delta_ll <- diff(ll)
total_gain <- ll[length(ll)] - ll[1]

first_iter <- 1L
iter_90 <- NA_integer_
iter_95 <- NA_integer_
iter_99 <- NA_integer_
if (is.finite(total_gain) && total_gain > 0) {
  gain <- ll - ll[1]
  iter_90 <- which(gain >= 0.90 * total_gain)[1]
  iter_95 <- which(gain >= 0.95 * total_gain)[1]
  iter_99 <- which(gain >= 0.99 * total_gain)[1]
}

last_100_mean <- mean(tail(delta_ll, 100), na.rm = TRUE)
last_500_mean <- mean(tail(delta_ll, 500), na.rm = TRUE)
first_100_mean <- mean(head(delta_ll, 100), na.rm = TRUE)

# ---- Procrustes alignment across time periods ----
x <- res$ideal_points
if (is.null(x) || length(dim(x)) != 3) stop("ideal_points must be N x K x T")
N <- dim(x)[1]
K <- dim(x)[2]
T_total <- dim(x)[3]

procrustes_align <- function(A, B) {
  # Align A to B using orthogonal Procrustes (rotation/reflection only).
  A <- as.matrix(A)
  B <- as.matrix(B)
  A_center <- scale(A, center = TRUE, scale = FALSE)
  B_center <- scale(B, center = TRUE, scale = FALSE)
  sv <- svd(t(A_center) %*% B_center)
  R <- sv$u %*% t(sv$v)
  A_aligned <- A_center %*% R
  list(A_aligned = A_aligned, B_center = B_center)
}

if (T_total < 2) stop("Need at least 2 periods for Procrustes diagnostics")
proc_rows <- vector("list", T_total - 1L)
for (t in 2:T_total) {
  A <- x[, , t]
  B <- x[, , t - 1L]
  aligned <- procrustes_align(A, B)
  cor_dims <- rep(NA_real_, K)
  for (k in seq_len(K)) {
    cor_dims[k] <- suppressWarnings(stats::cor(aligned$A_aligned[, k],
                                               aligned$B_center[, k],
                                               use = "pairwise.complete.obs"))
  }
  proc_rows[[t - 1L]] <- data.frame(
    period_prev = t - 1L,
    period_curr = t,
    cor_dim1 = cor_dims[1],
    cor_dim2 = if (K >= 2) cor_dims[2] else NA_real_,
    stringsAsFactors = FALSE
  )
}
proc_df <- do.call(rbind, proc_rows)
proc_df$cor_min <- pmin(proc_df$cor_dim1, proc_df$cor_dim2, na.rm = TRUE)
proc_min <- min(proc_df$cor_min, na.rm = TRUE)
proc_mean <- mean(proc_df$cor_min, na.rm = TRUE)

# ---- Beta max norm across snapshots ----
iter_list <- sort(unique(summary_df$iter_curr))
beta_rows <- vector("list", length(iter_list))
for (i in seq_along(iter_list)) {
  it <- iter_list[i]
  snap_path <- file.path("outputs/v5_per_domain_2d_diag",
                         paste0(domain, "_snapshot_iter", it, ".rds"))
  if (!file.exists(snap_path)) next
  snap <- readRDS(snap_path)
  beta <- snap$beta
  if (is.null(beta)) next
  beta <- as.matrix(beta)
  row_norm <- sqrt(rowSums(beta^2))
  beta_rows[[i]] <- data.frame(
    iter = it,
    max_beta_l2 = max(row_norm, na.rm = TRUE),
    max_beta_abs = max(abs(beta), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}
beta_df <- do.call(rbind, beta_rows)
beta_df <- beta_df[order(beta_df$iter), , drop = FALSE]

# ---- Build markdown ----
date_str <- format(Sys.Date(), "%Y-%m-%d")

fmt <- function(x, digits = 3) {
  if (is.na(x)) return("NA")
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

md <- c(
  paste0("# Diagnóstico V5 — ", domain),
  "",
  paste0("Data: ", date_str),
  "",
  "## Arquivos usados",
  "",
  paste0("- Resultados principais: `", results_path, "`"),
  paste0("- Resumo de snapshots: `", summary_csv, "`"),
  paste0("- Gráfico de tendências: `", trend_plot, "`"),
  "",
  "## 1. Traço de log-verossimilhança (LL)",
  "",
  "Observações:",
  "",
  paste0("- LL inicial: ", fmt(ll[1], 3)),
  paste0("- LL final: ", fmt(ll[length(ll)], 3)),
  paste0("- Ganho total de LL: ", fmt(total_gain, 3)),
  paste0("- Iteração com 90% do ganho total: ", ifelse(is.na(iter_90), "NA", iter_90)),
  paste0("- Iteração com 95% do ganho total: ", ifelse(is.na(iter_95), "NA", iter_95)),
  paste0("- Iteração com 99% do ganho total: ", ifelse(is.na(iter_99), "NA", iter_99)),
  paste0("- Média de ΔLL (primeiras 100 iterações): ", fmt(first_100_mean, 4)),
  paste0("- Média de ΔLL (últimas 500 iterações): ", fmt(last_500_mean, 4)),
  paste0("- Média de ΔLL (últimas 100 iterações): ", fmt(last_100_mean, 4)),
  "",
  "Q-function não é calculada no código atual; o diagnóstico usa apenas a LL observada.",
  "",
  "Tabela 1 — Resumo por intervalo de 500 iterações (LL e estabilidade prática).",
  ""
)

table1 <- summary_df[, c("iter_prev", "iter_curr", "loglik_delta",
                         "p95_abs_mean", "max_abs_mean",
                         "cor_mean_dim1", "cor_mean_dim2", "stable_both")]
table1$loglik_delta <- round(table1$loglik_delta, 3)
table1$p95_abs_mean <- round(table1$p95_abs_mean, 3)
table1$max_abs_mean <- round(table1$max_abs_mean, 3)
table1$cor_mean_dim1 <- round(table1$cor_mean_dim1, 3)
table1$cor_mean_dim2 <- round(table1$cor_mean_dim2, 3)

md <- c(md, knitr::kable(table1, format = "markdown"))

md <- c(
  md,
  "",
  "Figura 1 — Tendência de estabilização (LL e p95_abs_mean).",
  "",
  paste0("Arquivo: `", trend_plot, "`"),
  "",
  "## 2. Medida de rotação (Procrustes) entre períodos",
  "",
  "Definição: para cada par de períodos adjacentes (t-1, t), aplicamos Procrustes ortogonal",
  "com centragem (sem escala) para alinhar `x_t` em `x_{t-1}` e calculamos a correlação por dimensão.",
  "",
  paste0("- Correlação mínima por dimensão (mínimo entre períodos): ", fmt(proc_min, 3)),
  paste0("- Correlação média mínima por dimensão: ", fmt(proc_mean, 3)),
  "",
  "Tabela 2 — Correlação por dimensão após alinhamento Procrustes.",
  ""
)

proc_df_print <- proc_df
proc_df_print$cor_dim1 <- round(proc_df_print$cor_dim1, 3)
proc_df_print$cor_dim2 <- round(proc_df_print$cor_dim2, 3)
proc_df_print$cor_min <- round(proc_df_print$cor_min, 3)
md <- c(md, knitr::kable(proc_df_print, format = "markdown"))

md <- c(
  md,
  "",
  "## 3. Norma máxima de β ao longo dos snapshots",
  "",
  "Definição: para cada snapshot, calculamos o maior valor da norma L2 por item",
  "(`max_beta_l2`) e o maior coeficiente absoluto (`max_beta_abs`).",
  "",
  "Tabela 3 — Norma máxima de β por snapshot.",
  ""
)

beta_print <- beta_df
beta_print$max_beta_l2 <- round(beta_print$max_beta_l2, 3)
beta_print$max_beta_abs <- round(beta_print$max_beta_abs, 3)
md <- c(md, knitr::kable(beta_print, format = "markdown"))

writeLines(md, output_md, useBytes = TRUE)
cat(sprintf("Saved markdown: %s\n", output_md))
