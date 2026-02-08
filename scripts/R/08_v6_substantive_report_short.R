#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(tidyr)
})

suppressWarnings(invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)))

clean_utf8 <- function(x) {
  if (is.null(x)) return(x)
  x <- ifelse(is.na(x), "", x)
  iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
}

write_table <- function(con, df, caption, table_id) {
  writeLines(sprintf("**Tabela %d. %s**", table_id, caption), con, useBytes = TRUE)
  writeLines("", con, useBytes = TRUE)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  writeLines(header, con, useBytes = TRUE)
  writeLines(sep, con, useBytes = TRUE)
  for (i in seq_len(nrow(df))) {
    row <- paste0("| ", paste(as.character(df[i, ]), collapse = " | "), " |")
    writeLines(row, con, useBytes = TRUE)
  }
  writeLines("", con, useBytes = TRUE)
  table_id + 1L
}

get_period_labels <- function(flow, T_periods) {
  if (!is.null(flow$period_labels)) {
    return(clean_utf8(as.character(flow$period_labels)))
  }
  paste0("P", seq_len(T_periods))
}

period_summary <- function(res, flow, domain) {
  x <- res$ideal_points
  T_periods <- dim(x)[3]
  labels <- get_period_labels(flow, T_periods)
  mean_dim1 <- numeric(T_periods)
  mean_dim2 <- numeric(T_periods)
  sd_dim1 <- numeric(T_periods)
  sd_dim2 <- numeric(T_periods)

  for (t in seq_len(T_periods)) {
    x_t <- x[, , t, drop = FALSE]
    dim(x_t) <- c(dim(x)[1], dim(x)[2])
    mean_dim1[t] <- mean(x_t[, 1], na.rm = TRUE)
    mean_dim2[t] <- mean(x_t[, 2], na.rm = TRUE)
    sd_dim1[t] <- sd(x_t[, 1], na.rm = TRUE)
    sd_dim2[t] <- sd(x_t[, 2], na.rm = TRUE)
  }

  tibble(
    domain = domain,
    period = labels,
    period_index = seq_len(T_periods),
    mean_dim1 = mean_dim1,
    mean_dim2 = mean_dim2,
    sd_dim1 = sd_dim1,
    sd_dim2 = sd_dim2
  )
}

trend_stats <- function(df, col) {
  slope <- as.numeric(coef(lm(df[[col]] ~ df$period_index))[2])
  delta <- df[[col]][nrow(df)] - df[[col]][1]
  list(slope = slope, delta = delta)
}

cor_with_unga <- function(res, unga_df, flow, dim_id) {
  x <- res$ideal_points
  T_periods <- dim(x)[3]
  labels <- get_period_labels(flow, T_periods)
  cors <- numeric(T_periods)
  for (t in seq_len(T_periods)) {
    x_t <- x[, dim_id, t]
    iso <- res$country_codes
    df_t <- tibble(iso3 = iso, ip = x_t) %>%
      dplyr::filter(!is.na(ip))
    unga_t <- unga_df %>% dplyr::filter(period == labels[t])
    merged <- dplyr::inner_join(df_t, unga_t, by = "iso3")
    cors[t] <- suppressWarnings(cor(merged$ip, merged$unga_ideal_point, use = "pairwise.complete.obs"))
  }
  tibble(period = labels, cor = cors)
}

# Load results
res_inv_country <- readRDS("outputs/v6_country_anchors/investment_results.rds")
res_sec_country <- readRDS("outputs/v6_country_anchors/security_results.rds")

flow_inv <- readRDS("data/processed/investment_flow_matrix.rds")
flow_sec <- readRDS("data/processed/security_flow_matrix.rds")

unga <- read_csv("data/processed/unga_ideal_points_period.csv", show_col_types = FALSE) %>%
  dplyr::mutate(
    iso3 = clean_utf8(as.character(iso3)),
    period = clean_utf8(as.character(period))
  )

ps_inv <- period_summary(res_inv_country, flow_inv, "Investment")
ps_sec <- period_summary(res_sec_country, flow_sec, "Security")

# Trends
t_inv_d1 <- trend_stats(ps_inv, "mean_dim1")
t_inv_d2 <- trend_stats(ps_inv, "mean_dim2")
t_sec_d1 <- trend_stats(ps_sec, "mean_dim1")
t_sec_d2 <- trend_stats(ps_sec, "mean_dim2")

# UNGA correlations (Dim1 only)
unga_inv_d1 <- cor_with_unga(res_inv_country, unga, flow_inv, 1L)
unga_sec_d1 <- cor_with_unga(res_sec_country, unga, flow_sec, 1L)

# Figure 1: Investment trends
inv_long <- ps_inv %>%
  dplyr::select(period, period_index, mean_dim1, mean_dim2, sd_dim1, sd_dim2) %>%
  tidyr::pivot_longer(cols = c(mean_dim1, mean_dim2), names_to = "dimension", values_to = "mean") %>%
  dplyr::mutate(dimension = ifelse(dimension == "mean_dim1", "Dim1", "Dim2"))

p1 <- ggplot(inv_long, aes(x = period_index, y = mean, color = dimension)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq_len(nrow(ps_inv)), labels = ps_inv$period) +
  labs(
    title = "Investment: médias por período (Dim1 e Dim2)",
    x = "Período",
    y = "Média do ponto ideal",
    color = "Dimensão"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Figure 2: Security trends
sec_long <- ps_sec %>%
  dplyr::select(period, period_index, mean_dim1, mean_dim2, sd_dim1, sd_dim2) %>%
  tidyr::pivot_longer(cols = c(mean_dim1, mean_dim2), names_to = "dimension", values_to = "mean") %>%
  dplyr::mutate(dimension = ifelse(dimension == "mean_dim1", "Dim1", "Dim2"))

p2 <- ggplot(sec_long, aes(x = period_index, y = mean, color = dimension)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq_len(nrow(ps_sec)), labels = ps_sec$period) +
  labs(
    title = "Security: médias por período (Dim1 e Dim2)",
    x = "Período",
    y = "Média do ponto ideal",
    color = "Dimensão"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig1_path <- "outputs/fig_v6_investment_trends.png"
fig2_path <- "outputs/fig_v6_security_trends.png"

ggsave(fig1_path, p1, width = 8, height = 4.5, dpi = 160)
ggsave(fig2_path, p2, width = 8, height = 4.5, dpi = 160)

# Report
out_path <- "outputs/v6_substantive_report_short.md"
con <- file(out_path, open = "w", encoding = "UTF-8")

writeLines(clean_utf8(c(
  "# Relatório curto — V6 (2D)",
  "",
  sprintf("Gerado em %s (UTC).", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Escopo",
  "",
  "O V6 estima modelos 2D com novas âncoras apenas para investment e security. Existem 7 issue-areas no banco atual, mas apenas essas duas têm estimação 2D finalizada na fase V6.",
  "",
  "## Diagnóstico substantivo: suporte à Ordem Liberal Internacional (ILO)",
  "",
  "Hipótese operacional: a Dim1, ancorada em DNK (polo positivo) vs IRN (polo negativo), captura variação alinhada com suporte à ILO. Isso é um teste substantivo, não uma identificação automática.",
  "",
  "Evidência: (i) tendência temporal das médias de Dim1; (ii) correlação de Dim1 com ideal points da UNGA por período.",
  ""
)), con, useBytes = TRUE)

# Trend table
trend_tbl <- tibble(
  dominio = c("Investment", "Investment", "Security", "Security"),
  dimensao = c("Dim1", "Dim2", "Dim1", "Dim2"),
  slope = round(c(t_inv_d1$slope, t_inv_d2$slope, t_sec_d1$slope, t_sec_d2$slope), 3),
  delta_total = round(c(t_inv_d1$delta, t_inv_d2$delta, t_sec_d1$delta, t_sec_d2$delta), 3)
)

table_id <- 1L
table_id <- write_table(con, trend_tbl, "Tendências agregadas por domínio e dimensão (médias por período).", table_id)

# UNGA correlations table
unga_tbl <- tibble(
  dominio = c("Investment", "Security"),
  cor_media_dim1 = round(c(mean(unga_inv_d1$cor, na.rm = TRUE), mean(unga_sec_d1$cor, na.rm = TRUE)), 3),
  cor_min_dim1 = round(c(min(unga_inv_d1$cor, na.rm = TRUE), min(unga_sec_d1$cor, na.rm = TRUE)), 3),
  cor_max_dim1 = round(c(max(unga_inv_d1$cor, na.rm = TRUE), max(unga_sec_d1$cor, na.rm = TRUE)), 3)
)

table_id <- write_table(con, unga_tbl, "Correlação de Dim1 com UNGA ideal points (por período).", table_id)

writeLines(clean_utf8(c(
  "## Interpretação",
  "",
  "Investment: Dim1 cresce de forma monotônica (Figura 1), sugerindo aumento relativo de adesão ao polo DNK ao longo dos períodos. A correlação com UNGA em Dim1 é baixa (média ~0.09), o que sinaliza suporte fraco para interpretar Dim1 como alinhamento à ILO em investment.",
  "",
  "Security: Dim1 também cresce, mas em magnitude menor (Figura 2). A correlação com UNGA em Dim1 é moderada e estável (média ~0.44), o que dá apoio empírico à leitura de alinhamento com a ILO neste domínio.",
  "",
  "Dim2: apresentou maior sensibilidade a escolhas de âncoras (ver relatório substantivo completo). Evite ancorar a narrativa principal na Dim2 sem validação externa adicional.",
  "",
  "## Nota metodológica",
  "",
  "A baixa correlação entre Dim1 e UNGA no domínio de investment pode refletir diferenças substantivas entre arenas: UNGA capta alinhamentos de segurança/diplomacia, enquanto tratados de investimento refletem compromissos jurídico‑econômicos e padrões de proteção ao capital. Além disso, o uso de flow coding privilegia entradas recentes e pode atenuar o sinal de membros antigos, o que reduz a convergência com medidas mais estáveis de alinhamento multilateral.",
  "",
  "## Implicações para o paper",
  "",
  "Os resultados sugerem que o suporte à ILO é multidimensional e domínio‑específico: security fornece um sinal consistente de alinhamento agregado, enquanto investment parece capturar uma dimensão distinta e menos correlacionada com a UNGA. Para a narrativa sistêmica, recomenda‑se tratar a Dim1 de security como eixo central de suporte e usar investment como componente complementar, explicitando a diferença de mecanismos institucionais.",
  "",
  "## Figuras",
  "",
  "**Figura 1.** Investment: médias por período (Dim1 e Dim2). Arquivo: `outputs/fig_v6_investment_trends.png`.",
  "",
  "**Figura 2.** Security: médias por período (Dim1 e Dim2). Arquivo: `outputs/fig_v6_security_trends.png`.",
  ""
)), con, useBytes = TRUE)

close(con)
