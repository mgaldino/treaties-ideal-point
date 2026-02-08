#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
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

period_summary <- function(res, flow) {
  x <- res$ideal_points
  T_periods <- dim(x)[3]
  labels <- get_period_labels(flow, T_periods)
  mean_dim1 <- numeric(T_periods)
  mean_dim2 <- numeric(T_periods)
  sd_dim1 <- numeric(T_periods)
  sd_dim2 <- numeric(T_periods)
  n_active <- integer(T_periods)

  for (t in seq_len(T_periods)) {
    x_t <- x[, , t, drop = FALSE]
    dim(x_t) <- c(dim(x)[1], dim(x)[2])
    mean_dim1[t] <- mean(x_t[, 1], na.rm = TRUE)
    mean_dim2[t] <- mean(x_t[, 2], na.rm = TRUE)
    sd_dim1[t] <- sd(x_t[, 1], na.rm = TRUE)
    sd_dim2[t] <- sd(x_t[, 2], na.rm = TRUE)
    n_active[t] <- sum(!is.na(x_t[, 1]))
  }

  tibble(
    period = labels,
    period_index = seq_len(T_periods),
    n_active = n_active,
    mean_dim1 = mean_dim1,
    sd_dim1 = sd_dim1,
    mean_dim2 = mean_dim2,
    sd_dim2 = sd_dim2
  )
}

trend_stats <- function(df, col) {
  slope <- as.numeric(coef(lm(df[[col]] ~ df$period_index))[2])
  delta <- df[[col]][nrow(df)] - df[[col]][1]
  list(slope = slope, delta = delta)
}

extreme_table <- function(res, country_map, dim_id, n = 10L) {
  x_mean <- res$ideal_points_mean
  codes <- res$country_codes
  df <- tibble(
    iso3 = clean_utf8(as.character(codes)),
    score = as.numeric(x_mean[, dim_id])
  ) %>%
    dplyr::left_join(country_map, by = "iso3") %>%
    dplyr::mutate(country_name = ifelse(is.na(country_name) | country_name == "", iso3, country_name)) %>%
    dplyr::filter(!is.na(score)) %>%
    dplyr::select(country_name, iso3, score)

  top <- df %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::mutate(rank = dplyr::row_number())
  bottom <- df %>%
    dplyr::arrange(score) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::mutate(rank = dplyr::row_number())

  tibble(
    rank = top$rank,
    top_country = top$country_name,
    top_iso = top$iso3,
    top_score = round(top$score, 3),
    bottom_country = bottom$country_name,
    bottom_iso = bottom$iso3,
    bottom_score = round(bottom$score, 3)
  )
}

jaccard_overlap <- function(res_a, res_b, dim_id, n = 10L) {
  x_a <- res_a$ideal_points_mean[, dim_id]
  x_b <- res_b$ideal_points_mean[, dim_id]
  iso <- res_a$country_codes
  df_a <- tibble(iso3 = iso, score = x_a) %>% dplyr::filter(!is.na(score))
  df_b <- tibble(iso3 = iso, score = x_b) %>% dplyr::filter(!is.na(score))
  top_a <- df_a %>% dplyr::arrange(dplyr::desc(score)) %>% dplyr::slice_head(n = n)
  top_b <- df_b %>% dplyr::arrange(dplyr::desc(score)) %>% dplyr::slice_head(n = n)
  bot_a <- df_a %>% dplyr::arrange(score) %>% dplyr::slice_head(n = n)
  bot_b <- df_b %>% dplyr::arrange(score) %>% dplyr::slice_head(n = n)

  j_top <- length(intersect(top_a$iso3, top_b$iso3)) / length(union(top_a$iso3, top_b$iso3))
  j_bot <- length(intersect(bot_a$iso3, bot_b$iso3)) / length(union(bot_a$iso3, bot_b$iso3))
  list(j_top = j_top, j_bot = j_bot)
}

period_cor <- function(res_a, res_b, dim_id) {
  x_a <- res_a$ideal_points
  x_b <- res_b$ideal_points
  T_periods <- dim(x_a)[3]
  cors <- numeric(T_periods)
  for (t in seq_len(T_periods)) {
    v_a <- x_a[, dim_id, t]
    v_b <- x_b[, dim_id, t]
    cors[t] <- suppressWarnings(cor(v_a, v_b, use = "pairwise.complete.obs"))
  }
  cors
}

country_map <- read_csv("data/processed/country_codebook.csv", show_col_types = FALSE) %>%
  dplyr::mutate(
    iso3 = clean_utf8(as.character(iso3)),
    country_name = clean_utf8(as.character(country_name))
  ) %>%
  dplyr::distinct(iso3, .keep_all = TRUE) %>%
  dplyr::select(iso3, country_name)

# Load results
res_inv_country <- readRDS("outputs/v6_country_anchors/investment_results.rds")
res_inv_item <- readRDS("outputs/v6_item_anchors/investment_results.rds")
res_sec_country <- readRDS("outputs/v6_country_anchors/security_results.rds")
res_sec_item <- readRDS("outputs/v6_item_anchors/security_results.rds")

flow_inv <- readRDS("data/processed/investment_flow_matrix.rds")
flow_sec <- readRDS("data/processed/security_flow_matrix.rds")

out_path <- "outputs/v6_substantive_report.md"
con <- file(out_path, open = "w", encoding = "UTF-8")

writeLines(clean_utf8(c(
  "# Relatório V6 — análise substantiva (2D)",
  "",
  sprintf("Gerado em %s (UTC).", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Este relatório resume os resultados substantivos dos modelos 2D estimados na fase V6, com foco em investment e security.",
  "",
  "## 1. Escopo e dados",
  "",
  sprintf("Investment: N=%d, J=%d, T=%d.", nrow(flow_inv$rc), ncol(flow_inv$rc), flow_inv$T),
  sprintf("Security: N=%d, J=%d, T=%d.", nrow(flow_sec$rc), ncol(flow_sec$rc), flow_sec$T),
  "",
  "## 2. Identificação e âncoras",
  "",
  "### 2.1 Investment",
  sprintf("Âncoras por país: %s.", paste(res_inv_country$anchors$iso, collapse = ", ")),
  sprintf("Posições: %s.", paste(apply(res_inv_country$anchors$positions, 1, function(x) paste0("(", paste(x, collapse=","), ")")), collapse = ", ")),
  sprintf("Âncoras por item: %s.", paste(clean_utf8(res_inv_item$anchor_item_labels), collapse = "; ")),
  "",
  "### 2.2 Security",
  sprintf("Âncoras por país: %s.", paste(res_sec_country$anchors$iso, collapse = ", ")),
  sprintf("Posições: %s.", paste(apply(res_sec_country$anchors$positions, 1, function(x) paste0("(", paste(x, collapse=","), ")")), collapse = ", ")),
  sprintf("Âncoras por item: %s.", paste(clean_utf8(res_sec_item$anchor_item_labels), collapse = "; ")),
  ""
)), con, useBytes = TRUE)

# Helper to write domain section
write_domain_section <- function(domain_label, res_country, res_item, flow, con, table_id_start) {
  table_id <- table_id_start
  writeLines(clean_utf8(c(sprintf("## 3. Resultados substantivos — %s", domain_label), "")), con, useBytes = TRUE)

  # Dim 1 extremes
  ext_dim1 <- extreme_table(res_country, country_map, 1L, n = 10L)
  table_id <- write_table(
    con,
    ext_dim1,
    sprintf("Extremos de Dimensão 1 (%s; âncoras por país).", domain_label),
    table_id
  )

  # Dim 2 extremes
  ext_dim2 <- extreme_table(res_country, country_map, 2L, n = 10L)
  table_id <- write_table(
    con,
    ext_dim2,
    sprintf("Extremos de Dimensão 2 (%s; âncoras por país).", domain_label),
    table_id
  )

  # Period summaries
  ps_country <- period_summary(res_country, flow)
  ps_item <- period_summary(res_item, flow)

  ps_country_tbl <- ps_country %>%
    dplyr::mutate(
      mean_dim1 = round(mean_dim1, 3),
      sd_dim1 = round(sd_dim1, 3),
      mean_dim2 = round(mean_dim2, 3),
      sd_dim2 = round(sd_dim2, 3)
    ) %>%
    dplyr::select(period, n_active, mean_dim1, sd_dim1, mean_dim2, sd_dim2)

  table_id <- write_table(
    con,
    ps_country_tbl,
    sprintf("Médias e dispersão por período (%s; âncoras por país).", domain_label),
    table_id
  )

  # Trend stats
  t1 <- trend_stats(ps_country, "mean_dim1")
  t2 <- trend_stats(ps_country, "mean_dim2")

  # Robustness
  cor_dim1 <- suppressWarnings(cor(res_country$ideal_points_mean[,1], res_item$ideal_points_mean[,1], use = "pairwise.complete.obs"))
  cor_dim2 <- suppressWarnings(cor(res_country$ideal_points_mean[,2], res_item$ideal_points_mean[,2], use = "pairwise.complete.obs"))
  cor_per_dim1 <- period_cor(res_country, res_item, 1L)
  cor_per_dim2 <- period_cor(res_country, res_item, 2L)
  jac1 <- jaccard_overlap(res_country, res_item, 1L, n = 10L)
  jac2 <- jaccard_overlap(res_country, res_item, 2L, n = 10L)

  robust_tbl <- tibble(
    indicador = c("Correlação Dim1 (média por país)", "Correlação Dim2 (média por país)",
                  "Correlação Dim1 por período (média)", "Correlação Dim2 por período (média)",
                  "Jaccard Top10 Dim1", "Jaccard Bottom10 Dim1",
                  "Jaccard Top10 Dim2", "Jaccard Bottom10 Dim2"),
    valor = c(
      round(cor_dim1, 3),
      round(cor_dim2, 3),
      round(mean(cor_per_dim1, na.rm = TRUE), 3),
      round(mean(cor_per_dim2, na.rm = TRUE), 3),
      round(jac1$j_top, 3),
      round(jac1$j_bot, 3),
      round(jac2$j_top, 3),
      round(jac2$j_bot, 3)
    )
  )

  table_id <- write_table(
    con,
    robust_tbl,
    sprintf("Robustez entre estratégias (%s).", domain_label),
    table_id
  )

  # Interpretive summary
  writeLines(clean_utf8(c(
    sprintf("Interpretação operacional (Dim1): valores altos aproximam-se do polo definido pela âncora positiva (ex.: %s) e valores baixos do polo negativo (ex.: %s).", res_country$anchors$iso[1], res_country$anchors$iso[2]),
    sprintf("Tendência agregada Dim1: slope=%.3f por período; variação total=%.3f.", t1$slope, t1$delta),
    sprintf("Tendência agregada Dim2: slope=%.3f por período; variação total=%.3f.", t2$slope, t2$delta),
    ""
  )), con, useBytes = TRUE)

  table_id
}

# Investment section
next_id <- write_domain_section("Investment", res_inv_country, res_inv_item, flow_inv, con, 1L)

# Security section
next_id <- write_domain_section("Security", res_sec_country, res_sec_item, flow_sec, con, next_id)

writeLines(clean_utf8(c(
  "## 4. Síntese para o paper (nível sistêmico)",
  "",
  "As séries de médias por período (Dim1) servem como indicador de suporte agregado dentro de cada issue-area. Um declínio persistente indica erosão relativa do polo associado à âncora positiva; um aumento sugere reforço. A interpretação substantiva deve ser feita junto ao desenho de períodos e ao viés de flow coding descrito em docs/research_design_notes.md.",
  "",
  "A robustez entre estratégias (âncoras por país vs por item) é moderada: correlações por dimensão e sobreposição de extremos fornecem um diagnóstico direto de estabilidade identificacional. Em especial, Dim2 tende a ser mais sensível a escolhas de âncora, o que recomenda cautela na narrativa substantiva dessa dimensão.",
  ""
)), con, useBytes = TRUE)

close(con)
