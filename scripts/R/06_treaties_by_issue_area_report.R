#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

suppressWarnings(invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)))

in_path <- "data/processed/item_codebook.csv"
out_path <- "outputs/treaties_by_issue_area.md"

clean_utf8 <- function(x) {
  if (is.null(x)) return(x)
  x <- ifelse(is.na(x), "", x)
  iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
}

items <- read_csv(in_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    issue_area = clean_utf8(as.character(issue_area)),
    item_id = clean_utf8(as.character(item_id)),
    item_name = clean_utf8(as.character(item_name)),
    source = clean_utf8(as.character(source)),
    treaty_type = clean_utf8(as.character(treaty_type)),
    treaty_open_year = clean_utf8(as.character(treaty_open_year)),
    source_url = clean_utf8(as.character(source_url))
  ) %>%
  dplyr::select(issue_area, item_id, item_name, source, treaty_type, treaty_open_year, source_url)

issue_order <- c(
  "trade",
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)
all_areas <- unique(items$issue_area)
ordered_areas <- c(issue_order, setdiff(all_areas, issue_order))
ordered_areas <- ordered_areas[!is.na(ordered_areas) & ordered_areas != ""]

items <- items %>%
  dplyr::arrange(issue_area, item_name, item_id)

header <- c(
  "# Tratados por issue-area",
  "",
  sprintf("Gerado em %s (UTC).", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  sprintf("Fonte: `%s`.", in_path),
  "",
  "Observação: a lista é completa e pode ser longa em alguns domínios (ex.: investment).",
  ""
)

con <- file(out_path, open = "w", encoding = "UTF-8")
writeLines(clean_utf8(header), con, useBytes = TRUE)

for (area in ordered_areas) {
  area_items <- items %>% dplyr::filter(issue_area == area)
  writeLines(clean_utf8(c(sprintf("## %s", area), "")), con, useBytes = TRUE)
  if (nrow(area_items) == 0) {
    writeLines(clean_utf8(c("- (sem itens)", "")), con, useBytes = TRUE)
    next
  }
  years <- ifelse(
    is.na(area_items$treaty_open_year) | area_items$treaty_open_year == "",
    "ano desconhecido",
    area_items$treaty_open_year
  )
  lines <- sprintf(
    "- %s [%s] - %s; %s; %s",
    area_items$item_name,
    area_items$item_id,
    area_items$source,
    area_items$treaty_type,
    years
  )
  writeLines(clean_utf8(lines), con, useBytes = TRUE)
  writeLines("", con, useBytes = TRUE)
}
close(con)
