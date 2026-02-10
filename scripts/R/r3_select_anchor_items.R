#!/usr/bin/env Rscript
set.seed(2026)

suppressPackageStartupMessages({
  library(dplyr)
})

domains <- c(
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)

out_dir <- file.path("outputs", "r3_item_anchors")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

codebook_path <- file.path("data", "processed", "item_codebook.csv")
if (!file.exists(codebook_path)) stop("Missing: ", codebook_path)
codebook <- utils::read.csv(codebook_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

required_cols <- c("issue_area", "item_id", "source", "item_name", "treaty_type", "treaty_open_year")
missing_cols <- setdiff(required_cols, names(codebook))
if (length(missing_cols) > 0) stop("item_codebook.csv missing cols: ", paste(missing_cols, collapse = ", "))

# Theory-driven targets: regex + optional year filter to avoid pulling "implementing agreements" etc.
theory_targets <- list(
  investment = list(
    list(label = "ICSID Convention", regex = "icsid|settlement of investment disputes", year = NA_integer_)
  ),
  security = list(
    list(label = "NATO founding treaty", regex = "north atlantic treaty|nato", year = 1949L)
  ),
  environment = list(
    list(label = "Paris Agreement", regex = "paris agreement", year = 2015L),
    list(label = "Montreal Protocol", regex = "montreal protocol", year = 1987L)
  ),
  human_rights = list(
    list(label = "ICCPR", regex = "covenant on civil and political rights|\\biccp\\b|\\biccpR\\b", year = 1966L),
    list(label = "ICESCR", regex = "covenant on economic, social and cultural rights|\\bicescr\\b", year = 1966L)
  ),
  arms_control = list(
    list(label = "CWC", regex = "chemical weapons|prohibition of chemical weapons", year = 1993L),
    list(label = "CTBT", regex = "comprehensive nuclear[- ]?test[- ]?ban|\\bctbt\\b", year = 1996L)
  ),
  intellectual_property = list(
    list(label = "Berne Convention", regex = "berne convention", year = 1886L),
    list(label = "WIPO Copyright Treaty", regex = "wipo copyright treaty|\\bwct\\b", year = 1996L)
  )
)

get_v7_betas <- function(dom) {
  p <- sprintf("outputs/v7_country_anchors/%s_results.rds", dom)
  if (!file.exists(p)) stop("Missing V7 baseline: ", p)
  res <- readRDS(p)
  beta <- as.matrix(res$beta)
  if (ncol(beta) < 2) stop("Unexpected beta shape for domain=", dom)
  item_id <- as.character(res$item_labels)
  if (length(item_id) != nrow(beta)) stop("item_labels length != nrow(beta) for domain=", dom)
  data.frame(
    issue_area = dom,
    item_id = item_id,
    base_item_id = sub("__.*$", "", item_id),
    v7_beta1 = as.numeric(beta[, 1]),
    v7_beta2 = as.numeric(beta[, 2]),
    stringsAsFactors = FALSE
  )
}

all_rows <- list()

cat("R3 Phase 1: Selecting anchor items\n")
cat(sprintf("Codebook: %s | rows=%d\n\n", codebook_path, nrow(codebook)))

for (dom in domains) {
  cat(sprintf("Domain: %s\n", dom))

  cb_dom <- codebook %>%
    dplyr::filter(.data$issue_area == dom)

  v7 <- get_v7_betas(dom)
  v7_dom <- v7 %>%
    dplyr::filter(.data$issue_area == dom)

  # ---- Set A (theory-driven) ----
  targets <- theory_targets[[dom]]
  theory_rows <- list()
  if (!is.null(targets) && length(targets) > 0) {
    for (t in targets) {
      nm <- cb_dom$item_name
      nm2 <- ifelse(is.na(nm), "", nm)
      hit <- grepl(t$regex, tolower(nm2), perl = TRUE)
      cand_all <- cb_dom[hit, , drop = FALSE]
      if (nrow(cand_all) == 0) {
        warning(sprintf("[R3] Theory anchor not found for domain=%s: %s", dom, t$label))
        next
      }

      v7_bases <- unique(v7_dom$base_item_id)

      # Prefer exact-year match if provided; but if that base item is not in V7, relax to any match present in V7.
      cand <- cand_all
      if (!is.na(t$year)) {
        cand_year <- cand_all[cand_all$treaty_open_year == t$year, , drop = FALSE]
        if (nrow(cand_year) > 0) {
          cand_year2 <- cand_year[cand_year$item_id %in% v7_bases, , drop = FALSE]
          if (nrow(cand_year2) > 0) cand <- cand_year2
        }
      }

      # Keep only candidates that actually appear in V7 (by base ID).
      cand <- cand[cand$item_id %in% v7_bases, , drop = FALSE]
      if (nrow(cand) == 0) {
        warning(sprintf("[R3] Theory anchor found in codebook but not present in V7 results (domain=%s, label=%s).",
                        dom, t$label))
        next
      }

      if (!is.na(t$year)) {
        cand$year_dist <- abs(as.integer(cand$treaty_open_year) - as.integer(t$year))
        cand <- cand[order(cand$year_dist, cand$treaty_open_year, cand$item_id), , drop = FALSE]
      } else {
        cand <- cand[order(cand$treaty_open_year, cand$item_id), , drop = FALSE]
      }
      pick_cb <- cand[1, , drop = FALSE]

      v7_hits <- v7_dom %>% dplyr::filter(.data$base_item_id == pick_cb$item_id)
      v7_hits <- v7_hits[order(abs(v7_hits$v7_beta1), decreasing = TRUE), , drop = FALSE]
      pick_v7 <- v7_hits[1, , drop = FALSE]

      theory_rows[[length(theory_rows) + 1L]] <- data.frame(
        domain = dom,
        set_name = "A",
        item_id = pick_v7$item_id, # expanded ID used in estimation
        item_name = as.character(pick_cb$item_name),
        v7_beta1 = as.numeric(pick_v7$v7_beta1),
        v7_beta2 = as.numeric(pick_v7$v7_beta2),
        expected_sign = "positive",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(theory_rows) == 0) {
    cat("  Set A (theory): none found\n")
  } else {
    cat(sprintf("  Set A (theory): %d items\n", length(theory_rows)))
    all_rows[[length(all_rows) + 1L]] <- dplyr::bind_rows(theory_rows)
  }

  # ---- Set B (data-driven) ----
  top_idx <- order(abs(v7_dom$v7_beta1), decreasing = TRUE)
  top_idx <- top_idx[seq_len(min(3L, length(top_idx)))]
  data_items <- v7_dom[top_idx, , drop = FALSE] %>%
    dplyr::left_join(cb_dom %>% dplyr::select(issue_area, item_id, item_name),
                     by = c("issue_area" = "issue_area", "base_item_id" = "item_id")) %>%
    dplyr::mutate(
      domain = .data$issue_area,
      set_name = "B",
      expected_sign = NA_character_
    ) %>%
    dplyr::select(domain, set_name, item_id, item_name, v7_beta1, v7_beta2, expected_sign)

  cat(sprintf("  Set B (data): %d items (top |beta1|)\n\n", nrow(data_items)))
    all_rows[[length(all_rows) + 1L]] <- data_items
  }

anchors <- dplyr::bind_rows(all_rows) %>%
  dplyr::arrange(.data$domain, .data$set_name, dplyr::desc(abs(.data$v7_beta1)))

out_path <- file.path(out_dir, "anchor_items.csv")
utils::write.csv(anchors, out_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("Anchor items (saved): ", out_path, "\n", sep = "")
print(anchors)
