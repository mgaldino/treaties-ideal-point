#!/usr/bin/env Rscript

# R2 Robustness Check: Alternative country anchor sensitivity (selection step)
# Selects two alternative sets of 3 anchor countries per domain:
#   Alt1: PCA-driven (2nd/3rd extremes on PC1 for dim1+, dim1-; dim2 ~ near-zero PC1)
#   Alt2: Theory-driven (from candidate lists; dim2 ~ plausibly orthogonal, medium PC1)

options(stringsAsFactors = FALSE)

domains <- c(
  "investment",
  "security",
  "environment",
  "human_rights",
  "arms_control",
  "intellectual_property"
)

# V7 baseline anchors to exclude (do NOT reuse these in alternative sets)
v7_baseline <- list(
  investment            = list(dim1_pos = "DNK", dim1_neg = "IRN", dim2 = "CHN"),
  security              = list(dim1_pos = "DNK", dim1_neg = "IRN", dim2 = "UKR"),
  environment           = list(dim1_pos = "DNK", dim1_neg = "SAU", dim2 = "AUS"),
  human_rights          = list(dim1_pos = "DNK", dim1_neg = "PRK", dim2 = "USA"),
  arms_control          = list(dim1_pos = "NZL", dim1_neg = "ISR", dim2 = "IND"),
  intellectual_property = list(dim1_pos = "DNK", dim1_neg = "AGO", dim2 = "BRA")
)

theory_dim1_pos <- c("NOR", "SWE", "NLD", "FIN", "CHE")
theory_dim1_neg <- c("SYR", "SDN", "ERI", "MMR", "LBY")

# Plausible dim2 pool: countries that could separate a confounder from dim1,
# with a preference for medium PC1 (operationalized below).
theory_dim2_pool <- c(
  "RUS", "TUR", "IDN", "EGY", "MEX", "ZAF", "ARG", "CHL", "POL", "HUN",
  "KOR", "JPN", "SGP", "ISR", "SAU", "IRN", "IND", "BRA", "CHN", "USA"
)

dir.create("outputs/r2_alt_anchors", recursive = TRUE, showWarnings = FALSE)

read_pca <- function(domain) {
  f <- file.path("outputs/eda_phase2", sprintf("pca_country_loadings_%s.csv", domain))
  if (!file.exists(f)) stop(sprintf("Missing PCA loadings file: %s", f))
  pca <- read.csv(f)
  # Project files may use either `country_code` or `iso3`.
  if (!("pc1" %in% names(pca))) {
    stop(sprintf("PCA file %s must contain column: pc1", f))
  }
  if (!("country_code" %in% names(pca))) {
    if ("iso3" %in% names(pca)) {
      pca$country_code <- pca$iso3
    } else {
      stop(sprintf("PCA file %s must contain either `country_code` or `iso3`", f))
    }
  }
  pca$country_code <- toupper(trimws(pca$country_code))
  pca$pc1 <- as.numeric(pca$pc1)
  pca
}

read_flow_codes <- function(domain) {
  f <- file.path("data/processed", sprintf("%s_flow_matrix.rds", domain))
  if (!file.exists(f)) stop(sprintf("Missing flow matrix: %s", f))
  flow <- readRDS(f)
  codes <- toupper(trimws(flow$country_codes))
  if (any(!nzchar(codes))) stop(sprintf("Empty country code(s) in flow matrix for domain %s", domain))
  codes
}

orient_pc1_using_v7 <- function(pca, domain) {
  v7 <- v7_baseline[[domain]]
  if (is.null(v7)) return(pca)
  pc1_pos <- get_pc1(pca, v7$dim1_pos)
  pc1_neg <- get_pc1(pca, v7$dim1_neg)
  if (!is.na(pc1_pos) && !is.na(pc1_neg) && pc1_pos < pc1_neg) {
    # PCA sign is arbitrary. Flip so that higher PC1 corresponds to the V7 dim1_pos anchor.
    pca$pc1 <- -pca$pc1
    cat(sprintf("  - PC1 oriented by V7 anchors: flipped sign (pc1[%s]=%.4f < pc1[%s]=%.4f)\n",
                v7$dim1_pos, pc1_pos, v7$dim1_neg, pc1_neg))
  }
  pca
}

pick_first_available <- function(candidates, available_set, excluded_set, label) {
  candidates <- unique(toupper(trimws(candidates)))
  for (iso in candidates) {
    if (is.na(iso) || !nzchar(iso)) next
    if (iso %in% excluded_set) {
      cat(sprintf("  - skip %s candidate %s (excluded)\n", label, iso))
      next
    }
    if (!(iso %in% available_set)) {
      cat(sprintf("  - skip %s candidate %s (not in flow matrix)\n", label, iso))
      next
    }
    return(iso)
  }
  NA_character_
}

pick_dim2_by_pc1_near_zero <- function(pca, available_set, excluded_set, label) {
  p <- pca
  p <- p[!is.na(p$pc1), , drop = FALSE]
  p$country_code <- toupper(trimws(p$country_code))
  p <- p[p$country_code %in% available_set, , drop = FALSE]
  p <- p[!(p$country_code %in% excluded_set), , drop = FALSE]
  if (nrow(p) == 0) return(NA_character_)
  p <- p[order(abs(p$pc1), decreasing = FALSE), , drop = FALSE]
  iso <- p$country_code[1]
  if (is.na(iso)) return(NA_character_)
  cat(sprintf("  - %s dim2 picked by near-zero PC1: %s (pc1=%.4f)\n",
              label, iso, p$pc1[1]))
  iso
}

pick_dim2_theory_pool <- function(pca, available_set, excluded_set, label) {
  p <- pca
  p <- p[!is.na(p$pc1), , drop = FALSE]
  p$country_code <- toupper(trimws(p$country_code))
  p <- p[p$country_code %in% available_set, , drop = FALSE]
  p <- p[!(p$country_code %in% excluded_set), , drop = FALSE]
  if (nrow(p) == 0) return(NA_character_)

  # Prefer dim2 candidates that exist and have medium PC1 (middle 20% by rank).
  p_sorted <- p[order(p$pc1), , drop = FALSE]
  n <- nrow(p_sorted)
  lo <- max(1L, floor(n * 0.4))
  hi <- min(n, ceiling(n * 0.6))
  medium_set <- p_sorted$country_code[lo:hi]

  for (iso in unique(toupper(trimws(theory_dim2_pool)))) {
    if (is.na(iso) || !nzchar(iso)) next
    if (iso %in% excluded_set) {
      cat(sprintf("  - skip %s dim2 pool %s (excluded)\n", label, iso))
      next
    }
    if (!(iso %in% available_set)) {
      cat(sprintf("  - skip %s dim2 pool %s (not in flow matrix)\n", label, iso))
      next
    }
    if (!(iso %in% p$country_code)) {
      cat(sprintf("  - skip %s dim2 pool %s (no PCA loading)\n", label, iso))
      next
    }
    if (iso %in% medium_set) {
      pc1 <- p$pc1[match(iso, p$country_code)]
      cat(sprintf("  - %s dim2 picked by theory pool (medium PC1): %s (pc1=%.4f)\n",
                  label, iso, pc1))
      return(iso)
    } else {
      pc1 <- p$pc1[match(iso, p$country_code)]
      cat(sprintf("  - %s dim2 candidate %s exists but PC1 not medium (pc1=%.4f); keep searching\n",
                  label, iso, pc1))
    }
  }

  # Fallback: closest to median PC1 (still within flow and not excluded).
  p_sorted$rank <- seq_len(nrow(p_sorted))
  med <- (nrow(p_sorted) + 1) / 2
  p_sorted$dist_med <- abs(p_sorted$rank - med)
  p_sorted <- p_sorted[order(p_sorted$dist_med, decreasing = FALSE), , drop = FALSE]
  iso <- p_sorted$country_code[1]
  cat(sprintf("  - %s dim2 fallback (closest to median PC1): %s (pc1=%.4f)\n",
              label, iso, p_sorted$pc1[1]))
  iso
}

get_pc1 <- function(pca, iso) {
  if (is.na(iso)) return(NA_real_)
  idx <- match(iso, pca$country_code)
  if (is.na(idx)) return(NA_real_)
  pca$pc1[idx]
}

anchor_rows <- list()

cat("R2 Phase 1: Selecting alternative anchor sets\n\n")
for (domain in domains) {
  cat(sprintf("Domain: %s\n", domain))
  pca <- read_pca(domain)
  available <- read_flow_codes(domain)
  available_set <- unique(available)

  v7 <- v7_baseline[[domain]]
  excluded_v7 <- unique(c(v7$dim1_pos, v7$dim1_neg, v7$dim2))

  # Restrict PCA to available countries and orient PC1 using V7 anchors (high PC1 == pro-ILO).
  p_av <- pca[pca$country_code %in% available_set & !is.na(pca$pc1), , drop = FALSE]
  if (nrow(p_av) == 0) stop(sprintf("No overlapping PCA loadings with flow matrix for domain %s", domain))
  p_av <- orient_pc1_using_v7(p_av, domain)
  p_hi <- p_av[order(p_av$pc1, decreasing = TRUE), , drop = FALSE]
  p_lo <- p_av[order(p_av$pc1, decreasing = FALSE), , drop = FALSE]

  # --- Alt1 (PCA-driven): dim1 anchors from 2nd/3rd extremes ---
  alt1_excl <- excluded_v7
  pos_cand_alt1 <- unique(p_hi$country_code[p_hi$country_code != v7$dim1_pos])[1:3]
  neg_cand_alt1 <- unique(p_lo$country_code[p_lo$country_code != v7$dim1_neg])[1:3]
  # "Use 2nd and 3rd highest/lowest": operationalize as try {2nd, 3rd} after removing baseline.
  pos_try <- pos_cand_alt1[2:3]
  neg_try <- neg_cand_alt1[2:3]

  alt1_dim1_pos <- pick_first_available(pos_try, available_set, alt1_excl, "Alt1 dim1+")
  alt1_excl <- unique(c(alt1_excl, alt1_dim1_pos))
  alt1_dim1_neg <- pick_first_available(neg_try, available_set, alt1_excl, "Alt1 dim1-")
  alt1_excl <- unique(c(alt1_excl, alt1_dim1_neg))

  alt1_dim2 <- pick_dim2_by_pc1_near_zero(p_av, available_set, alt1_excl, "Alt1")
  alt1_excl <- unique(c(alt1_excl, alt1_dim2))

  if (any(is.na(c(alt1_dim1_pos, alt1_dim1_neg, alt1_dim2)))) {
    stop(sprintf("Failed to select complete Alt1 anchor set for domain %s", domain))
  }

  # --- Alt2 (Theory-driven) ---
  alt2_excl <- excluded_v7
  alt2_dim1_pos <- pick_first_available(theory_dim1_pos, available_set, alt2_excl, "Alt2 dim1+")
  alt2_excl <- unique(c(alt2_excl, alt2_dim1_pos))
  alt2_dim1_neg <- pick_first_available(theory_dim1_neg, available_set, alt2_excl, "Alt2 dim1-")
  alt2_excl <- unique(c(alt2_excl, alt2_dim1_neg))
  alt2_dim2 <- pick_dim2_theory_pool(p_av, available_set, alt2_excl, "Alt2")
  alt2_excl <- unique(c(alt2_excl, alt2_dim2))

  if (any(is.na(c(alt2_dim1_pos, alt2_dim1_neg, alt2_dim2)))) {
    stop(sprintf("Failed to select complete Alt2 anchor set for domain %s", domain))
  }

  # Record rows
  add_row <- function(set_name, role, iso) {
    anchor_rows[[length(anchor_rows) + 1L]] <<- data.frame(
      domain = domain,
      set_name = set_name,
      anchor_role = role,
      iso3 = iso,
      pc1_loading = get_pc1(p_av, iso),
      stringsAsFactors = FALSE
    )
  }

  add_row("Alt1", "dim1_pos", alt1_dim1_pos)
  add_row("Alt1", "dim1_neg", alt1_dim1_neg)
  add_row("Alt1", "dim2",     alt1_dim2)

  add_row("Alt2", "dim1_pos", alt2_dim1_pos)
  add_row("Alt2", "dim1_neg", alt2_dim1_neg)
  add_row("Alt2", "dim2",     alt2_dim2)

  cat("Selected anchors:\n")
  cat(sprintf("  Alt1: dim1_pos=%s (pc1=%.4f), dim1_neg=%s (pc1=%.4f), dim2=%s (pc1=%.4f)\n",
              alt1_dim1_pos, get_pc1(p_av, alt1_dim1_pos),
              alt1_dim1_neg, get_pc1(p_av, alt1_dim1_neg),
              alt1_dim2, get_pc1(p_av, alt1_dim2)))
  cat(sprintf("  Alt2: dim1_pos=%s (pc1=%.4f), dim1_neg=%s (pc1=%.4f), dim2=%s (pc1=%.4f)\n",
              alt2_dim1_pos, get_pc1(p_av, alt2_dim1_pos),
              alt2_dim1_neg, get_pc1(p_av, alt2_dim1_neg),
              alt2_dim2, get_pc1(p_av, alt2_dim2)))
  cat("\n")
}

anchor_df <- do.call(rbind, anchor_rows)
anchor_df <- anchor_df[order(anchor_df$domain, anchor_df$set_name, anchor_df$anchor_role), , drop = FALSE]

out_csv <- file.path("outputs/r2_alt_anchors", "anchor_sets.csv")
write.csv(anchor_df, out_csv, row.names = FALSE)
cat(sprintf("Saved: %s\n", out_csv))
