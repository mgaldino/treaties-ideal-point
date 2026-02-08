#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

read_csv_or_stop <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  utils::read.csv(path)
}

read_rds_or_stop <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  readRDS(path)
}

parse_treaty_id <- function(item_label) {
  # Expected patterns like: source_treaty-name__1990-1994
  # Treat everything before "__" as the underlying treaty identifier.
  if (grepl("__", item_label, fixed = TRUE)) {
    sub("__.*$", "", item_label)
  } else {
    item_label
  }
}

build_stock_rc <- function(flow, domain) {
  if (is.null(flow$rc) || is.null(flow$bill.session) || is.null(flow$item_labels) || is.null(flow$T)) {
    stop("Flow object missing required fields for domain: ", domain)
  }

  rc <- flow$rc
  if (!is.matrix(rc)) rc <- as.matrix(rc)
  storage.mode(rc) <- "integer"

  J <- ncol(rc)
  if (length(flow$item_labels) != J) {
    stop("Length mismatch item_labels vs rc columns for domain: ", domain)
  }
  if (length(flow$bill.session) != J) {
    stop("Length mismatch bill.session vs rc columns for domain: ", domain)
  }

  bill <- as.integer(flow$bill.session) # 0-indexed
  treaty_ids <- vapply(flow$item_labels, parse_treaty_id, character(1))

  no_delim_n <- sum(!grepl("__", flow$item_labels, fixed = TRUE))
  if (no_delim_n > 0) {
    warning(sprintf(
      "[%s] %d/%d item_labels do not contain '__' delimiter; using full label as treaty id for those items.",
      domain, no_delim_n, J
    ))
  }

  groups <- split(seq_len(J), treaty_ids)
  multi_period_groups <- sum(lengths(groups) > 1L)
  if (multi_period_groups == 0L) {
    warning(sprintf(
      "[%s] No treaty id groups span multiple periods (all groups size 1). Stock propagation cannot increase +1 counts for this domain with current matrix structure.",
      domain
    ))
  }

  dup_period_groups <- 0L
  zeros_after_ratification <- 0L

  for (g in names(groups)) {
    idx <- groups[[g]]
    if (length(idx) <= 1) next

    p <- bill[idx]
    if (any(duplicated(p))) dup_period_groups <- dup_period_groups + 1L

    ord <- order(p, idx)
    idx_ord <- idx[ord]

    rc_sub <- rc[, idx_ord, drop = FALSE]
    rat_int <- (rc_sub == 1L) * 1L
    cum_rat <- t(apply(rat_int, 1, cummax)) > 0L

    # In the existing flow matrices, 0 can appear after ratification (i.e., "no longer at risk").
    # For stock coding, those cells should become +1 as well.
    zeros_after_ratification <- zeros_after_ratification + sum(cum_rat & (rc_sub == 0L))

    # Stock rule: after ratification, subsequent available periods become +1.
    # After a +1 occurs for a treaty, all subsequent periods for that treaty become +1.
    rc_sub[cum_rat] <- 1L
    rc[, idx_ord] <- rc_sub
  }

  if (dup_period_groups > 0) {
    warning(sprintf("[%s] %d treaty groups have duplicate bill.session values within group.", domain, dup_period_groups))
  }
  if (zeros_after_ratification > 0) {
    warning(sprintf("[%s] %d cells were 0 in periods after ratification within treaty groups (converted to +1 under stock coding).", domain, zeros_after_ratification))
  }

  rc
}

counts_per_period <- function(rc, bill_session, T) {
  bill_session <- as.integer(bill_session)
  out <- integer(T)
  for (t in seq_len(T)) {
    cols <- which(bill_session == (t - 1L))
    if (length(cols) == 0) {
      out[t] <- 0L
    } else {
      out[t] <- sum(rc[, cols, drop = FALSE] == 1L)
    }
  }
  out
}

country_row_index <- function(flow, iso3, country_codebook) {
  cc <- flow$country_codes
  if (is.null(cc)) return(NA_integer_)

  i <- match(iso3, cc)
  if (!is.na(i)) return(i)

  # Best-effort: if flow codes aren't ISO3, try mapping via country_codebook.
  if (!is.null(country_codebook) && is.data.frame(country_codebook)) {
    # Identify ISO3 column.
    iso_col <- NULL
    if ("iso3" %in% names(country_codebook)) iso_col <- "iso3"
    if (is.null(iso_col)) {
      iso_candidates <- names(country_codebook)[grepl("iso3", names(country_codebook), ignore.case = TRUE)]
      if (length(iso_candidates) >= 1) iso_col <- iso_candidates[1]
    }
    if (!is.null(iso_col) && iso_col %in% names(country_codebook)) {
      row_cb <- which(country_codebook[[iso_col]] == iso3)
      if (length(row_cb) == 1) {
        # Try matching any other column value against flow codes.
        for (nm in names(country_codebook)) {
          v <- country_codebook[[nm]][row_cb]
          if (length(v) == 1) {
            j <- match(v, cc)
            if (!is.na(j)) return(j)
          }
        }
      }
    }
  }

  NA_integer_
}

main <- function() {
  domains <- c(
    "investment",
    "security",
    "environment",
    "human_rights",
    "arms_control",
    "intellectual_property"
  )

  flow_paths <- file.path("data/processed", paste0(domains, "_flow_matrix.rds"))
  stock_paths <- file.path("data/processed", paste0(domains, "_stock_matrix.rds"))

  baseline_events <- read_csv_or_stop("data/processed/baseline_events.csv")
  item_codebook <- read_csv_or_stop("data/processed/item_codebook.csv")
  country_codebook <- read_csv_or_stop("data/processed/country_codebook.csv")

  cat(sprintf("Loaded baseline_events: %d rows, %d cols\n", nrow(baseline_events), ncol(baseline_events)))
  cat(sprintf("Loaded item_codebook: %d rows, %d cols\n", nrow(item_codebook), ncol(item_codebook)))
  cat(sprintf("Loaded country_codebook: %d rows, %d cols\n\n", nrow(country_codebook), ncol(country_codebook)))

  summary_rows <- list()

  for (k in seq_along(domains)) {
    domain <- domains[k]
    in_path <- flow_paths[k]
    out_path <- stock_paths[k]

    flow <- read_rds_or_stop(in_path)
    rc_flow <- flow$rc
    if (!is.matrix(rc_flow)) rc_flow <- as.matrix(rc_flow)
    storage.mode(rc_flow) <- "integer"

    N <- nrow(rc_flow)
    J <- ncol(rc_flow)
    T <- as.integer(flow$T)

    flow_plus <- sum(rc_flow == 1L)

    rc_stock <- build_stock_rc(flow, domain)
    stock_plus <- sum(rc_stock == 1L)

    if (!all(dim(rc_stock) == dim(rc_flow))) {
      stop(sprintf("[%s] rc dimension changed (flow %dx%d vs stock %dx%d)", domain, N, J, nrow(rc_stock), ncol(rc_stock)))
    }
    if (!identical(as.integer(flow$T), T)) {
      stop(sprintf("[%s] T changed unexpectedly", domain))
    }

    if (stock_plus < flow_plus) {
      warning(sprintf("[%s] Stock has fewer +1 than flow (flow=%d, stock=%d). This should not happen.", domain, flow_plus, stock_plus))
    }

    ratio <- if (flow_plus == 0L) NA_real_ else (stock_plus / flow_plus)

    stock <- flow
    stock$rc <- rc_stock
    saveRDS(stock, out_path)

    summary_rows[[domain]] <- data.frame(
      domain = domain,
      N = N,
      J = J,
      T = T,
      flow_plus_ones = flow_plus,
      stock_plus_ones = stock_plus,
      ratio_stock_over_flow = ratio,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  cat("Per-domain summary (+1 counts):\n")
  print(summary_df)
  cat("\n")

  # Spot checks for one domain (prefer human_rights).
  spot_domain <- if ("human_rights" %in% domains) "human_rights" else domains[1]
  spot_in <- file.path("data/processed", paste0(spot_domain, "_flow_matrix.rds"))
  spot_out <- file.path("data/processed", paste0(spot_domain, "_stock_matrix.rds"))

  flow_spot <- read_rds_or_stop(spot_in)
  stock_spot <- read_rds_or_stop(spot_out)

  rc_f <- flow_spot$rc
  rc_s <- stock_spot$rc
  if (!is.matrix(rc_f)) rc_f <- as.matrix(rc_f)
  if (!is.matrix(rc_s)) rc_s <- as.matrix(rc_s)
  storage.mode(rc_f) <- "integer"
  storage.mode(rc_s) <- "integer"

  T <- as.integer(flow_spot$T)
  period_labels <- flow_spot$period_labels
  if (is.null(period_labels) || length(period_labels) != T) period_labels <- paste0("t", seq_len(T))

  countries <- c("DNK", "USA", "BRA")
  cat(sprintf("Spot checks (%s): treaty counts per period (flow vs stock)\n", spot_domain))
  for (iso3 in countries) {
    i <- country_row_index(flow_spot, iso3, country_codebook)
    if (is.na(i)) {
      warning(sprintf("Could not find country '%s' in %s country_codes; skipping spot check for it.", iso3, spot_domain))
      next
    }

    flow_counts <- integer(T)
    stock_counts <- integer(T)
    for (t in seq_len(T)) {
      cols <- which(as.integer(flow_spot$bill.session) == (t - 1L))
      flow_counts[t] <- sum(rc_f[i, cols, drop = FALSE] == 1L)
      stock_counts[t] <- sum(rc_s[i, cols, drop = FALSE] == 1L)
    }

    df <- data.frame(
      period = period_labels,
      flow_treaties = flow_counts,
      stock_treaties = stock_counts,
      stringsAsFactors = FALSE
    )
    cat("\n", iso3, ":\n", sep = "")
    print(df)
  }

  cat("\nSaved stock matrices:\n")
  ok <- file.exists(stock_paths)
  for (k in seq_along(stock_paths)) {
    cat(sprintf(" - %s : %s\n", stock_paths[k], if (ok[k]) "OK" else "MISSING"))
  }
  if (!all(ok)) {
    stop("Some stock .rds files were not written successfully.")
  }
}

main()
