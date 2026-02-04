suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  library(dplyr)
})

input_path <- "data/processed/country_year_long.csv"
output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 1990

to_safe <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("&", " and ", x)
  x <- gsub("\\(.*?\\)", " ", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(agreement|treaty|free|trade|fta|rta|economic|partnership|cooperation|association|between|of|and|the|for|on|with|area|protocol)\\b", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

min_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_integer_)
  }
  min(x)
}

if (!file.exists(input_path)) {
  stop("Input file not found: data/processed/country_year_long.csv")
}

x <- read.csv(input_path, stringsAsFactors = FALSE)

trade <- x %>%
  dplyr::filter(issue_area == "trade", source %in% c("DESTA", "WTO RTA"))

other <- x %>%
  dplyr::filter(issue_area != "trade")

# Treaty-level metadata

desta_treaties <- trade %>%
  dplyr::filter(source == "DESTA") %>%
  dplyr::group_by(org_or_treaty_id, org_or_treaty_name) %>%
  dplyr::summarise(
    sig_year = min_or_na(year[event_type == "signature"]),
    any_year = min_or_na(year),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    treaty_name_norm = to_safe(org_or_treaty_name),
    sig_year = ifelse(is.na(sig_year), any_year, sig_year)
  )

wto_treaties <- trade %>%
  dplyr::filter(source == "WTO RTA") %>%
  dplyr::group_by(org_or_treaty_id, org_or_treaty_name) %>%
  dplyr::summarise(
    sig_year = min_or_na(year[event_type == "signature"]),
    any_year = min_or_na(year),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    treaty_name_norm = to_safe(org_or_treaty_name),
    sig_year = ifelse(is.na(sig_year), any_year, sig_year)
  )

# Name matching
if (nrow(desta_treaties) > 0 && nrow(wto_treaties) > 0) {
  desta_names <- desta_treaties$treaty_name_norm
  wto_names <- wto_treaties$treaty_name_norm

  dist_mat <- adist(wto_names, desta_names)
  denom <- pmax(nchar(wto_names), 1)

  best_idx <- apply(dist_mat, 1, which.min)
  best_dist <- dist_mat[cbind(seq_along(best_idx), best_idx)]
  dist_norm <- best_dist / pmax(nchar(wto_names), nchar(desta_names[best_idx]))

  matches <- data.frame(
    wto_id = wto_treaties$org_or_treaty_id,
    wto_name = wto_treaties$org_or_treaty_name,
    wto_sig_year = wto_treaties$sig_year,
    desta_id = desta_treaties$org_or_treaty_id[best_idx],
    desta_name = desta_treaties$org_or_treaty_name[best_idx],
    desta_sig_year = desta_treaties$sig_year[best_idx],
    dist_norm = dist_norm,
    stringsAsFactors = FALSE
  )

  matches$year_diff <- abs(matches$wto_sig_year - matches$desta_sig_year)
  matches$year_diff[is.na(matches$year_diff)] <- Inf

  matches$match_ok <- (matches$dist_norm <= 0.20 & matches$year_diff <= 5) | matches$dist_norm <= 0.10

  candidates <- matches %>% dplyr::filter(match_ok)

  # enforce one-to-one by DESTA id
  final_matches <- candidates %>%
    dplyr::arrange(dist_norm, year_diff) %>%
    dplyr::group_by(desta_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  write.csv(final_matches, file = file.path(output_dir, "trade_dedup_matches.csv"), row.names = FALSE)

  matched_desta_ids <- unique(final_matches$desta_id)
} else {
  final_matches <- data.frame()
  matched_desta_ids <- character(0)
  write.csv(final_matches, file = file.path(output_dir, "trade_dedup_matches.csv"), row.names = FALSE)
}

# Dedupe rule: keep WTO RTA, drop overlapping DESTA treaties
trade_dedup <- trade %>%
  dplyr::filter(
    source == "WTO RTA" |
      (source == "DESTA" & !(org_or_treaty_id %in% matched_desta_ids))
  )

combined <- dplyr::bind_rows(other, trade_dedup) %>%
  dplyr::filter(!is.na(year) & year >= year_min)

write.csv(combined, file = file.path(output_dir, "country_year_long_deduped.csv"), row.names = FALSE)
saveRDS(combined, file = file.path(output_dir, "country_year_long_deduped.rds"))

summary <- data.frame(
  metric = c(
    "desta_treaties",
    "wto_treaties",
    "matched_treaties",
    "dropped_desta_treaties"
  ),
  value = c(
    nrow(desta_treaties),
    nrow(wto_treaties),
    nrow(final_matches),
    length(matched_desta_ids)
  ),
  stringsAsFactors = FALSE
)

write.csv(summary, file = file.path(output_dir, "trade_dedup_summary.csv"), row.names = FALSE)
