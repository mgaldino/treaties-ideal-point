#!/usr/bin/env Rscript
# stan_full_v7.R — USA/CUB anchors, V-Dem as predictor, simplified gamma
# Quick test: 1 chain, 200 warmup + 200 sampling

library(cmdstanr)
library(posterior)
library(dplyr)

set.seed(42)

out_dir <- "outputs/stan_v7"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Stan v7: USA/CUB anchors, V-Dem as predictor ===\n\n")

# --- 1. Load treaty data ---
data_dir <- "data/processed"
domains <- c("investment", "human_rights", "arms_control", "intellectual_property")

flows <- lapply(domains, function(d) {
  f <- readRDS(file.path(data_dir, paste0(d, "_flow_matrix_extended.rds")))
  if (d == "investment") {
    j_keep <- which(colSums(f$rc == 1) >= 3)
    cat(sprintf("  %s: filtered to %d non-bilateral (from %d)\n", d, length(j_keep), ncol(f$rc)))
    f$rc <- f$rc[, j_keep, drop = FALSE]
    f$bill.session <- f$bill.session[j_keep, , drop = FALSE]
    f$item_labels <- f$item_labels[j_keep]
  } else {
    cat(sprintf("  %s: %d items\n", d, ncol(f$rc)))
  }
  f
})
names(flows) <- domains

# --- 2. Shared countries ---
shared_countries <- Reduce(intersect, lapply(flows, function(f) f$country_codes))
cat(sprintf("\nShared countries: %d\n", length(shared_countries)))

anchors <- c("USA", "CUB")
stopifnot(all(anchors %in% shared_countries))
sampled <- sort(shared_countries)

# --- 3. V-Dem ---
vdem <- read.csv(file.path(data_dir, "vdem_liberal_democracy_period_v8.csv"),
                 stringsAsFactors = FALSE)
periods_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009",
                     "2010-2014", "2015-2019", "2020-2024")
T_keep <- 7

# --- 4. Domain predictors ---
nuclear <- read.csv(file.path(data_dir, "nuclear_armed_country_year_1990_2022.csv"),
                    stringsAsFactors = FALSE)
nuclear_ever <- nuclear %>%
  group_by(iso3) %>%
  summarize(ever_nuclear = as.integer(any(nuclear_armed == 1)), .groups = "drop")

tax_haven <- read.csv(file.path(data_dir, "tax_haven_ofc_country_year_1990_2022.csv"),
                      stringsAsFactors = FALSE)
tax_haven_ever <- tax_haven %>%
  group_by(iso3) %>%
  summarize(ever_tax_haven = as.integer(any(tax_haven_or_ofc == 1)), .groups = "drop")

# --- 5. Long-format treaty data ---
all_obs <- list()
for (d_idx in seq_along(domains)) {
  d <- domains[d_idx]
  f <- flows[[d]]
  c_idx <- which(f$country_codes %in% sampled)
  rc_sub <- f$rc[c_idx, , drop = FALSE]
  codes_sub <- f$country_codes[c_idx]
  bs <- as.integer(f$bill.session[, 1])
  j_keep <- which(bs < T_keep)
  if (length(j_keep) == 0) j_keep <- seq_len(ncol(rc_sub))
  rc_sub <- rc_sub[, j_keep, drop = FALSE]
  bs_sub <- bs[j_keep]
  labels_sub <- f$item_labels[j_keep]
  cat(sprintf("  %s: J=%d, N_obs=%d\n", d, ncol(rc_sub), sum(rc_sub != 0)))
  for (i in seq_len(nrow(rc_sub))) {
    for (j in seq_len(ncol(rc_sub))) {
      val <- rc_sub[i, j]
      if (val != 0) {
        all_obs[[length(all_obs) + 1]] <- data.frame(
          country = codes_sub[i], item_label = labels_sub[j],
          domain_idx = d_idx, period_1idx = bs_sub[j] + 1L,
          y = as.integer(val == 1), stringsAsFactors = FALSE)
      }
    }
  }
}
obs_df <- bind_rows(all_obs)
cat(sprintf("\nTotal: N_obs=%d, y=1: %.1f%%\n", nrow(obs_df), 100 * mean(obs_df$y)))

# --- 6. Stan indices ---
country_list <- sort(unique(obs_df$country))
N <- length(country_list)
country_map <- setNames(seq_along(country_list), country_list)
obs_df$ii <- country_map[obs_df$country]

item_list <- unique(obs_df$item_label)
J <- length(item_list)
item_map <- setNames(seq_along(item_list), item_list)
obs_df$jj <- item_map[obs_df$item_label]

D <- length(domains)
obs_df$dd <- obs_df$domain_idx
obs_df$tt <- obs_df$period_1idx

country_periods <- obs_df %>%
  group_by(country) %>%
  summarize(startlegis = min(tt), endlegis = max(tt), .groups = "drop") %>%
  arrange(country)

anchor_pos_idx <- country_map["USA"]
anchor_neg_idx <- country_map["CUB"]
free_idx <- setdiff(seq_len(N), c(anchor_pos_idx, anchor_neg_idx))
N_free <- length(free_idx)

cat(sprintf("Anchors: USA (idx=%d) = +2, CUB (idx=%d) = -2\n", anchor_pos_idx, anchor_neg_idx))

# --- 7. V-Dem matrix (standardized) ---
cat("\nBuilding standardized V-Dem matrix...\n")
free_country_codes <- country_list[free_idx]

vdem_raw <- matrix(0, nrow = N_free, ncol = T_keep)
vdem_obs_mask <- matrix(0, nrow = N_free, ncol = T_keep)

for (k in 1:N_free) {
  iso <- free_country_codes[k]
  for (t in 1:T_keep) {
    match_row <- which(vdem$iso3c == iso & vdem$period == periods_labels[t])
    if (length(match_row) == 1 && !is.na(vdem$vdem_libdem[match_row])) {
      vdem_raw[k, t] <- vdem$vdem_libdem[match_row]
      vdem_obs_mask[k, t] <- 1
    }
  }
}

# Standardize V-Dem (using observed values only)
obs_vals <- vdem_raw[vdem_obs_mask == 1]
vdem_mean <- mean(obs_vals)
vdem_sd <- sd(obs_vals)
cat(sprintf("V-Dem raw: mean=%.3f, sd=%.3f\n", vdem_mean, vdem_sd))

vdem_std <- (vdem_raw - vdem_mean) / vdem_sd
# Set unobserved to 0 (won't affect LL due to mask)
vdem_std[vdem_obs_mask == 0] <- 0

n_vdem_obs <- sum(vdem_obs_mask)
cat(sprintf("V-Dem coverage: %d/%d (%.1f%%)\n", n_vdem_obs, N_free * T_keep,
    100 * n_vdem_obs / (N_free * T_keep)))

# --- 8. Z_domain matrix ---
Z_domain <- matrix(0, nrow = N, ncol = D)

# d=1: investment -> tax_haven
for (i in 1:N) {
  m <- which(tax_haven_ever$iso3 == country_list[i])
  if (length(m) == 1) Z_domain[i, 1] <- tax_haven_ever$ever_tax_haven[m]
}
cat(sprintf("  investment: %d/%d tax havens\n", sum(Z_domain[, 1]), N))

# d=2: HR -> no predictor (Z=0)
# d=3: arms_control -> nuclear
for (i in 1:N) {
  m <- which(nuclear_ever$iso3 == country_list[i])
  if (length(m) == 1) Z_domain[i, 3] <- nuclear_ever$ever_nuclear[m]
}
cat(sprintf("  arms_control: %d/%d nuclear\n", sum(Z_domain[, 3]), N))
# d=4: IP -> no predictor (Z=0)

# --- 9. Stan data ---
OMEGA_FIXED <- 0.25
SIGMA_REG <- 1.0

stan_data <- list(
  N = N, N_free = N_free, J = J,
  T_periods = T_keep, D = D, N_obs = nrow(obs_df),
  ii = obs_df$ii, jj = obs_df$jj, dd = obs_df$dd, tt = obs_df$tt,
  y = obs_df$y,
  startlegis = country_periods$startlegis,
  endlegis = country_periods$endlegis,
  anchor_pos_idx = anchor_pos_idx, anchor_neg_idx = anchor_neg_idx,
  anchor_pos_val = 2.0, anchor_neg_val = -2.0,
  free_idx = free_idx,
  vdem_std_free = vdem_std,
  vdem_obs = vdem_obs_mask,
  Z_domain = Z_domain,
  omega_fixed = OMEGA_FIXED,
  sigma_reg = SIGMA_REG
)

cat(sprintf("\nStan: N=%d, J=%d, T=%d, D=%d, N_obs=%d, N_free=%d\n",
    N, J, T_keep, D, nrow(obs_df), N_free))
cat(sprintf("Fixed: omega=%.2f, sigma_reg=%.1f\n", OMEGA_FIXED, SIGMA_REG))

saveRDS(list(stan_data = stan_data, obs_df = obs_df, country_list = country_list,
             domains = domains, country_map = country_map,
             free_idx = free_idx, free_country_codes = free_country_codes,
             vdem_std = vdem_std, vdem_obs_mask = vdem_obs_mask,
             vdem_mean = vdem_mean, vdem_sd = vdem_sd,
             Z_domain = Z_domain, periods_labels = periods_labels),
        file.path(out_dir, "stan_data.rds"))

# --- 10. Compile ---
cat("\nCompiling Stan v7 model...\n")
model <- cmdstan_model("stan/hierarchical_joint_irt_v7.stan")
cat("Compilation successful.\n")

# --- 11. MCMC: 1500w + 2000s, 4 chains ---
cat("\n=== MCMC: 1500 warmup + 2000 sampling, 4 chains ===\n")
t0 <- Sys.time()
fit <- model$sample(
  data = stan_data,
  seed = 42,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1500,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 200
)
t_mcmc <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("\nMCMC: %.1fs (%.1f min)\n", t_mcmc, t_mcmc / 60))

fit$save_object(file.path(out_dir, "fit_v7.rds"))

# --- 12. Diagnostics ---
cat("\n=== Diagnostics ===\n")
diag <- fit$diagnostic_summary()
n_div <- sum(diag$num_divergent)
n_td <- sum(diag$num_max_treedepth)
summ <- fit$summary()
rhat_vals <- summ$rhat
ess_vals <- summ$ess_bulk

cat(sprintf("Divergences: %d\n", n_div))
cat(sprintf("Max treedepth hits: %d\n", n_td))
cat(sprintf("Max Rhat: %.4f\n", max(rhat_vals, na.rm = TRUE)))
cat(sprintf("Pct Rhat>1.01: %.1f%%\n", 100 * mean(rhat_vals > 1.01, na.rm = TRUE)))
cat(sprintf("ESS_bulk: min=%.0f, median=%.0f\n",
    min(ess_vals, na.rm = TRUE), median(ess_vals, na.rm = TRUE)))

worst <- summ %>% filter(!is.na(rhat)) %>% arrange(desc(rhat)) %>% head(10)
cat("\nWorst Rhat:\n")
print(worst[, c("variable", "mean", "sd", "rhat", "ess_bulk")])

draws <- fit$draws(format = "draws_matrix")

cat(sprintf("\nb_v: %.4f (%.4f)\n", mean(draws[, "b_v"]), sd(draws[, "b_v"])))

cat("\n--- sigma_d ---\n")
for (d in 1:D) {
  cat(sprintf("  %s: %.4f (%.4f)\n", domains[d],
      mean(draws[, paste0("sigma_d[", d, "]")]),
      sd(draws[, paste0("sigma_d[", d, "]")])))
}

cat("\n--- delta ---\n")
for (d in 1:D) {
  cat(sprintf("  %s: %.4f (%.4f)\n", domains[d],
      mean(draws[, paste0("delta[", d, "]")]),
      sd(draws[, paste0("delta[", d, "]")])))
}

# Country rankings (mu_mean)
mu_cols <- paste0("mu_mean[", 1:N, "]")
mu_mean_post <- colMeans(draws[, mu_cols])
names(mu_mean_post) <- country_list

cat("\nTop 15 mu_mean:\n")
top <- sort(mu_mean_post, decreasing = TRUE)[1:15]
for (i in seq_along(top)) cat(sprintf("  %2d. %s: %.3f\n", i, names(top)[i], top[i]))
cat("\nBottom 15 mu_mean:\n")
bot <- sort(mu_mean_post)[1:15]
for (i in seq_along(bot)) cat(sprintf("  %2d. %s: %.3f\n", i, names(bot)[i], bot[i]))

# V7 correlations
cat("\n=== V7 Correlations ===\n")
v7_dir <- "outputs/v7_country_anchors"
for (d_idx in seq_along(domains)) {
  d <- domains[d_idx]
  v7_path <- file.path(v7_dir, paste0(d, "_results.rds"))
  if (!file.exists(v7_path)) next
  v7 <- readRDS(v7_path)
  v7_dim1 <- v7$ideal_points_mean[, 1]
  names(v7_dim1) <- v7$country_codes
  theta_cols <- paste0("theta_mean[", 1:N, ",", d_idx, "]")
  theta_post <- colMeans(draws[, theta_cols])
  names(theta_post) <- country_list
  shared <- intersect(names(v7_dim1), names(theta_post))
  r_theta <- cor(v7_dim1[shared], theta_post[shared])
  cat(sprintf("  %s: r(theta,V7)=%.3f (n=%d)\n", d, r_theta, length(shared)))
}

# UNGA
cat("\n=== UNGA Validation ===\n")
unga_path <- file.path(data_dir, "unga_ideal_points_period_v8.csv")
if (file.exists(unga_path)) {
  unga <- read.csv(unga_path, stringsAsFactors = FALSE)
  unga_mean <- unga %>%
    group_by(iso3) %>%
    summarize(unga_mean = mean(unga_ideal_point, na.rm = TRUE), .groups = "drop")
  shared_unga <- intersect(unga_mean$iso3, country_list)
  if (length(shared_unga) >= 10) {
    unga_vals <- unga_mean$unga_mean[match(shared_unga, unga_mean$iso3)]
    mu_vals <- mu_mean_post[shared_unga]
    cat(sprintf("  r(mu_mean, UNGA) = %.3f (n=%d)\n",
        cor(unga_vals, mu_vals, use = "complete.obs"), length(shared_unga)))
  }
}

# --- Convergence verdict ---
converged <- n_div == 0 && max(rhat_vals, na.rm = TRUE) < 1.01
cat(sprintf("\n=== CONVERGED: %s ===\n", ifelse(converged, "YES", "NO")))

# Save report
sink(file.path(out_dir, "v7_report.txt"))
cat("=== V7 Report: USA/CUB anchors, V-Dem as predictor ===\n")
cat(sprintf("Date: %s\n", Sys.time()))
cat(sprintf("Config: 1500w+2000s, 4 chains, omega_fixed=%.2f, sigma_reg=%.1f\n", OMEGA_FIXED, SIGMA_REG))
cat(sprintf("Anchors: USA(+2), CUB(-2)\n"))
cat(sprintf("Dims: N=%d, J=%d, T=%d, D=%d, N_obs=%d\n", N, J, T_keep, D, nrow(obs_df)))
cat(sprintf("MCMC: %.1fs (%.1f min)\n", t_mcmc, t_mcmc / 60))
cat(sprintf("Divergences: %d, Max Rhat: %.4f, Pct>1.01: %.1f%%\n",
    n_div, max(rhat_vals, na.rm=TRUE), 100*mean(rhat_vals>1.01, na.rm=TRUE)))
cat(sprintf("Min ESS: %.0f, Median ESS: %.0f\n", min(ess_vals, na.rm=TRUE), median(ess_vals, na.rm=TRUE)))
cat(sprintf("b_v: %.4f (%.4f)\n", mean(draws[,"b_v"]), sd(draws[,"b_v"])))
for (d in 1:D) {
  cat(sprintf("sigma_d[%s]: %.4f, delta[%s]: %.4f\n",
      domains[d], mean(draws[,paste0("sigma_d[",d,"]")]),
      domains[d], mean(draws[,paste0("delta[",d,"]")])))
}
cat(sprintf("CONVERGED: %s\n", ifelse(converged, "YES", "NO")))
sink()

cat(sprintf("\nReport saved to %s/v7_report.txt\n", out_dir))
cat("\n=== DONE ===\n")
