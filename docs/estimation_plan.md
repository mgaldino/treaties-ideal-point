# Estimation Plan: Dynamic Ideal Points from Treaty Participation (2D Main Results)

**Date**: 2026-02-06
**Status**: Revised for 2D main results and 7 domains
**Language**: R (data analysis and estimation), Python (data acquisition only)
**Key packages**: `dynIRT_KD` (primary 2D EM), `emIRT` (K=1 diagnostic only)

**Update summary (2026-02-06)**:
- Main results are **2D** (K = 2) using `dynIRT_KD`; 1D is diagnostic only.
- The project now includes **7 domains** (trade, investment, security, environment, human_rights, arms_control, intellectual_property).
- The **trade domain is paused** in treaty form; the paper requires a **continuous-response IRT** using tariff data (WITS/TRAINS) to measure trade/ILO support.
- Two estimation approaches must be run before choosing the final specification: **joint 2D** (all domains pooled) and **per-domain 2D**.

---

## General Rule: Troubleshooting Protocol

**IMPORTANT — read this before executing any phase.**

When any step fails (script error, download failure, package installation error, etc.), do NOT simply retry the same command. Follow this protocol:

### Step 1: Diagnose the problem
Run diagnostic commands **before** retrying. Choose based on the error type:

| Error type | Diagnostic commands to run | What to look for |
|------------|---------------------------|------------------|
| DNS / network (`Could not resolve host`, `nodename nor servname`) | `ping -c 2 en.wikipedia.org` and `nslookup en.wikipedia.org` | If ping succeeds → transient failure, retry. If ping fails → no network access from this environment. |
| HTTP error (403, 404, 500) | `curl -I <URL>` (headers only) | 403 = blocked/auth required. 404 = wrong URL. 500 = server issue (retry later). |
| Timeout | `curl -m 10 <URL>` | If timeout persists, the host may be unreachable from this environment. |
| Python import error | `pip list \| grep <package>` | Package may not be installed. Install it. |
| R package error | `Rscript -e 'packageVersion("pkg")'` | Package may not be installed or may be the wrong version. |

### Step 2: Check sandbox/permission constraints
If the diagnostic commands themselves fail or the network is unreachable, the problem may be that your execution environment (sandbox) restricts outbound network access. Run these checks **in order**, stopping as soon as one succeeds:

```bash
# Level 1: Can I resolve DNS at all?
nslookup google.com

# Level 2: Can I reach any external host?
ping -c 2 -W 3 google.com

# Level 3: Can I make HTTP requests?
curl -s -o /dev/null -w "%{http_code}" https://www.google.com

# Level 4: Can I make HTTP requests from Python?
python3 -c "import urllib.request; print(urllib.request.urlopen('https://www.google.com').status)"
```

**Interpret the results and escalate to the user:**

| Level 1 (DNS) | Level 2 (ping) | Level 3 (curl) | Level 4 (Python) | Diagnosis | What to ask the user |
|:-:|:-:|:-:|:-:|---|---|
| FAIL | — | — | — | Sandbox blocks all outbound network | "My environment has no network access. Can you either (a) grant network permission, (b) download the file yourself and place it in `data/raw/wto_accession/`, or (c) paste the data directly to me?" |
| OK | FAIL | — | — | DNS works but ICMP blocked | Not necessarily a problem — continue to Level 3 |
| OK | — | FAIL | — | Sandbox blocks HTTP from curl | Try Level 4; the block may be tool-specific |
| OK | — | OK | FAIL | Python specifically is blocked | Use `curl` or `wget` to download instead of Python. Rewrite the acquisition step as a shell command. |
| OK | — | OK | OK | Network works fine | The problem is in the script, not permissions. Debug the script logic. |

**CRITICAL**: If you reach the conclusion that your sandbox has no network access, do NOT keep retrying. Immediately ask the user for help using the diagnosis table above. Report exactly which levels passed and which failed.

### Step 3: Act on the diagnosis
- **If the network works** (ping succeeds, curl succeeds): retry the original command once. If it fails again, the problem is in the script, not the network.
- **If the network does NOT work**: do NOT retry. Follow the escalation table in Step 2 — ask the user.
- **If a specific host is blocked** (google.com works but the target host doesn't): try an alternative source URL (listed in per-phase fallback sections).
- **If Python is blocked but curl works**: rewrite the download as a `curl` command instead of a Python script. Example: `curl -L -o data/raw/wto_accession/wto_members.html "https://en.wikipedia.org/wiki/Member_states_of_the_World_Trade_Organization"`.

### Step 4: If all else fails
- Log the exact error message and ALL diagnostic results (Levels 1–4) to `logs/`.
- Report to the user: (1) what was tried, (2) which diagnostic levels passed/failed, (3) what concrete options remain (e.g., "please place the file at X path" or "please grant network permission").
- Do NOT loop on the same failing command more than twice.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Decisions and Constraints](#2-decisions-and-constraints)
3. [Phase 0 — Acquire WTO Accession Data](#3-phase-0--acquire-wto-accession-data)
4. [Phase 1 — Data Preparation](#4-phase-1--data-preparation)
5. [Phase 2 — Exploratory Analysis](#5-phase-2--exploratory-analysis)
6. [Phase 3 — Estimation with dynIRT_KD](#6-phase-3--estimation-with-dynirt_kd-k--2)
7. [Phase 4 — Validation](#7-phase-4--validation)
8. [Phase 5 (Future) — Multidimensional Stan Model](#8-phase-5-future--multidimensional-stan-model)
9. [Appendix A — Robustness Checks](#9-appendix-a--robustness-checks)
10. [Appendix B — Key References](#10-appendix-b--key-references)

---

## 1. Overview

**Goal**: Estimate dynamic **2D** ideal points for countries (K = 2), using treaty participation data across **7 domains**. The latent space is interpreted as *systemic commitment to the International Liberal Order (ILO)*. The 1D model is used only as a diagnostic (K = 1 equivalence).

**Data sources** (7 domains + trade tariffs, all under `data/raw/`):

| # | Source | Issue area | Raw location |
|---|--------|-----------|--------------|
| 1 | DESTA v2.3 + WTO RTA (merged, deduplicated) | Trade treaties (paused for main results) | `data/raw/desta/`, `data/raw/wto_rta/` |
| 2 | UNCTAD IIA Navigator | Investment | `data/raw/unctad_iia/` |
| 3 | ATOP 5.1 | Security | `data/raw/atop/` |
| 4 | IEADB | Environment | `data/raw/ieadb/` |
| 5 | UN HR treaties (Chapter IV) | Human rights | `data/raw/un_hr_treaties/` |
| 6 | Arms control treaties (Chapter XXVI) | Arms control | `data/raw/arms_control/` |
| 7 | WIPO treaties | Intellectual property | `data/raw/wipo_treaties/` |
| 8 | WTO accession dates | Trade anchor item | `data/raw/wto_accession/` |
| 9 | WITS/UNCTAD TRAINS tariffs | Trade (continuous response IRT) | `data/raw/tariffs/` |

**Method**: `dynIRT_KD` — EM with data augmentation + Kalman filter–smoother for K-dimensional dynamic IRT. Two specifications must be run before final choice: **joint 2D** (all domains pooled) and **per-domain 2D**. `emIRT::dynIRT()` is used only as a diagnostic baseline (K = 1).

**Time structure**: 6 five-year periods: 1990–1994, 1995–1999, 2000–2004, 2005–2009, 2010–2014, 2015–2018.

---

## 2. Decisions and Constraints

These decisions were agreed upon in the Q&A process. Do not change them without explicit approval.

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Vote coding | Flow/transition (newly ratified in period) | Better discrimination than cumulative |
| Time periods | 5-year windows (6 periods) | Treaties are infrequent; annual is too sparse |
| Common window | 1990–2018 (truncated to shortest source) | ATOP ends 2018, DESTA ends 2019 |
| Domains | 7 domains (trade, investment, security, environment, human_rights, arms_control, intellectual_property) | Full coverage of ILO institutional architecture |
| Models | K = 2 main; run joint 2D and per-domain 2D; 1D diagnostic only | 2D required for paper and cross-domain structure |
| Trade dimension | Treaty-based trade paused; main trade signal via continuous-response IRT on tariffs | Tariff policy is central to ILO support |
| Baseline event type | Entry-into-force (EIF) only | Signature/ratification used only in robustness checks |
| Identification | Country anchors from PCA + theory; item anchors via **sign constraints** on beta | Identification + substantive direction without tight priors |
| Anchor items | WTO accession, Paris, ICSID, NATO, ICCPR, NPT, TRIPS (sign constraints) | Clear ILO direction per domain |
| Anchor countries | Denmark/Iran/China (PCA-checked) + alternatives for sensitivity | Non-collinearity in 2D and robustness |
| REIO/territories | Exclude from estimation; keep in separate files | Preserve for robustness or later re-inclusion |
| Estimates | Point estimates (SEs via bootstrap if needed) | dynIRT_KD variance estimates are unreliable |
| Language | R for all analysis and estimation | Per RULES.md |

---

## 3. Phase 0 — Acquire WTO Accession Data

### Objective
Add WTO membership accession dates as a standalone item in the trade issue area.

**Note (2026-02-06)**: The treaty-based trade dimension is paused for main results. WTO accession remains useful for anchors and robustness, but the paper requires a **continuous-response IRT** trade dimension built from tariff data (WITS/TRAINS).

### Why this matters
WTO accession is one of the strongest signals of commitment to the liberal trade order. It is not captured by DESTA or WTO RTA (those cover trade agreements *between* countries, not accession to the WTO itself). It will serve as an **anchor item** for scale identification in the trade model.

### Steps

**Step 0.1**: Obtain WTO membership data.

- **Primary source**: WTO official accession page:
  https://www.wto.org/english/thewto_e/acc_e/acc_status_e.htm
- **Practical source** (easier to extract): Wikipedia table of WTO members:
  https://en.wikipedia.org/wiki/Member_states_of_the_World_Trade_Organization
- The Wikipedia table has columns: Country, Date of accession, and notes.
- As of early 2026 there are 166 WTO members.
- Original GATT/WTO members (1 January 1995) should be coded with accession date 1995-01-01.
- Countries that were GATT contracting parties before 1995 but converted to WTO membership on 1 January 1995 should also be coded as 1995-01-01.

**Step 0.2**: Create a Python acquisition script (`scripts/python/acquire_wto_accession.py`).

- Use `pandas.read_html()` to extract the Wikipedia table, or manually create a CSV if the table is small enough (166 rows).
- Output columns: `country_name`, `country_code_iso3`, `accession_date`, `is_original_member` (boolean: TRUE if accession_date is 1995-01-01).
- Save to `data/raw/wto_accession/wto_members.csv`.
- Log access date in `data/raw/wto_accession/access_log.json`.

**Step 0.3**: Validate.

- Check: exactly 166 members (or current count per WTO site).
- Check: all accession dates are between 1948-01-01 (earliest GATT) and today.
- Check: no duplicate country codes.
- Check: ISO3 codes are valid (cross-check against a reference like the `countrycode` R package).

### Expected output
- `data/raw/wto_accession/wto_members.csv` (~166 rows × 4 columns)

### Tips and warnings
- Wikipedia sometimes has footnotes embedded in cells (e.g., "[1]"). Strip these during extraction.
- Some territories/customs unions are WTO members but not sovereign states (e.g., European Union, Hong Kong, Macao, Chinese Taipei). Include them in raw data, flag with `is_sovereign = FALSE`, and exclude from estimation by default (keep in separate files for robustness).
- The WTO Accession Commitments Database (https://acdb.wto.org/) has richer data but is harder to extract. Not needed for this phase.

### Fallback for network failures
If the primary source (Wikipedia) fails due to DNS, timeout, or HTTP errors:
1. **Retry after 30 seconds** — transient DNS failures are common and often resolve quickly.
2. **Try an alternative source** — the WTO official page has the same data: https://www.wto.org/english/thewto_e/acc_e/acc_status_e.htm
3. **Last resort: create the CSV manually** — there are only ~166 rows. Open the WTO page in a browser, copy the membership table into a spreadsheet, and save as CSV. This is a legitimate approach for a small, stable dataset that rarely changes.

---

## 4. Phase 1 — Data Preparation

### Objective
Build 7 country × item × period flow matrices (one per issue area) ready for `dynIRT_KD()` (K = 2). The 1D `emIRT::dynIRT()` model is diagnostic only.

### Prerequisites
- All raw data in `data/raw/` (already acquired).
- WTO accession data from Phase 0.
- DESTA + WTO RTA merged and deduplicated (handled by separate agent; assume result is available as a single file).

### Script
`scripts/R/01_prepare_data.R`

---

### Step 1.1: Harmonize country codes to ISO3

Use the `countrycode` R package to convert all country identifiers to ISO 3166-1 alpha-3.

```r
library(countrycode)
# Example:
df$iso3 <- countrycode(df$country_name, origin = "country.name", destination = "iso3c")
```

**Things to watch for:**

| Problem | Solution |
|---------|----------|
| `countrycode()` returns NA for some names | Inspect NAs manually. Common culprits: "Korea, Republic of" vs. "South Korea", "Côte d'Ivoire" vs. "Ivory Coast", "Lao PDR", "Eswatini" vs. "Swaziland". Fix with a manual lookup table before calling `countrycode()`. |
| Historical countries (USSR, Yugoslavia, Czechoslovakia, Serbia and Montenegro) | Map to successor states with a clear rule. Recommendation: drop pre-dissolution observations and keep only post-dissolution successor states from their independence date onward. Document the rule. |
| Territories that aren't ISO countries (Hong Kong, Macao, Taiwan, Palestine) | Assign custom codes (HKG, MAC, TWN, PSE). These have de facto ISO3 codes. Include them but flag `is_sovereign = FALSE`. Exclude `is_sovereign = FALSE` from estimation; keep a separate file for robustness. |
| Different sources use different coding (COW codes in ATOP, custom IDs in IEADB) | Convert each source separately before merging. ATOP uses COW numeric codes — use `countrycode(df$cowcode, origin = "cown", destination = "iso3c")`. |

**Validation check**: After harmonization, print the unique ISO3 codes per source and compare. Every source should have largely overlapping country sets. Investigate any country present in one source but not others.

---

### Step 1.2: Define time periods

Create a period lookup:

```r
period_breaks <- c(1990, 1995, 2000, 2005, 2010, 2015, 2019)  # right boundary is exclusive
period_labels <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018")

# For any date or year:
assign_period <- function(year) {
  cut(year, breaks = period_breaks, labels = period_labels,
      right = FALSE, include.lowest = TRUE)
}
```

**Warning**: The last period is 2015–2018 (4 years, not 5), because the common window ends in 2018. This is fine but document it.

---

### Step 1.3: Assign issue-area labels

| Issue area | Sources |
|-----------|---------|
| Trade | DESTA + WTO RTA (merged) + WTO accession |
| Investment | UNCTAD IIA |
| Security | ATOP |
| Environment | IEADB |

Each treaty/agreement gets a unique `item_id` (string like `"desta_123"`, `"unctad_iia_456"`, `"atop_789"`, `"ieadb_012"`, `"wto_accession"`). Prefix with source to avoid ID collisions across sources.

---

### Step 1.4: Extract baseline entry-into-force (EIF) dates per country-treaty pair

Baseline estimation uses **EIF only**. Signature and ratification dates are reserved for robustness checks. For sources without an explicit state-level EIF field, use the closest legal entry date (ratification/acceptance/accession) as a documented proxy.

For each source, extract the date (or year) when each country became party to each treaty/agreement. The relevant baseline date fields differ by source:

| Source | Date field to use | Notes |
|--------|------------------|-------|
| DESTA | `entry_into_force` | Use the dyadic treaty list (`desta_list_of_treaties_02_03_dyads.csv`). Each row is a country-treaty pair. Signature dates are for robustness only. |
| WTO RTA | Entry-into-force date | From `WTO_RTA_AllRTAs.xlsx`. |
| UNCTAD IIA | Entry-into-force date | Signature dates are for robustness only. |
| ATOP | `begyr` (beginning year of alliance for each member) | From the alliance-member-level dataset. |
| IEADB | Earliest of `Ratification` / `AcceptanceApproval` / `Accession` | Use as a proxy for state-level EIF when explicit EIF is not available. |
| WTO accession | `accession_date` | From Phase 0 output. |

**Output of this step**: A long-format data frame:

```
country_iso3 | item_id | issue_area | event_year | event_period
BRA          | desta_42| trade      | 2003              | 2000-2004
DEU          | ieadb_7 | environment| 1997              | 1995-1999
...
```

**Validation checks**:
- No event year before 1945 (post-WWII era for all sources).
- No event year after 2018 (common window).
- No duplicate country-treaty pairs within the same source.
- If a country appears as party to the same treaty in both DESTA and WTO RTA (after merge/dedup), that should already be resolved. Verify no duplicates remain.

---

### Step 1.5: Determine treaty availability periods

For each treaty/agreement, determine the **first period** it was open for participation using the treaty-level open/adoption date (e.g., earliest signature/adoption across all parties), not the country-specific accession date. This is the period corresponding to:
- The signature/adoption date (for treaties)
- The formation date (for alliances in ATOP)
- The agreement date (for trade agreements)

```
item_id | issue_area | first_available_period | treaty_open_year
desta_42| trade      | 2000-2004              | 2001
```

**Why this matters**: A country can only "vote" on a treaty starting from the period it becomes available. Before that, the entry in the vote matrix is 0 (missing) in `dynIRT_KD()` coding.

---

### Step 1.6: Construct the flow matrix

This is the most critical step. For each issue area, build an N × J matrix where:
- N = number of countries
- J = number of **phantom items** (one per treaty-period combination)

**The phantom item concept**: Each treaty *j* that exists in period *t* generates a phantom item *(j, t)*. This captures *when* countries join, not just *whether* they join.

**Coding rules for `rc[i, (j,t)]`**:

| Situation | Code | Meaning |
|-----------|------|---------|
| Country *i* newly ratified treaty *j* in period *t* | **1** | "Yea" — joined the liberal order institution in this period |
| Treaty *j* exists in period *t*, country *i* has NOT ratified *j* by end of *t*, and *i* did not ratify before *t* | **-1** | "Nay" — chose not to join (yet) |
| Country *i* already ratified treaty *j* before period *t* | **0** | Missing — no longer "at risk", already joined |
| Treaty *j* does not exist yet in period *t* (treaty opened after *t*) | **0** | Missing — item not available |
| Country *i* does not exist in period *t* (e.g., South Sudan before 2011) | **0** | Missing — legislator not serving |

**Implementation in R** (pseudocode):

```r
# For one issue area:
# Start with the long-format data from Step 1.4

# 1. Create all treaty-period phantom items
phantom_items <- expand.grid(
  item_id = unique(treaties$item_id),
  period = period_labels
) |>
  inner_join(treaty_availability, by = "item_id") |>
  filter(period >= first_available_period)

# 2. For each phantom item (j, t), determine each country's "vote"
# Start with all combinations: countries × phantom items
all_combos <- expand.grid(
  country_iso3 = unique(countries$iso3),
  item_id = unique(phantom_items$item_id),
  period = unique(phantom_items$period)
)

# 3. Merge with ratification data and apply coding rules
# ... (see detailed logic above)

# 4. Reshape to N × J matrix
rc_matrix <- all_combos |>
  pivot_wider(id_cols = country_iso3,
              names_from = phantom_item_id,
              values_from = vote_code)
```

**Critical validation checks after building the matrix**:

1. **Sparsity report**: Print the fraction of 0s (missing), 1s (yea), and -1s (nay) per issue area. Expected: mostly 0s and -1s, with 1s being the minority (ratification events are less common than non-events).
2. **No country should have ALL zeros** (entirely missing). If so, that country has no treaty activity in this issue area and should be dropped from this model.
3. **No phantom item should have ALL zeros** (no country voted on it). Drop these items.
4. **No phantom item should have ALL 1s or ALL -1s** (zero variation). Drop these items — they provide no discrimination.
5. **Row sums** (total 1s per country): countries with more 1s are more engaged. Print the distribution. Outliers (very high or very low) deserve inspection.
6. **Column sums** (total 1s per item): items with more 1s are more popular treaties. Print the distribution.

---

### Step 1.7: Build additional `dynIRT_KD()` input vectors

Beyond the `rc` matrix, `dynIRT_KD()` needs:

```r
# N × 1: first period each country is "active" (0-indexed!)
startlegis <- matrix(0L, nrow = N, ncol = 1)
# For countries that gained independence after 1990, set to the period index.
# E.g., South Sudan (2011) → period index 4 (2010-2014, 0-indexed)

# N × 1: last period each country is "active" (0-indexed!)
endlegis <- matrix(5L, nrow = N, ncol = 1)  # all end at period 5 (2015-2018)
# Adjust for countries that ceased to exist (rare in 1990-2018)

# J × 1: period index for each phantom item (0-indexed!)
bill.session <- matrix(period_index_for_each_phantom_item, ncol = 1)

# Integer: total number of periods
T_periods <- 6L
```

**CRITICAL WARNING**: `dynIRT_KD()` uses **0-indexed** periods. Period 1990–1994 = 0, 1995–1999 = 1, ..., 2015–2018 = 5. This is a common source of bugs. Double-check all indexing.

---

### Expected outputs from Phase 1

| File | Contents | Location |
|------|----------|----------|
| `data/processed/trade_flow_matrix.rds` | List with `rc`, `startlegis`, `endlegis`, `bill.session`, `T`, plus metadata (country names, item labels) | `data/processed/` |
| `data/processed/investment_flow_matrix.rds` | Same structure | `data/processed/` |
| `data/processed/security_flow_matrix.rds` | Same structure | `data/processed/` |
| `data/processed/environment_flow_matrix.rds` | Same structure | `data/processed/` |
| `data/processed/country_codebook.csv` | ISO3, country name, independence year, sovereignty flag | `data/processed/` |
| `data/processed/item_codebook.csv` | item_id, phantom_item_id, treaty name, issue area, first available period, source | `data/processed/` |

Save as `.rds` (R native) to preserve matrix structure and attributes.

---

## 5. Phase 2 — Exploratory Analysis

### Objective
Verify the data is suitable for IRT estimation, check dimensionality, and identify tentative anchor countries/items.

### Script
`scripts/R/02_eda.R`

### Step 2.1: Summary statistics per issue area

For each issue area, report:

| Statistic | What to report |
|-----------|---------------|
| N (countries) | After dropping all-zero rows |
| J (phantom items) | After dropping zero-variation columns |
| T (periods) | Should be 6 |
| Sparsity | % of matrix that is 0 (missing) |
| % yea (1) | Among non-missing entries |
| % nay (-1) | Among non-missing entries |
| Mean items per country per period | Rough guide to information density |

**Red flags**:
- If sparsity > 95%: the matrix is too sparse for reliable estimation. Consider collapsing to fewer periods (3 instead of 6) or merging issue areas.
- If % yea < 5% or > 95% among non-missing: extreme imbalance. The model may struggle. Consider dropping near-unanimous items.
- If N < 50: too few countries for a meaningful ideal-point model in that issue area.

### Step 2.2: PCA on the participation matrix

For each issue area:

```r
# Replace 0 (missing) with NA, keep only 1 and -1
rc_for_pca <- rc_matrix
rc_for_pca[rc_for_pca == 0] <- NA

# Recode: 1 stays 1, -1 becomes 0 (for PCA purposes)
rc_for_pca[rc_for_pca == -1] <- 0

# Impute NAs with column means (simple imputation for PCA only)
for (j in 1:ncol(rc_for_pca)) {
  col_mean <- mean(rc_for_pca[, j], na.rm = TRUE)
  rc_for_pca[is.na(rc_for_pca[, j]), j] <- col_mean
}

# Run PCA
pca_result <- prcomp(rc_for_pca, center = TRUE, scale. = TRUE)
```

**What to examine**:

1. **Scree plot**: Plot eigenvalues (proportion of variance explained). Save as `outputs/eda/fig_scree_{issue_area}.png`. If the first eigenvalue dominates (e.g., explains >30% of variance and the second explains <15%), a 1D model is defensible. If the first two are close, consider 2D (future Phase 5).

2. **PC1 loadings by country**: Which countries load most positively and negatively on the first principal component? These are your tentative anchor countries. Compare with the prior expectation (Denmark positive, Iran negative). Save ranked loadings as `outputs/eda/pca_loadings_{issue_area}.csv`.

3. **PC1 loadings by item**: Which treaties/agreements load most strongly? This helps identify anchor items. The WTO accession item (trade) and Paris Agreement (environment) should load positively if the first dimension captures ILO commitment.

**Tip**: PCA is just exploratory here — it does NOT replace the IRT model. It's a quick sanity check on dimensionality and polarity.

### Step 2.3: Identify anchor countries and items

Based on PCA results:

1. **Confirm Denmark (or Netherlands/Norway)** is near the positive extreme of PC1 across most issue areas.
2. **Confirm Iran (or North Korea/Myanmar)** is near the negative extreme.
3. **Confirm WTO accession** has a positive loading on PC1 in the trade model.
4. **Confirm Paris Agreement** has a positive loading on PC1 in the environment model.
5. If any of these don't hold, investigate. The scale might be flipped (PCA eigenvectors are sign-indeterminate) — flip the sign of PC1 and re-check.

**Output**: A table `outputs/eda/anchor_candidates.csv`:

```
issue_area  | anchor_country_pos | anchor_country_neg | anchor_item        | notes
trade       | DNK                | IRN                | wto_accession      | confirmed by PCA
investment  | DNK                | IRN                | ...                | ...
security    | DNK                | IRN                | ...                | ...
environment | DNK                | IRN                | ieadb_paris_agree  | confirmed by PCA
```

**Fallback anchoring strategy (if the original anchors fail or lead to non-identification)**:
- Recompute anchor countries **per issue area** using PCA extremes (top/bottom 1–2 countries on PC1).
- Select 1–2 anchor items per issue area using the strongest positive PC1 loadings.
- Re-run estimation with these alternative anchors and document the rationale and effects on scale orientation.

### Step 2.4: Participation time-series plots

For a selection of ~10 substantively important countries (US, China, Russia, Brazil, India, Germany, Denmark, Iran, South Africa, Japan), plot the number of new ratifications per period per issue area. Save as `outputs/eda/fig_ratification_timeseries_{issue_area}.png`.

**Purpose**: visual sanity check. Does the pattern make substantive sense? E.g., China's trade activity should increase after WTO accession (2001); Iran's should be consistently low.

---

## 6. Phase 3 — Estimation with dynIRT_KD (K = 2)

### Objective
Estimate dynamic **2D** ideal points using `dynIRT_KD()` (main results). Run both **joint 2D** and **per-domain 2D** specifications. Use `emIRT::dynIRT()` only for K = 1 diagnostic checks.

### Script
`scripts/R/03_estimate_ideal_points.R` (must be updated to call `dynIRT_KD()` for K = 2)

### Step 3.0: Load dynIRT_KD

`dynIRT_KD` is implemented locally under `scripts/R/`. Source it before estimation. Install `emIRT` **only** if you need the K = 1 diagnostic.

```r
source("scripts/R/dynIRT_KD.R")

# Optional: only for K = 1 diagnostic
# install.packages("emIRT")
# library(emIRT)
```

---

### Step 3.1: Prepare `.data` list for `dynIRT_KD()`

For each issue area, build the required list (same structure as `emIRT::dynIRT()` and `dynIRT_KD()`):

```r
# Load the flow matrix from Phase 1
flow <- readRDS("data/processed/trade_flow_matrix.rds")

# .data list
data_list <- list(
  rc           = flow$rc,            # N × J matrix: 1, -1, 0
  startlegis   = flow$startlegis,    # N × 1 matrix: 0-indexed first active period
  endlegis     = flow$endlegis,      # N × 1 matrix: 0-indexed last active period
  bill.session = flow$bill.session,  # J × 1 matrix: 0-indexed period per item
  T            = flow$T              # integer: 6
)
```

**Dimension checks** (run these, they catch the most common bugs):

```r
stopifnot(is.matrix(data_list$rc))
stopifnot(all(data_list$rc %in% c(-1, 0, 1)))
stopifnot(nrow(data_list$rc) == nrow(data_list$startlegis))
stopifnot(ncol(data_list$rc) == nrow(data_list$bill.session))
stopifnot(all(data_list$startlegis >= 0))
stopifnot(all(data_list$endlegis <= data_list$T - 1))
stopifnot(all(data_list$bill.session >= 0))
stopifnot(all(data_list$bill.session <= data_list$T - 1))
stopifnot(all(data_list$startlegis <= data_list$endlegis))
```

---

### Step 3.2: Set starting values (`.starts`)

Good starting values speed convergence. Use PCA results from Phase 2 (first two PCs):

```r
K <- 2L

# PCA scores (Phase 2)
pc_scores <- pca_result$x[, 1:K, drop = FALSE]
pc_scores <- scale(pc_scores)  # center and scale each PC

# Starting ideal points: N × K × T array
x_start <- array(NA_real_, dim = c(N, K, T_periods))
for (t in 1:T_periods) {
  x_start[, , t] <- pc_scores
}

# Starting item parameters (random or PCA-informed)
alpha_start <- rnorm(J, 0, 0.1)                 # J
beta_start  <- matrix(rnorm(J * K, 0, 0.5), nrow = J, ncol = K)  # J × K

starts_list <- list(
  alpha = alpha_start,   # J
  beta  = beta_start,    # J × K
  x     = x_start        # N × K × T
)
```

**Tip**: If the model converges to a degenerate solution, try alternative starts: PCA-based vs random, or embed 1D estimates into K = 2 (see `docs/estimation_plan_2d.md` for the initialization options).

---

### Step 3.3: Set priors (`.priors`) — THIS IS WHERE IDENTIFICATION HAPPENS

**Identification strategy**: Use informative priors on **K+1 anchor countries** for the initial ideal points, and enforce **sign constraints** on anchor items (no tight priors on item parameters).

```r
K <- 2L

# Default (diffuse) priors for most countries
x_mu0    <- matrix(0, nrow = N, ncol = K)     # prior mean for x at t=1
x_sigma0 <- matrix(1, nrow = N, ncol = K)     # diagonal variances for x at t=1

# ---- ANCHORING: tighten priors for anchor countries ----
idx_denmark <- which(country_codes == "DNK")
idx_iran    <- which(country_codes == "IRN")
idx_china   <- which(country_codes == "CHN")

# Denmark (+2, +2)
x_mu0[idx_denmark, ]    <- c(2.0, 2.0)
x_sigma0[idx_denmark, ] <- c(0.01, 0.01)

# Iran (-2, -2)
x_mu0[idx_iran, ]    <- c(-2.0, -2.0)
x_sigma0[idx_iran, ] <- c(0.01, 0.01)

# China (+1, -1) — off-diagonal anchor
x_mu0[idx_china, ]    <- c(1.0, -1.0)
x_sigma0[idx_china, ] <- c(0.01, 0.01)

# Item parameter priors (diffuse)
beta_mu    <- rep(0, K + 1)              # mean for (alpha, beta_1, ..., beta_K)
beta_sigma <- 25 * diag(K + 1)           # diffuse prior covariance

# Evolution covariance (random walk)
omega <- 0.1 * diag(K)

priors_list <- list(
  x.mu0      = x_mu0,        # N × K
  x.Sigma0   = x_sigma0,     # N × K (diagonal variances)
  beta.mu    = beta_mu,      # (K+1)
  beta.sigma = beta_sigma,   # (K+1) × (K+1)
  omega      = omega         # K × K
)
```

**How to choose omega**:
- 0.01 · I_K: very smooth trajectories, ideal points barely change across periods.
- 0.1 · I_K: moderate smoothness (recommended starting value).
- 1.0 · I_K: ideal points can jump substantially between periods.
- After initial estimation, examine the estimated trajectories. If they look too erratic, decrease omega. If too flat, increase it.

---

### Step 3.4: Set control parameters and run

```r
control_list <- list(
  threads  = 1L,       # set to number of CPU cores if desired
  verbose  = TRUE,     # print progress
  thresh   = 1e-6,     # convergence threshold
  maxit    = 500L,     # max EM iterations
  checkfreq = 50L      # print every 50 iterations
)

# ---- RUN THE MODEL (K = 2) ----
result <- dynIRT_KD(
  .data    = data_list,
  .starts  = starts_list,
  .priors  = priors_list,
  .control = control_list,
  K        = 2L
)
```

**What to monitor during estimation**:
- The log-likelihood (or ELBO) should increase monotonically at every iteration. If it decreases, something is wrong with the data or starting values.
- Typical convergence: 100–300 iterations for well-behaved data. If it hits 500 without converging, increase `maxit` to 1000 or adjust starting values.

---

### Step 3.5: Extract and examine results

```r
# Ideal point estimates: N × K × T array
ideal_points <- result$means$x

# Item parameters
alpha <- result$means$alpha  # J
beta  <- result$means$beta   # J × K

# Convergence info
result$runtime$iters       # iterations used
result$runtime$conv        # 1 = converged, 0 = hit maxit
result$runtime$threads
```

**Post-estimation checks**:

1. **Convergence**: `result$runtime$conv` must be 1. If 0, increase `maxit` and re-run.

2. **Anchor verification**:
   - Denmark should be near (+2, +2), Iran near (−2, −2), China near (+1, −1) at t = 1.
   - If the configuration is rotated or flipped, align via Procrustes to the anchor targets.

3. **Anchor item verification** (item-side sign constraints):
   - Anchor items must have **positive** loadings on their intended dimension.
   - If any anchor item violates the sign constraint, fix the sign and re-estimate or flag as identification failure.

4. **Item parameter reasonableness**:
   - `beta` values should mostly be between -5 and +5. Extreme values (|beta| > 10) suggest identification problems or items with near-zero variation.
   - `alpha` values can range more widely but should not be extreme.

5. **Ideal point trajectories**: Plot 2D paths over time for 10+ key countries. Do they make substantive sense (e.g., economic vs security/HR separation)?

---

### Step 3.6: Handle standard errors

**The problem**: `dynIRT_KD()` does not provide reliable uncertainty estimates by default. If standard errors are needed for the paper, use a bootstrap.

**Solution: Parametric bootstrap** (recommended if SEs are needed for the paper):

```r
B <- 200  # number of bootstrap replications
boot_ideal_points <- array(NA, dim = c(N, T_periods, B))

for (b in 1:B) {
  # Resample the vote matrix: for each non-missing entry,
  # simulate a new vote using the estimated item parameters
  # and the estimated ideal points (parametric bootstrap)
  rc_boot <- simulate_votes(ideal_points, alpha, beta, data_list)

  data_boot <- data_list
  data_boot$rc <- rc_boot

  result_boot <- dynIRT_KD(
    .data = data_boot, .starts = starts_list,
    .priors = priors_list,
    .control = list(threads = 1, verbose = FALSE,
                    thresh = 1e-5, maxit = 300),
    K = 2L
  )

  boot_ideal_points[, , b] <- result_boot$means$x
  cat("Bootstrap", b, "of", B, "\n")
}

# Compute SEs as the SD across bootstrap samples
se_matrix <- apply(boot_ideal_points, c(1, 2), sd)
```

**`simulate_votes()` helper function** (must be written):

```r
simulate_votes <- function(x, alpha, beta, data_list) {
  N <- dim(x)[1]
  K <- dim(x)[2]
  J <- length(alpha)
  rc_sim <- matrix(0L, nrow = N, ncol = J)

  for (j in 1:J) {
    t_j <- data_list$bill.session[j, 1] + 1  # convert 0-indexed to 1-indexed
    linpred <- alpha[j] + x[, , t_j] %*% beta[j, ]
    prob_yea <- pnorm(as.vector(linpred))
    votes <- rbinom(N, 1, prob_yea)
    rc_sim[, j] <- ifelse(votes == 1, 1L, -1L)
  }

  # Re-apply missing data pattern from original
  rc_sim[data_list$rc == 0] <- 0L

  return(rc_sim)
}
```

**Warning**: The bootstrap is computationally expensive — 200 replications × 4 issue areas. On a laptop, this could be slow. Start with B = 50 to test, then increase.

**Alternative**: If bootstrap is too slow, report ideal points without SEs and note the limitation. Most ideal-point papers in IR report point estimates with at most MCMC-based credible intervals, not bootstrap SEs.

---

### Step 3.7: Save results

```r
# For each issue area:
saveRDS(list(
  ideal_points = ideal_points,       # N × T matrix
  se           = se_matrix,          # N × T matrix (if bootstrap was run)
  alpha        = alpha,              # J × 1
  beta         = beta,               # J × 1
  country_codes = country_codes,     # character vector length N
  period_labels = period_labels,     # character vector length T
  item_labels   = item_labels,       # character vector length J
  convergence   = result$runtime,
  omega2_used   = omega2[1, 1],
  anchors       = list(country_pos = "DNK", country_neg = "IRN",
                       item_anchor = "wto_accession")
), file = "outputs/estimates/trade_ideal_points.rds")
```

Repeat for each issue area. Also export a flat CSV for portability:

```r
# Long format: country, period, ideal_point, se
ideal_df <- expand.grid(
  country_iso3 = country_codes,
  period = period_labels
) |>
  mutate(
    ideal_point = as.vector(ideal_points),
    se = as.vector(se_matrix)
  )

write.csv(ideal_df, "outputs/estimates/trade_ideal_points.csv", row.names = FALSE)
```

---

## 7. Phase 4 — Validation

### Objective
Assess whether the estimated ideal points are meaningful by comparing with external benchmarks.

### Script
`scripts/R/04_validate.R`

---

### Step 4.1: Download UNGA ideal points (Bailey, Strezhnev & Voeten)

**Source**: Harvard Dataverse
- Persistent URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ
- This is the **year-based** version of the ideal point estimates (preferred over session-based).
- Download the main data file (likely `.tab` or `.csv` format).
- Save to `data/raw/unga_ideal_points/`.

**Key variables** expected in the dataset:
- Country identifier (likely COW code or ISO3 — check and convert with `countrycode`)
- Year
- `IdealPointAll` or similar column with the ideal point estimate

**Aggregation**: UNGA ideal points are annual. Aggregate to 5-year periods by taking the **mean** of annual ideal points within each period for each country.

```r
unga <- read.csv("data/raw/unga_ideal_points/...") |>
  mutate(period = assign_period(year)) |>
  group_by(country_iso3, period) |>
  summarise(unga_ideal_point = mean(ideal_point, na.rm = TRUE))
```

---

### Step 4.2: Compute correlations

For each issue area, merge your ideal points with the UNGA ideal points and compute:

```r
merged <- inner_join(your_ideal_points, unga, by = c("country_iso3", "period"))

# Overall correlation
cor(merged$ideal_point, merged$unga_ideal_point, use = "complete.obs")

# Correlation per period
merged |>
  group_by(period) |>
  summarise(r = cor(ideal_point, unga_ideal_point, use = "complete.obs"))
```

**What to expect**:
- Moderate positive correlations (r = 0.3–0.7) would support construct validity — treaty-based ideal points capture something related to, but not identical to, UNGA voting positions.
- Very high correlations (r > 0.85) would suggest the measures are redundant — interesting but less novel.
- Very low or negative correlations (r < 0.1) would raise concerns — either the model is poorly identified or treaty participation captures a genuinely different dimension.

Save the correlation table as `outputs/validation/unga_correlations.csv`.

---

### Step 4.3: Time-series comparison plots

For ~10 key countries (US, China, Russia, Brazil, India, Germany, Denmark, Iran, South Africa, Japan):

- Plot your treaty-based ideal point (with SE bands if available) alongside the UNGA ideal point, both over the 6 periods.
- Use dual y-axes or standardize both to z-scores for comparability.
- Save as `outputs/validation/fig_timeseries_{country}.png`.

**Numbered figure convention** (per RULES.md): Number all figures sequentially across the project (Figure 1, Figure 2, etc.).

---

### Step 4.4: Additional external benchmarks (document for later)

These are not required for Phase 4 but should be implemented before the paper is finalized:

| Benchmark | Source | Expected relationship |
|-----------|--------|----------------------|
| Trade openness | Penn World Table (`openk` variable) or WDI | Positive correlation with trade ideal point |
| Human rights | V-Dem liberal democracy index (`v2x_libdem`) | Positive correlation with overall ILO ideal point |
| Environmental performance | Yale EPI (Environmental Performance Index) | Positive correlation with environment ideal point |
| GDP per capita | WDI | Positive but weaker — wealth enables but doesn't determine ILO participation |

---

## 8. Phase 5 (Future) — Multidimensional Stan Model

**This phase is NOT for immediate implementation.** It is documented here for planning purposes.

### Goal
Estimate a joint K-dimensional dynamic IRT model across all issue areas simultaneously, using Stan.

### Approach
- Pool all items from all 4 issue areas into a single vote matrix.
- Estimate a K-dimensional ideal point vector per country per period (K = 2 or K = 4).
- Use a non-centered parameterization for the random walk to mitigate autocorrelation:

```
// Non-centered parameterization:
// Instead of: x[i,t] ~ normal(x[i,t-1], sigma)
// Code as:    x[i,t] = x[i,t-1] + sigma * z[i,t],  z[i,t] ~ normal(0,1)
```

- Identification in K > 1 dimensions requires resolving the rotation problem. Standard approach: constrain the discrimination matrix to be lower-triangular with positive diagonal (see Bafumi et al. 2005).
- Use `cmdstanr` with `$variational()` for fast approximate inference on a laptop, or `$sample()` for full MCMC if compute is available.

### Key references for implementation
- Imai, Lo & Olmsted (2016) — the 1D EM approach (what Phase 3 uses)
- Martin & Quinn (2002) — dynamic ideal points via MCMC
- Kubinec (2019) — `idealstan` R package (Stan-based IRT, may support some dynamic models)
- Stan User's Guide, chapter on IRT models

---

## 9. Appendix A — Robustness Checks

These should be run after the main results are established, not during the first estimation.
See `docs/research_design_notes.md` for detailed implementation plans on HIGH-priority items.

| Check | Description | Priority | Notes |
|-------|-------------|----------|-------|
| Stock/cumulative coding | Use cumulative participation instead of flow | **HIGH** | Flow creates "activity bias" penalizing long-standing members. See design notes §1. |
| Alternative country anchors | Replace Denmark/Iran with 2+ alternative anchor pairs per domain | **HIGH** | Risk of artificial cross-domain coherence. See design notes §2a. |
| Item anchor sensitivity | Re-estimate using item-side anchoring (constrain anchor item betas) | **HIGH** | Complementary identification strategy. See design notes §2b. |
| omega2 sensitivity | Re-estimate with omega2 = 0.01, 0.05, 0.5 | **HIGH** | |
| Bootstrap SEs | If not run in main analysis, run for final paper | **HIGH** | |
| 3-year periods | Finer temporal resolution: ~10 periods instead of 6 | **MEDIUM** | Captures rapid systemic shifts (e.g., 2017-18 withdrawals). See design notes §3. |
| Annual periods | Finest resolution (only if sparsity permits) | **MEDIUM** | Try environment domain first (densest data). See design notes §3. |
| Ordinal coding | Replace binary (member/not) with ordinal (signed < ratified < in-force) | Medium | |
| Event-type robustness | Re-estimate using signature-only and ratification-only dates (separate runs) | Medium | |
| Drop regional agreements | Exclude PTAs/alliances with strong regional component | Medium | |
| 4-year periods | Alternative temporal resolution: 7 periods | Low | |
| Multi-dimensional model | Estimate K > 1 dimensions jointly across domains | **SPECIFIED** | Full EM algorithm documented in `docs/estimation_plan_2d.md`. Custom R implementation (no Stan needed). See design notes §4. |

---

## 10. Appendix B — Key References

1. **Imai, K., Lo, J., & Olmsted, J. (2016)**. "Fast Estimation of Ideal Points with Massive Data." *American Political Science Review*, 110(4), 631–656. — The method behind `emIRT::dynIRT()`.

2. **Martin, A. D., & Quinn, K. M. (2002)**. "Dynamic Ideal Point Estimation via Markov Chain Monte Carlo for the U.S. Supreme Court, 1953–1999." *Political Analysis*, 10(2), 134–153. — The original dynamic IRT model for ideal points.

3. **Bailey, M. A., Strezhnev, A., & Voeten, E. (2017)**. "Estimating Dynamic State Preferences from United Nations Voting Data." *Journal of Conflict Resolution*, 61(2), 430–456. — The UNGA ideal points benchmark.

4. **Kubinec, R. (2019)**. "Generalized Ideal Point Models for Time-Varying and Missing-Data Inference." *OSF Preprints*. — `idealstan` R package, Stan-based.

5. **emIRT CRAN page**: https://cran.r-project.org/package=emIRT

6. **UNGA ideal points data**: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ

7. **emIRT GitHub**: https://github.com/kosukeimai/emIRT
