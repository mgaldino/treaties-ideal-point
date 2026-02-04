# Plan: Acquire & Integrate 3 New Issue-Area Data Sources

**Date**: 2026-02-02
**Status**: Approved (pending implementation)
**Goal:** Add Human Rights, Arms Control, and WIPO treaties as new issue areas for ideal-point estimation.

**Execution order:** All acquisition first (Phase 0), then all R integration (Phase 1), then EDA (Phase 2), then **STOP and wait for user review of PCA/anchors**, then estimation (Phase 3), then validation (Phase 4). Each phase must be tested before proceeding.

---

## Phase 0 — Data Acquisition (Python)

Create 3 new Python scripts following the existing `acquire_*.py` pattern (standard library only, uses `_common.py` helpers, outputs to `data/raw/{source}/`, writes `access_log.json`).

### 0A. Human Rights Treaties — `scripts/python/acquire_un_hr_treaties.py`

**Issue area:** `human_rights`

**What to acquire:** Ratification/accession dates for the 9 core UN human rights treaties + optional protocols (~18-20 instruments):
- ICCPR (1966), ICCPR-OP1, ICCPR-OP2
- ICESCR (1966), ICESCR-OP
- CERD (1965)
- CEDAW (1979), CEDAW-OP
- CAT (1984), CAT-OP (OPCAT)
- CRC (1989), CRC-OP-AC, CRC-OP-SC, CRC-OP-IC
- CMW (1990)
- CRPD (2006), CRPD-OP
- CED (2006)

**Primary source:** OHCHR status of ratification page:
- https://indicators.ohchr.org/ (interactive dashboard with export)
- Individual treaty pages at https://treaties.un.org/ (Chapter IV: Human Rights)

**Fallback source:** Scrape individual treaty status pages from treaties.un.org (Chapter IV, treaties 1-9 + protocols).

**Acquisition strategy:**
1. Try OHCHR indicators site first — it has a structured ratification dashboard that may allow bulk download or HTML table scraping
2. If OHCHR fails, scrape individual treaty status pages from treaties.un.org (Chapter IV, treaties 1-9 + protocols)

**Output directory:** `data/raw/un_hr_treaties/`

**Output files:**
- `un_hr_ratifications.csv` — columns: `country_name`, `treaty_id` (e.g., `iccpr`, `cedaw_op`), `treaty_name`, `action_type` (Ratification/Accession/Succession), `action_date`, `signature_date`
- `access_log.json`

**Validation checks:**
- At least 150 countries should appear
- At least 15 treaty instruments
- No action_date before the treaty's open date
- No duplicate country-treaty pairs

---

### 0B. Arms Control Treaties — `scripts/python/acquire_arms_control.py`

**Issue area:** `arms_control`

**What to acquire:** Ratification/accession dates for major disarmament/arms control treaties (~15-20 instruments):
- NPT (1968) — Non-Proliferation Treaty
- BWC (1972) — Biological Weapons Convention
- ENMOD (1976) — Environmental Modification Convention
- CCW (1980) — Convention on Certain Conventional Weapons + Protocols I-V
- CWC (1993) — Chemical Weapons Convention
- CTBT (1996) — Comprehensive Nuclear-Test-Ban Treaty
- Ottawa Treaty (1997) — Anti-Personnel Mine Ban Convention
- CCM (2008) — Convention on Cluster Munitions
- ATT (2013) — Arms Trade Treaty
- TPNW (2017) — Treaty on Prohibition of Nuclear Weapons

**Primary source:** UN Treaty Collection, Chapter XXVI (Disarmament):
- https://treaties.un.org/Pages/Treaties.aspx?id=26&subid=A&clang=_en

**Fallback source:** UNODA treaty database: https://disarmament.unoda.org/

**Acquisition strategy:**
1. Scrape treaty status pages from treaties.un.org Chapter XXVI
2. Each treaty page lists signatories and parties with dates
3. If treaties.un.org is hard to parse, use UNODA as fallback

**Output directory:** `data/raw/arms_control/`

**Output files:**
- `arms_control_ratifications.csv` — columns: `country_name`, `treaty_id` (e.g., `npt`, `cwc`, `att`), `treaty_name`, `action_type`, `action_date`, `signature_date`, `treaty_open_year`
- `access_log.json`

**Validation checks:**
- NPT should have ~191 parties
- CWC should have ~193 parties
- CTBT should have ~170+ ratifications
- No action_date before treaty open date

---

### 0C. WIPO-Administered Treaties — `scripts/python/acquire_wipo_treaties.py`

**Issue area:** `intellectual_property`

**What to acquire:** Ratification/accession dates for WIPO-administered treaties (~26 instruments):
- Paris Convention (1883), Berne Convention (1886)
- Madrid Agreement (1891), Madrid Protocol (1989)
- Hague Agreement (1925), Lisbon Agreement (1958)
- PCT (1970), Budapest Treaty (1977)
- Nairobi Treaty (1981), TLT (1994)
- WCT (1996) — WIPO Copyright Treaty
- WPPT (1996) — WIPO Performances and Phonograms Treaty
- PLT (2000), Singapore Treaty (2006)
- Beijing Treaty (2012), Marrakesh Treaty (2013)
- And others listed at WIPO treaties portal

**Primary source:** WIPO Treaties Portal:
- https://www.wipo.int/treaties/en/
- Each treaty has a "Contracting Parties" page with country list and dates

**Acquisition strategy:**
1. Start from the WIPO treaties index page to get list of all treaties + URLs
2. For each treaty, scrape the "Contracting Parties" page
3. Parse tables: country, date of accession/ratification, entry into force date

**Output directory:** `data/raw/wipo_treaties/`

**Output files:**
- `wipo_ratifications.csv` — columns: `country_name`, `treaty_id` (e.g., `paris`, `pct`, `wct`), `treaty_name`, `action_type`, `action_date`, `entry_into_force_date`, `treaty_open_year`
- `access_log.json`

**Validation checks:**
- Paris Convention should have ~177 parties
- PCT should have ~157 parties
- Berne Convention should have ~181 parties
- No action_date before treaty open year

---

### Phase 0 testing

After all 3 acquisition scripts run:
- Verify each CSV exists and has expected row counts
- Verify access_log.json written for each
- Spot-check a few countries (DNK, USA, IRN, PRK) across all 3 datasets for plausibility
- Log results to `logs/phase0_acquire_{source}_TIMESTAMP.log`

---

## Phase 1 — R Integration (`scripts/R/01_prepare_data.R`)

Add 3 new `build_*()` functions and wire them into the pipeline.

### 1A. `build_un_hr_treaties()`

```
Raw input: data/raw/un_hr_treaties/un_hr_ratifications.csv
Issue area: "human_rights"
item_id format: "unhr_{treaty_id}" (e.g., "unhr_iccpr", "unhr_cedaw_op")
Country code conversion: countrycode(country_name, "country.name", "iso3c")
Event: use action_date (ratification/accession date) as EIF proxy
Filter: event_year >= 1990 & event_year <= 2018
```

### 1B. `build_arms_control()`

```
Raw input: data/raw/arms_control/arms_control_ratifications.csv
Issue area: "arms_control"
item_id format: "arms_{treaty_id}" (e.g., "arms_npt", "arms_cwc", "arms_att")
Country code conversion: countrycode(country_name, "country.name", "iso3c")
Event: use action_date as EIF proxy
Filter: event_year >= 1990 & event_year <= 2018
Note: Many treaties opened before 1990 but had ratifications during 1990-2018.
      treaty_open_year should reflect the treaty's actual open date (even if before 1990).
```

### 1C. `build_wipo_treaties()`

```
Raw input: data/raw/wipo_treaties/wipo_ratifications.csv
Issue area: "intellectual_property"
item_id format: "wipo_{treaty_id}" (e.g., "wipo_paris", "wipo_pct", "wipo_wct")
Country code conversion: countrycode(country_name, "country.name", "iso3c")
Event: use action_date or entry_into_force_date as EIF
Filter: event_year >= 1990 & event_year <= 2018
```

### 1D. Wire into pipeline

In `01_prepare_data.R`, after existing build calls (~line 657):

```r
unhr_out <- build_un_hr_treaties()
arms_out <- build_arms_control()
wipo_out <- build_wipo_treaties()
```

Add to `all_events` bind_rows (~line 659):
```r
all_events <- dplyr::bind_rows(
  # ... existing 6 sources ...
  unhr_out$events,
  arms_out$events,
  wipo_out$events
)
```

Same for `all_items` (~line 668).

Save excluded entities if any.

### 1E. Update issue_areas vector

In `01_prepare_data.R` (~line 888), change:
```r
issue_areas <- c("trade", "investment", "security", "environment",
                 "human_rights", "arms_control", "intellectual_property")
```

This ensures flow matrices are generated for all 7 areas.

### Phase 1 testing

- Run `Rscript scripts/R/01_prepare_data.R`
- Verify 3 new flow matrix files exist:
  - `data/processed/human_rights_flow_matrix.rds`
  - `data/processed/arms_control_flow_matrix.rds`
  - `data/processed/intellectual_property_flow_matrix.rds`
- Verify `data/processed/item_codebook.csv` contains the 3 new issue areas
- Verify `outputs/validation/flow_matrix_summary_*.csv` for each new area
- Check sparsity: % of 0s, 1s, -1s for each new area (expect mostly -1 and 0, with 1s being minority)
- Log: `logs/phase1_prepare_TIMESTAMP.log`

---

## Phase 2 — Exploratory Data Analysis (`scripts/R/02_eda.R`)

### 2A. Update issue area list

In `02_eda.R` (~line 62), add the 3 new issue areas to the vector.

### 2B. Run EDA

- PCA scree plots for each new area
- PCA country loadings — verify anchor candidates
- PCA item loadings — check which treaties load most strongly
- Time-series participation plots for key countries

### 2C. Anchor verification (critical)

Expected anchor behavior based on substantive priors:

| Area | Positive anchor (pro-ILO) | Negative anchor (anti-ILO) | Rationale |
|------|--------------------------|---------------------------|-----------|
| human_rights | DNK | PRK or SAU | Scandinavian countries ratify all HR treaties; North Korea/Saudi Arabia ratify few |
| arms_control | DNK or NZL | PRK or ISR | NZ is strongly pro-disarmament; NK/Israel resist key treaties (CTBT, TPNW, Ottawa) |
| intellectual_property | DNK or FRA | PRK or SOM | France/Denmark are party to most WIPO treaties; NK/Somalia to very few |

**Important:** The PCA results will determine final anchors. If the expected anchors don't appear at extremes, investigate scale flipping or choose alternatives from PCA extremes.

### Phase 2 testing

- Run `Rscript scripts/R/02_eda.R`
- Inspect scree plots: first eigenvalue should dominate if 1D model is appropriate
- Inspect country loadings: verify anchor candidates appear near extremes
- Outputs in `outputs/eda_phase2/`
- Log: `logs/phase2_eda_TIMESTAMP.log`

### >>> STOP HERE — Wait for user review <<<

After Phase 2 completes, **do not proceed to Phase 3**. Instead:
1. Report PCA results to the user (scree plots, top/bottom country loadings, item loadings)
2. Present anchor candidates for each area with their PCA ranks/percentiles
3. Wait for user to approve or modify anchor choices
4. Only after user approval, proceed to Phase 3 with the confirmed anchors

---

## Phase 3 — Estimation (`scripts/R/03_estimate_ideal_points.R`)

**Prerequisite:** User has reviewed Phase 2 PCA results and approved anchor choices.

### 3A. Update areas and anchor_map

In `03_estimate_ideal_points.R`:

```r
areas <- c("investment", "security", "environment",
           "human_rights", "arms_control", "intellectual_property")

anchor_map <- list(
  investment            = list(pos = "DNK", neg = "IRN"),
  security              = list(pos = "DNK", neg = "IRN"),
  environment           = list(pos = "DNK", neg = "SAU"),
  human_rights          = list(pos = "???", neg = "???"),  # from user review
  arms_control          = list(pos = "???", neg = "???"),  # from user review
  intellectual_property = list(pos = "???", neg = "???")   # from user review
)
```

**Anchors must come from user-approved Phase 2 review.**

### 3B. Run estimation

- dynIRT for each new area
- Check convergence (result$runtime$conv == 1)
- Verify anchor countries have expected signs
- Save outputs: `outputs/estimates/{area}_ideal_points.rds`, `.csv`, `_item_params.csv`

### Phase 3 testing

- Run `Rscript scripts/R/03_estimate_ideal_points.R`
- Verify convergence for all 3 new areas
- Verify positive-anchor ideal points are positive, negative-anchor ideal points are negative
- Verify item beta signs make substantive sense
- Log: `logs/phase3_estimate_TIMESTAMP.log`

---

## Phase 4 — Validation (`scripts/R/04_validate.R`)

### 4A. Update validation script

Add the 3 new areas to the validation loop. Compute correlations with UNGA ideal points.

### 4B. Expected correlation patterns

| Area | Expected UNGA correlation | Rationale |
|------|--------------------------|-----------|
| human_rights | Moderate positive (0.3-0.6) | HR treaty ratification partially overlaps with UNGA alignment |
| arms_control | Moderate positive (0.3-0.5) | Disarmament commitment correlates with multilateral engagement |
| intellectual_property | Low (0.0-0.3) | IP regime is more technical, less politically aligned |

### Phase 4 testing

- Run validation scripts
- Verify correlation tables and time-series plots generated
- Re-render validation report PDF
- Log: `logs/phase4_validate_TIMESTAMP.log`

---

## Phase 5 — Documentation

- Update `HANDOFF.md` after each phase completion
- Update `docs/source_registry.md` with 3 new sources (name, URL, citation, coverage)
- Update `docs/data_dictionary.md` if it exists

---

## Critical files to modify

| File | Changes |
|------|---------|
| `scripts/python/acquire_un_hr_treaties.py` | **NEW** |
| `scripts/python/acquire_arms_control.py` | **NEW** |
| `scripts/python/acquire_wipo_treaties.py` | **NEW** |
| `scripts/R/01_prepare_data.R` | Add 3 build_*() functions + wire into pipeline + update issue_areas |
| `scripts/R/02_eda.R` | Add 3 new issue areas to loop |
| `scripts/R/03_estimate_ideal_points.R` | Add 3 new areas + anchor_map entries (after user review) |
| `scripts/R/04_validate.R` | Add 3 new areas to validation loop |
| `scripts/R/04_validation_summary.R` | Add 3 new areas |
| `docs/source_registry.md` | Add 3 new source entries |
| `HANDOFF.md` | Update after each phase |

## Execution order summary

1. Run all 3 Python acquisition scripts (can be parallelized)
2. Validate all raw data outputs
3. Update and run `01_prepare_data.R`
4. Validate flow matrices
5. Update and run `02_eda.R`
6. **STOP — Report PCA results to user and wait for anchor approval**
7. Update and run `03_estimate_ideal_points.R` (with user-approved anchors)
8. Validate convergence and estimates
9. Update and run `04_validate.R`
10. Update documentation (HANDOFF.md, source_registry.md)
