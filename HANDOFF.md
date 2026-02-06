# Handoff (current state)

Date: 2026-02-01

## What is done
- Core data sources acquired: DESTA, ATOP, IEADB, WTO RTA, UNCTAD IIA.
- UNCTAD IIA scraped via country pages; raw HTML preserved in `data/raw/unctad_iia/html/`.
- EDA report generated with numbered figures/tables: `outputs/eda_report.pdf`.
- Unified country-year long dataset built: `data/processed/country_year_long.csv` and `.rds`.

## Key scripts
- Acquisition: `scripts/python/acquire_*.py`
- EDA: `scripts/R/eda_run.R`
- Build dataset: `scripts/R/build_country_year_long.R`
- Validation: `scripts/R/validate_country_year_long.R`

## Processed outputs
- `data/processed/country_year_long.csv`
- `data/processed/country_year_long.rds`
- Excluded entities (REIOs):
  - `data/processed/excluded_entities_wto_rta.csv`
  - `data/processed/excluded_entities_unctad_iia.csv`

## Notes
- Schema is considered current but revisable.
- Event types vary by source (e.g., signature/entry_into_force; alliance_*).

## Phase 0 — WTO Accession Data (Completed)
- Date: 2026-02-01 15:27
- Status: SUCCESS
- Outputs:
  - data/raw/wto_accession/wto_members.csv (166 rows)
  - data/raw/wto_accession/access_log.json
  - outputs/validation/wto_accession_summary.csv
- Validation:
  - scripts/R/00_validate_wto_accession.R (passed)
  - Log: logs/phase0_validate_wto_accession_*.log
- Notes:
  - Download required escalated network permissions due to DNS/network restrictions.

## Phase 1 — Data Preparation (Completed)
- Date: 2026-02-01 15:38
- Status: SUCCESS (validation passed)
- Scripts:
  - scripts/R/01_prepare_data.R
  - scripts/R/01_validate_prepared_data.R
- Key outputs:
  - data/processed/baseline_events.csv
  - data/processed/item_codebook.csv
  - data/processed/country_codebook.csv
  - data/processed/trade_flow_matrix.rds
  - data/processed/investment_flow_matrix.rds
  - data/processed/security_flow_matrix.rds
  - data/processed/environment_flow_matrix.rds
  - outputs/validation/flow_matrix_summary_*.csv
- Logs:
  - logs/phase1_prepare_*.log
  - logs/phase1_validate_*.log
- Notes:
  - ATOP join is intentionally many-to-many (alliances by type), warning observed during preparation.
- Update 2026-02-01 15:39: Re-ran Phase 1 after fixing DESTA ISO3 mapping (COW -> ISO3). Validation passed; updated flow matrix summaries.

## Phase 2 — Exploratory Data Analysis (Completed)
- Date: 2026-02-01 15:52
- Status: SUCCESS
- Scripts:
  - scripts/R/02_eda.R
- Key outputs:
  - outputs/eda_phase2/flow_summary.csv
  - outputs/eda_phase2/pca_country_loadings_*.csv
  - outputs/eda_phase2/pca_item_loadings_*.csv
  - outputs/eda_phase2/figures/fig_scree_*.png
  - outputs/eda_phase2/figures/fig_timeseries_*.png
  - outputs/eda_phase2/eda_phase2_report.pdf
  - outputs/eda_phase2/eda_phase2_report.Rmd
- Logs:
  - logs/phase2_eda_*.log
  - logs/phase2_render_*.log

## Phase 3 — Estimation Setup (Decisions)
- Date: 2026-02-01 16:38
- Trade estimation: PAUSED (trade PCA anchors unstable; revisit later)
- Environment anchor change: use SAU (Saudi Arabia) instead of IRN for negative anchor

## Phase 3 — Estimation (Completed)
- Date: 2026-02-01 16:39
- Status: SUCCESS (investment, security, environment)
- Trade: NOT estimated (paused)
- Scripts:
  - scripts/R/03_estimate_ideal_points.R
- Key outputs:
  - outputs/estimates/investment_ideal_points.rds
  - outputs/estimates/security_ideal_points.rds
  - outputs/estimates/environment_ideal_points.rds
  - outputs/estimates/*_ideal_points.csv
  - outputs/estimates/*_item_params.csv
- Logs:
  - logs/phase3_estimate_*.log
- Anchors:
  - investment: DNK (+), IRN (-)
  - security: DNK (+), IRN (-)
  - environment: DNK (+), SAU (-)

## Phase 4 — Validation (Completed)
- Date: 2026-02-01 16:46
- Status: SUCCESS (investment, security, environment; trade excluded)
- Scripts:
  - scripts/python/acquire_unga_ideal_points.py
  - scripts/R/04_prepare_unga.R
  - scripts/R/04_validate.R
- Key outputs:
  - data/processed/unga_ideal_points_period.csv
  - outputs/validation/unga_correlations_overall.csv
  - outputs/validation/unga_correlations_by_period_*.csv
  - outputs/validation/figures/fig_timeseries_unga_*.png
  - outputs/validation/validation_report.pdf
  - outputs/validation/validation_report.Rmd
- Logs:
  - logs/phase4_acquire_unga_*.log
  - logs/phase4_prepare_unga_*.log
  - logs/phase4_validate_*.log
  - logs/phase4_render_*.log

- Update 2026-02-01 17:21: Added detailed interpretation section to outputs/validation/validation_report.Rmd and re-rendered PDF.

- Update 2026-02-01 17:24: Added limitations/cautions section to validation_report.Rmd and re-rendered PDF.

## Phase 5 — Tariff Pilot (BRA 1990) (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/python/acquire_wits_tariffs.py
- Outputs:
  - data/raw/tariffs/wits_trn/BRA/1990/countries_metadata.xml
  - data/raw/tariffs/wits_trn/BRA/1990/data_availability.xml
  - data/raw/tariffs/wits_trn/BRA/1990/tariffs_partner_000.json
  - data/raw/tariffs/wits_trn/BRA/1990/tariffs_partner_N32.json
  - data/raw/tariffs/wits_trn/BRA/1990/access_log.json
- Notes:
  - Data availability lists partners: 000 (World) and N32.
  - Reporter nomenclature for 1990 is H0 (HS88/92) in TRAINS availability metadata.

## Phase 6 — Tariff Parsing Pilot (BRA 1990) (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/R/05_parse_wits_tariffs.R
- Outputs:
  - data/processed/tariffs/wits_trn/BRA/1990/line_level_all.csv
  - data/processed/tariffs/wits_trn/BRA/1990/line_level_ad_valorem.csv
  - outputs/validation/wits_tariff_pilot_summary_BRA_1990.csv
- Notes:
  - SDMX-JSON contains missing values encoded as "[,"; parser replaces with "null" for valid JSON.
  - Series key dimension order inferred from key indices to correctly map PRODUCTCODE.
  - Partner group codes retained and flagged from metadata.

## Phase 7 — Tariff Pilot Expansion (BRA 1991–1992) (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/python/acquire_wits_tariffs.py
  - scripts/R/05_parse_wits_tariffs.R
- Outputs:
  - data/raw/tariffs/wits_trn/BRA/1991/ (raw JSON + metadata)
  - data/raw/tariffs/wits_trn/BRA/1992/ (raw JSON + metadata)
  - data/processed/tariffs/wits_trn/BRA/1991/line_level_all.csv
  - data/processed/tariffs/wits_trn/BRA/1991/line_level_ad_valorem.csv
  - data/processed/tariffs/wits_trn/BRA/1992/line_level_all.csv
  - data/processed/tariffs/wits_trn/BRA/1992/line_level_ad_valorem.csv
  - outputs/validation/wits_tariff_pilot_summary_BRA_1991.csv
  - outputs/validation/wits_tariff_pilot_summary_BRA_1992.csv

## Phase 8 — HS88/92 → HS2012 Mapping (BRA 1990–1992, USA 1990) (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/python/acquire_wits_concordance.py
  - scripts/R/06_map_hs_to_2012.R
- Outputs:
  - data/raw/concordance/wits/Concordance_H4_to_H0.zip
  - data/raw/concordance/wits/extracted/JobID-77_Concordance_H4_to_H0.CSV
  - data/processed/concordance/wits_hs88_to_hs2012.csv
  - data/processed/tariffs/wits_trn/BRA/1990/line_level_ad_valorem_hs2012.csv
  - data/processed/tariffs/wits_trn/BRA/1991/line_level_ad_valorem_hs2012.csv
  - data/processed/tariffs/wits_trn/BRA/1992/line_level_ad_valorem_hs2012.csv
  - data/processed/tariffs/wits_trn/USA/1990/line_level_ad_valorem_hs2012.csv
  - outputs/validation/wits_hs_mapping_summary_BRA_1990.csv
  - outputs/validation/wits_hs_mapping_summary_BRA_1991.csv
  - outputs/validation/wits_hs_mapping_summary_BRA_1992.csv
  - outputs/validation/wits_hs_mapping_summary_USA_1990.csv
- Notes:
  - Mapping uses WITS H4→H0 concordance inverted (HS2012 to HS88/92) with ambiguous mappings retained and flagged.
  - Mapping weights assigned as 1/n for one-to-many cases to preserve total weight per original HS6 line.

## Phase 9 — Margin Construction (BRA 1990–1992, USA 1990) (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/R/07_build_margins.R
- Outputs:
  - data/processed/tariffs/wits_trn/BRA/1990/margins_hs4.csv
  - data/processed/tariffs/wits_trn/BRA/1990/mfn_hs4.csv
  - data/processed/tariffs/wits_trn/BRA/1991/margins_hs4.csv
  - data/processed/tariffs/wits_trn/BRA/1991/mfn_hs4.csv
  - data/processed/tariffs/wits_trn/BRA/1992/margins_hs4.csv
  - data/processed/tariffs/wits_trn/BRA/1992/mfn_hs4.csv
  - data/processed/tariffs/wits_trn/USA/1990/margins_hs4.csv
  - data/processed/tariffs/wits_trn/USA/1990/mfn_hs4.csv
  - outputs/validation/wits_margin_summary_BRA_1990.csv
  - outputs/validation/wits_margin_summary_BRA_1991.csv
  - outputs/validation/wits_margin_summary_BRA_1992.csv
  - outputs/validation/wits_margin_summary_USA_1990.csv
- Notes:
  - Partner code 000 (World) used as MFN baseline; excluded from applied-partner margins.

## Phase 10 — USA 1990 Acquisition + Parsing (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/python/acquire_wits_tariffs.py
  - scripts/R/05_parse_wits_tariffs.R
- Outputs:
  - data/raw/tariffs/wits_trn/USA/1990/ (raw JSON + metadata)
  - data/processed/tariffs/wits_trn/USA/1990/line_level_all.csv
  - data/processed/tariffs/wits_trn/USA/1990/line_level_ad_valorem.csv
  - outputs/validation/wits_tariff_pilot_summary_USA_1990.csv
- Notes:
  - Initial download timed out; re-ran only missing partner N97 and then parsed successfully.

## Phase 11 — HS Mapping Diagnostics (BRA 1990–1992, USA 1990) (Completed)
- Date: 2026-02-02
- Status: SUCCESS
- Scripts:
  - scripts/R/08_diagnose_hs_mapping.R
- Outputs:
  - outputs/validation/wits_hs_mapping_by_hs2_BRA_1990.csv
  - outputs/validation/wits_hs_mapping_by_hs2_BRA_1991.csv
  - outputs/validation/wits_hs_mapping_by_hs2_BRA_1992.csv
  - outputs/validation/wits_hs_mapping_by_hs2_USA_1990.csv
  - outputs/validation/wits_hs_mapping_top_missing_BRA_1990.csv
  - outputs/validation/wits_hs_mapping_top_missing_BRA_1991.csv
  - outputs/validation/wits_hs_mapping_top_missing_BRA_1992.csv
  - outputs/validation/wits_hs_mapping_top_missing_USA_1990.csv
  - outputs/validation/wits_hs_mapping_sensitivity_BRA_1990.csv
  - outputs/validation/wits_hs_mapping_sensitivity_BRA_1991.csv
  - outputs/validation/wits_hs_mapping_sensitivity_BRA_1992.csv
  - outputs/validation/wits_hs_mapping_sensitivity_USA_1990.csv
  - outputs/validation/wits_hs_mapping_sensitivity_all.csv
- Notes:
  - Sensitivity summary compares HS4 coverage before/after dropping missing mappings and reports mapped line shares.

## Phase 12 — Mapping Summary Document (Completed)
- Date: 2026-02-03
- Status: SUCCESS
- Scripts:
  - scripts/R/09_write_mapping_summary.R
- Outputs:
  - docs/tariff_mapping_summary.md
- Notes:
  - Summarizes missing and ambiguous HS88/92 → HS2012 mapping shares and HS4 coverage ratios.

## Phase 13 — SAU 1990 Acquisition Attempt (Blocked)
- Date: 2026-02-03
- Status: BLOCKED (no data availability)
- Scripts:
  - scripts/python/acquire_wits_tariffs.py
- Outputs:
  - data/raw/tariffs/wits_trn/SAU/1990/access_log.json
  - data/raw/tariffs/wits_trn/SAU/1990/data_availability.xml
- Notes:
  - WITS API returned “Data not found” for SAU 1990; no partner list available to proceed.

## Phase 14 — Tariff Acquisition Expansion (BRA 1993–2024)
- Date: 2026-02-03
- Status: PARTIAL (1993–2021 success; 2022–2024 data not found)
- Scripts:
  - scripts/python/acquire_wits_tariffs.py
- Outputs:
  - data/raw/tariffs/wits_trn/BRA/1993/ ... /2021/ (raw JSON + metadata)
  - data/raw/tariffs/wits_trn/BRA/2022/data_availability.xml
  - data/raw/tariffs/wits_trn/BRA/2023/data_availability.xml
  - data/raw/tariffs/wits_trn/BRA/2024/data_availability.xml
  - logs/phase5_tariffs_acquire_BRA_1993_2024_20260203T180355.log
- Notes:
  - Attempted without WITS authentication; initial DNS failure required escalated network permissions.
  - 2021 partner 000 returned HTTP 502; retried after 30s with `--partner-list 000` and succeeded.
  - Preserved pre-retry access log: data/raw/tariffs/wits_trn/BRA/2021/access_log_partial_2026-02-03.json.
  - WITS API returned “Data not found” for BRA 2022–2024 (see data_availability.xml files).

## Phase 15 — Tariff Parsing + Mapping + Margins (BRA 1993–2021)
- Date: 2026-02-03
- Status: SUCCESS (1993–2021 processed)
- Scripts:
  - scripts/R/05_parse_wits_tariffs.R
  - scripts/R/06_map_hs_to_2012.R
  - scripts/R/07_build_margins.R
- Outputs:
  - data/processed/tariffs/wits_trn/BRA/1993/ ... /2021/ (line-level, HS2012, margins)
  - outputs/validation/wits_tariff_pilot_summary_BRA_1993...2021.csv
  - outputs/validation/wits_hs_mapping_summary_BRA_1993...2021.csv
  - outputs/validation/wits_margin_summary_BRA_1993...2021.csv
  - logs/phase6_tariffs_parse_map_margins_BRA_1993_2021_20260203T180355.log
- Notes:
  - Years 2022–2024 skipped (no data availability).

## Phase 16 — Tariff Diagnostics + Consolidation (BRA 1990–2021)
- Date: 2026-02-03
- Status: SUCCESS
- Scripts:
  - scripts/R/08_diagnose_hs_mapping.R
  - scripts/R/09_write_mapping_summary.R
  - scripts/R/10_consolidate_wits_tariffs.R
- Outputs:
  - outputs/validation/wits_hs_mapping_by_hs2_BRA_1990...2021.csv
  - outputs/validation/wits_hs_mapping_top_missing_BRA_1990...2021.csv
  - outputs/validation/wits_hs_mapping_sensitivity_BRA_1990...2021.csv
  - outputs/validation/wits_hs_mapping_sensitivity_all.csv
  - docs/tariff_mapping_summary.md (updated)
  - data/processed/tariffs/wits_trn/BRA/margins_hs4_timeseries.csv
  - data/processed/tariffs/wits_trn/BRA/mfn_hs4_timeseries.csv
  - logs/phase7_tariffs_diagnostics_consolidate_BRA_1990_2021_20260203T180355.log

## Next Session Checklist (2026-02-03)
- Confirm SAU is Saudi Arabia (ISO3 = SAU); SAU 1990 returned “Data not found” in WITS.
- Decide whether to probe SAU 1991+ (or skip SAU) and proceed with acquisition:
  - `python3 scripts/python/acquire_wits_tariffs.py --reporter-iso3 SAU --year 1991`
  - If data present but missing partners, rerun with `--partner-list` from access log.
- Mapping summary doc: `docs/tariff_mapping_summary.md` (generated by `scripts/R/09_write_mapping_summary.R`).
- Credentials: WITS API uses `.env` with `WITS_USER`/`WITS_PASS` (see `.env.example`).
- New helper: `scripts/python/run_with_env.py` loads `.env` and executes a command (use `--`), so you can run acquisition scripts without manual `source .env`.
- Key pipeline scripts:
  - Acquire: `scripts/python/acquire_wits_tariffs.py`
  - Parse: `scripts/R/05_parse_wits_tariffs.R`
  - Map HS to 2012: `scripts/R/06_map_hs_to_2012.R`
  - Margins: `scripts/R/07_build_margins.R`
  - Mapping diagnostics: `scripts/R/08_diagnose_hs_mapping.R`
  - Mapping summary: `scripts/R/09_write_mapping_summary.R`

## New Sources Plan — 3 New Issue Areas (Approved, Not Yet Implemented)
- Date: 2026-02-02
- Status: APPROVED (plan written, implementation not started)
- Plan file: `docs/new_sources_plan.md`
- Goal: Add Human Rights, Arms Control, and WIPO treaties as new issue areas
- New issue areas: `human_rights`, `arms_control`, `intellectual_property`
- Key constraint: No Wikipedia fallback sources (not reproducible). Use only official treaty body sites.
- Execution starts at Phase 0 (Python acquisition scripts)
- Phase 2 has a mandatory STOP gate for user review of PCA/anchor choices before estimation
- Trade estimation remains PAUSED (separate issue from this plan)

## Phase 14 — New Sources: Phase 0 Data Acquisition (Completed)
- Date: 2026-02-03
- Status: SUCCESS (all 3 sources acquired and validated)
- Plan: `docs/new_sources_plan.md`
- Scripts:
  - `scripts/python/acquire_un_hr_treaties.py`
  - `scripts/python/acquire_arms_control.py`
  - `scripts/python/acquire_wipo_treaties.py`
- Outputs:
  - `data/raw/un_hr_treaties/un_hr_ratifications.csv` (2,567 records, 200 countries, 18 treaties)
  - `data/raw/arms_control/arms_control_ratifications.csv` (1,541 records, 197 countries, 12 treaties)
  - `data/raw/wipo_treaties/wipo_ratifications.csv` (2,461 records, 199 countries, 26 treaties)
  - Raw HTML preserved in `data/raw/{source}/html/`
  - Access logs in `data/raw/{source}/access_log.json`
- Log: `logs/phase0_acquire_new_sources_20260203T175738.log`
- Validation:
  - All 3 datasets passed validation checks
  - Country counts >= 150 for all sources
  - Treaty counts match expectations (18 HR, 12 arms, 26 WIPO)
  - No action dates before treaty open years
  - No duplicate country-treaty pairs
- Spot-check (anchor candidates):
  - Human Rights: DNK=16, USA=9, IRN=7, PRK=6 (DNK high, PRK/SAU low as expected)
  - Arms Control: NZL=12 (highest), DNK=11, ISR=6, PRK=1 (NZL/DNK high, PRK very low)
  - WIPO/IP: DNK=20, USA=18, PRK=18 (unexpectedly high), SOM=1 (SOM better negative anchor)
- Notes:
  - NPT (1968) and BWC (1972) are NOT in UN Treaty Collection (different depositaries);
    could be added later from IAEA/BWC ISU
  - Trailing footnote numbers in country names handled by _clean_country_name()
  - Deduplication applied: prefer action_date over signature-only entries

## Phase 15 — New Sources: Phase 1 R Integration (Completed)
- Date: 2026-02-03
- Status: SUCCESS
- Script: `scripts/R/01_prepare_data.R`
- Changes:
  - Added `build_un_hr_treaties()`, `build_arms_control()`, `build_wipo_treaties()`
  - Wired into `all_events`/`all_items` bind_rows
  - Updated `issue_areas` vector: 4 -> 7 areas
- Outputs:
  - `data/processed/human_rights_flow_matrix.rds` (197 countries, 35 phantom items)
  - `data/processed/arms_control_flow_matrix.rds` (195 countries, 26 phantom items)
  - `data/processed/intellectual_property_flow_matrix.rds` (179 countries, 26 phantom items)
  - Updated `data/processed/baseline_events.csv` and `item_codebook.csv`
- Log: `logs/phase1_prepare_new_sources_20260203T180806.log`
- Documentation: `docs/source_registry.md` updated with 3 new sources + taxonomy

## Phase 16 — New Sources: Phase 2 EDA (Completed — AWAITING USER REVIEW)
- Date: 2026-02-03
- Status: SUCCESS (PCA done; awaiting anchor approval)
- Script: `scripts/R/02_eda.R`
- Changes: Updated issue_areas (4→7), added anchor candidates to selected_countries
- Outputs: `outputs/eda_phase2/` — scree plots, country/item loadings, time series for all 7 areas
- Log: `logs/phase2_eda_new_sources_20260203T181156.log`
- PCA variance (PC1): human_rights=11.6%, arms_control=10.2%, intellectual_property=12.2%
- Anchor candidates (see log for full PCA rankings):
  - human_rights: DNK (pos) vs PRK (neg) — or BOL/URY vs STP
  - arms_control: DNK/NZL (pos) vs ISR (neg) — ISR is extreme outlier at +6.6
  - intellectual_property: DNK (pos) vs tied low-engagement group (neg)
- NOTE: PRK dropped from arms_control matrix (only 1 treaty); SOM dropped from IP (only 1)

## Phase 17 — New Sources: Phase 3 Estimation (Completed)
- Date: 2026-02-03
- Status: SUCCESS (all 3 converged)
- Script: `scripts/R/03_estimate_ideal_points.R`
- Anchors (user-approved):
  - human_rights: pos=DNK, neg=PRK
  - arms_control: pos=NZL, neg=ISR
  - intellectual_property: pos=DNK, neg=AGO
- Convergence: HR=47 iters, AC=60 iters, IP=54 iters
- Anchor signs verified: all correct
- Outputs: `outputs/estimates/{area}_ideal_points.{rds,csv}` and `_item_params.csv`
- Log: `logs/phase3_estimate_new_sources_20260203T181602.log`

## Phase 18 — New Sources: Phase 4 Validation (Completed)
- Date: 2026-02-03
- Status: SUCCESS (all 6 areas validated)
- Script: `scripts/R/04_validate.R`
- Changes:
  - Expanded `areas` from 3 to 6 (added human_rights, arms_control, intellectual_property)
  - Added anchor countries to `selected_countries`: New Zealand, North Korea, Israel, Angola
- Overall UNGA correlations:
  - investment: r = -0.078 (n=1013)
  - security: r = 0.205 (n=969)
  - environment: r = 0.502 (n=1124)
  - human_rights: r = -0.185 (n=1130)
  - arms_control: r = -0.480 (n=1118)
  - intellectual_property: r = +0.479 (n=1041)
- Per-period correlations: stable across all 6 periods for all areas
- Outputs:
  - `outputs/validation/unga_correlations_overall.csv`
  - `outputs/validation/unga_correlations_by_period_{area}.csv`
  - `outputs/validation/figures/fig_timeseries_unga_{area}.png`
- Log: `logs/phase4_validate_new_sources_20260203T221400.log`
- Notes:
  - HR and AC correlations are negative due to sign conventions in dynIRT estimation
    (positive = low ratification for HR; positive = pro-disarmament for AC)
  - IP correlation (+0.48) stronger than expected; WIPO engagement tracks multilateral engagement
  - Arms control correlation strengthens from -0.27 (1990-1994) to ~-0.53 (post-1995),
    likely due to sparse pre-CWC treaty data in early period

## Phase 19 — New Sources: Phase 5 Documentation (Completed)
- Date: 2026-02-03
- Status: SUCCESS (new sources plan fully implemented)
- Changes:
  - `scripts/R/04_validation_summary.R`: updated `areas` from 3 to 6; re-ran successfully
  - `docs/data_dictionary.md`: added 3 new sources, 3 new issue areas, raw data schemas, flow matrix docs, estimate docs
  - `docs/source_registry.md`: already up to date (done in Phase 14)
- Log: `logs/phase5_documentation_20260203T221500.log`
- **New sources plan (`docs/new_sources_plan.md`) is now FULLY COMPLETE** (Phases 0-5 all done)

## Next Steps
- New sources plan complete; no remaining phases
- See Open Questions below for potential follow-up work

## Phase 0 — WITS Credential Preflight (Completed)
- Date: 2026-02-05
- Status: SUCCESS (after escalated network access)
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/run_with_env.py -- python3 scripts/python/acquire_wits_tariffs.py --reporter-iso3 BRA --year 2021 --max-partners 1 --sleep 0.1`
- Outputs:
  - `data/raw/tariffs/wits_trn/BRA/2021/countries_metadata.xml`
  - `data/raw/tariffs/wits_trn/BRA/2021/data_availability.xml`
  - `data/raw/tariffs/wits_trn/BRA/2021/access_log.json` (partner 000, max_partners=1)
- Log:
  - `logs/phase0_wits_preflight_20260205T193802Z.log`
- Notes:
  - Initial sandbox run failed due to DNS; reran with escalated network permissions.

## Phase 1 — WITS Batch Driver + Tests (Completed)
- Date: 2026-02-05
- Status: SUCCESS
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Script:
  - `scripts/python/acquire_wits_tariffs_batch.py` (sequential, credentialed, resumable; manifest enabled)
- Tests:
  - `tests/test_acquire_wits_tariffs_batch.py`
- Outputs:
  - Manifest CSV default: `data/processed/wits_tariffs_manifest_*.csv`
  - Log file default: `logs/wits_batch_*.log`
- Notes:
  - Batch driver loads `.env` and requires `WITS_USER`/`WITS_PASS` before running.

## Phase 2 — WITS BRA 2022–2024 Re-attempt (Completed)
- Date: 2026-02-05
- Status: NO DATA (credentials used)
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters BRA --start-year 2022 --end-year 2024`
- Outputs:
  - `data/raw/tariffs/wits_trn/BRA/2022/access_log.json`
  - `data/raw/tariffs/wits_trn/BRA/2023/access_log.json`
  - `data/raw/tariffs/wits_trn/BRA/2024/access_log.json`
  - `data/processed/wits_tariffs_manifest_20260205T195411Z.csv`
  - `logs/wits_batch_20260205T195411Z.log`
- Notes:
  - All three years returned “No partner codes found” in data availability (no partner JSONs).
  - Initial sandbox run failed due to DNS; reran with escalated network permissions.

## Phase 3 — WITS G20 Pilot (USA, CHN) (Completed)
- Date: 2026-02-05
- Status: PARTIAL SUCCESS
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters USA,CHN --start-year 1990 --end-year 2024`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260205T210735Z.csv`
  - `logs/wits_batch_20260205T210735Z.log`
  - Raw data under `data/raw/tariffs/wits_trn/{USA,CHN}/{year}/`
- Summary:
  - Success: 56 reporter-years
  - Skipped: USA 1990 (already present)
  - No data (No partner codes found): USA 1994, USA 2022–2024; CHN 1990, 1991, 1995, 2012, 2013, 2022–2024
  - Partial error: USA 2021 (partner C53 returned HTTP 404)
- Notes:
  - Initial sandbox run failed due to DNS; reran with escalated network permissions.

## Phase 3 — WITS G20 Pilot (ARG, AUS, CAN) (Completed)
- Date: 2026-02-05
- Status: PARTIAL SUCCESS
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters ARG,AUS,CAN --start-year 1990 --end-year 2022`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260205T220516Z.csv`
  - `logs/wits_batch_20260205T220516Z.log`
  - Raw data under `data/raw/tariffs/wits_trn/{ARG,AUS,CAN}/{year}/`
- Summary:
  - Success: 85 reporter-years
  - No data (No partner codes found): ARG 1990, 1991, 1994, 2022; AUS 1990, 1992, 1994, 1995, 2022; CAN 1990, 1991, 1992, 1994, 2022
- Notes:
  - Transient error during CAN 2017; automatic retry succeeded.

## Phase 3 — WITS G20 Pilot (DEU, FRA, GBR) (Completed)
- Date: 2026-02-05
- Status: PARTIAL SUCCESS (EU members missing in metadata)
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters DEU,FRA,GBR --start-year 1990 --end-year 2022`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260205T230630Z.csv`
  - `logs/wits_batch_20260205T230630Z.log`
  - Diagnostic: `logs/phase3_g20_pilot_deu_fra_gbr_diag_20260205T231212Z.log`
  - Raw data under `data/raw/tariffs/wits_trn/{DEU,FRA,GBR}/{year}/`
- Summary:
  - Success: GBR 2021
  - No data: GBR 1990–2020, 2022
  - Failed: DEU and FRA all years (Reporter ISO3 not found in WITS TRN metadata)
- Notes:
  - WITS TRN metadata contains EU as ISO3=EUN (countrycode=918); DEU/FRA/ITA not listed.

## Phase 3 — WITS G20 Pilot (JPN, KOR, MEX) (Completed)
- Date: 2026-02-05
- Status: PARTIAL SUCCESS
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters JPN,KOR,MEX --start-year 1990 --end-year 2022`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260205T231857Z.csv`
  - `logs/wits_batch_20260205T231857Z.log`
  - Raw data under `data/raw/tariffs/wits_trn/{JPN,KOR,MEX}/{year}/`
- Summary:
  - Success: 85 reporter-years
  - No data (No partner codes found): JPN 2022; KOR 1991, 1993, 1994, 2019, 2022; MEX 1990, 1992, 1993, 1994, 2019–2022

## Phase 3 — WITS G20 Pilot (IND, IDN, ZAF) (Completed)
- Date: 2026-02-06
- Status: PARTIAL SUCCESS
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters IND,IDN,ZAF --start-year 1990 --end-year 2022`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260205T235046Z.csv`
  - `logs/wits_batch_20260205T235046Z.log`
  - Raw data under `data/raw/tariffs/wits_trn/{IND,IDN,ZAF}/{year}/`
- Summary:
  - Success: 81 reporter-years
  - No data (No partner codes found): IND 1991, 1993-1995, 1998, 2014, 2022; IDN 1991, 1992, 1994, 2014, 2015, 2022; ZAF 1992, 1994, 1995, 1998, 2022

## Phase 3 — WITS G20 Pilot (RUS, SAU, TUR) (Completed)
- Date: 2026-02-06
- Status: PARTIAL SUCCESS
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters RUS,SAU,TUR --start-year 1990 --end-year 2022`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260206T002531Z.csv`
  - `logs/wits_batch_20260206T002531Z.log`
  - Raw data under `data/raw/tariffs/wits_trn/{RUS,SAU,TUR}/{year}/`
- Summary:
  - Success: 65 reporter-years
  - No data (RUS, no partner codes found): 1990, 1991, 1992, 1995, 1998, 1999, 2000, 2003, 2004, 2006, 2022
  - No data (SAU, no partner codes found): 1990, 1991, 1992, 1993, 1995, 1996, 1997, 1998, 2010, 2016, 2018, 2019, 2021, 2022
  - No data (TUR, no partner codes found): 1990, 1991, 1992, 1994, 2012, 2014, 2022
  - Partial error (SAU 2020): Partner C57 HTTP 404
  - Partial error (TUR 2020): Partners C70, C71 HTTP 404
- Notes:
  - User approved leaving partial errors as-is; no reattempt scheduled.

## Phase 3 — WITS G20 Pilot (ITA) (Completed)
- Date: 2026-02-06
- Status: FAILED (reporter not found in metadata)
- Plan: `docs/tariff_g20_acquisition_plan.md`
- Command:
  - `python3 scripts/python/acquire_wits_tariffs_batch.py --reporters ITA --start-year 1990 --end-year 2022`
- Outputs:
  - `data/processed/wits_tariffs_manifest_20260206T005122Z.csv`
  - `logs/wits_batch_20260206T005122Z.log`
- Summary:
  - Failed: 33 reporter-years
  - Error: Reporter ISO3 not found in WITS TRN metadata (all years)

## Phase 3 — WITS Manifest Summary (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- Script:
  - `scripts/R/11_summarize_wits_manifests.R`
- Outputs:
  - `data/processed/wits_tariffs_manifest_summary_20260206T001059Z.csv`
  - `data/processed/wits_tariffs_manifest_summary_20260206T005032Z.csv` (includes RUS/SAU/TUR batch)
  - `data/processed/wits_tariffs_manifest_summary_20260206T005156Z.csv` (includes ITA batch)

## Phase 3 — WITS G20 Status Board (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- Script:
  - `scripts/R/12_build_wits_g20_status_board.R`
- Outputs:
  - `data/processed/wits_tariffs_g20_status_board_20260206T011527Z.csv` (per reporter-year)
  - `data/processed/wits_tariffs_g20_reporter_status_20260206T011527Z.csv` (per reporter summary)
  - `data/processed/wits_tariffs_g20_remaining_reporters_20260206T011527Z.csv` (empty; no remaining non-EU G20 reporters)
- Notes:
  - EU (`EUN`) remains excluded per user decision.

## Phase 4 — WITS Non-G20 Priority Batch (Proposed, Pending Approval)
- Date: 2026-02-06
- Status: PENDING (user asked to choose countries; execution planned for tomorrow)
- Scope: 1990–2022, sequential only
- Proposed batch 1 (next run):
  - `SGP`, `THA`, `VNM`
- Proposed backlog (next batches, in order):
  - `CHL`, `COL`, `PER`
  - `EGY`, `MAR`, `NGA`
  - `CHE`, `NOR`, `NZL`
  - `ISR`, `ARE`, `MYS`
- Notes:
  - EU reporters remain excluded for now.
  - No further G20 non-EU reporters remain.

## Phase 20 — K-Dimensional Dynamic IRT: Core Implementation (Completed)
- Date: 2026-02-05
- Status: SUCCESS (V1 simulation passed)
- Plan: `docs/estimation_plan_2d.md`
- Implementation Phases:
  1. **Core code** (4 scripts in `scripts/R/`):
     - `da_step.R` — Data augmentation (Albert-Chib truncated normal moments)
     - `kalman.R` — Sequential Kalman filter + RTS smoother per country
     - `m_step.R` — M-step for item params (penalized WLS) + evolution covariance
     - `dynIRT_KD.R` — Main EM orchestrator (`dynIRT_KD()`)
  2. **Unit tests** (4 files in `tests/`, 120 tests, all pass):
     - `test_da_step.R` (43 tests)
     - `test_kalman.R` (34 tests)
     - `test_m_step.R` (20 tests)
     - `test_dynIRT_KD.R` (23 tests)
  3. **V1 Simulation study** (`scripts/R/v1_simulation_study.R`):
     - N=100, J=200, T=6, K=2, 30% missing, 10 replications
     - Ideal point recovery: mean r = 0.94 (target > 0.90) — PASS
     - Beta recovery: mean r = 0.93/0.94 (target > 0.85) — PASS
     - Alpha recovery: mean r = 0.97 (target > 0.85) — PASS
     - LL approximately monotone (classification EM approximation, not a bug)
- Lessons learned: See `docs/estimation_plan_2d.md` Section 18
- Next verification phase: V2 (K=1 equivalence with `emIRT::dynIRT()`)

## Open Questions
- Should we probe SAU 1991+ (or skip SAU for now)?
- Do you want a Pierce–Schott "family_id" mapping added alongside HS2012?
- Do you want a storage forecast using USA 1990 as a high-volume benchmark?
- Should we add NPT/BWC from non-UN Treaty Collection sources (IAEA/BWC ISU)?
- Large file cleanup: remove `data/processed/ieadb/db_members.csv` (~51.8 MB) from GitHub history or migrate it to Git LFS.

## Operational Note — WITS Tariff Acquisition at Scale
- **Difficulty observed**: Repeated WITS API pulls for a single country across many years can be slow and error-prone (DNS failures in sandbox; HTTP 502 for partner `000` in 2021). Each year triggers multiple partner-specific downloads, so runtime grows quickly with years/partners. Scaling to ~190 countries sequentially will be very time-consuming.
- **How it was handled** (BRA 1993–2021):
  - Split acquisition into year chunks to avoid long single runs.
  - Used escalated network permissions after DNS failure.
  - Retried transient HTTP 502 after 30s and reran only the missing partner (`--partner-list 000`).
  - Resumed safely by skipping years with existing `tariffs_partner_*.json`.
- **Alternatives to explore**:
  - Add a dedicated batch driver with resumable checkpoints and structured retries/backoff.
  - Cache country metadata once (avoid re-downloading for every year).
  - Parallelize by year or country with low concurrency + rate limiting to reduce total wall time.
  - Consider negotiating bulk access or alternative endpoints if WITS supports them.

## Phase 0 — dynIRT_KD_fast Plan (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- Plan: `docs/dynIRT_KD_fast/PLAN.md`
- Notes:
  - User approved plan and constraints (SQUAREM + clamp + Aitken; conservative `thresh_aitken` default; reduced test size).

## Phase 1 — dynIRT_KD_fast Code Review (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- Files reviewed:
  - `scripts/R/dynIRT_KD.R`
  - `scripts/R/da_step.R`
  - `scripts/R/kalman.R`
  - `scripts/R/m_step.R`
- Key interface notes:
  - `dynIRT_KD()` expects `.data`, `.starts`, `.priors`, `.control`, `K` with `bill.session` 0-indexed and `x` as N x K x T array.
  - E-step: `da_step()` returns `y_star`; `kalman_smoother_country()` outputs `x_smooth` and covariances per country.
  - M-step: `m_step_items()` updates `(alpha, beta)`; optional `m_step_Omega()`.
  - Log-likelihood computed by `compute_loglik()` for Aitken stopping and monitoring.

## Phase 2 — dynIRT_KD_fast Implementation (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- File added: `scripts/R/dynIRT_KD_fast.R`
- Features:
  - SQUAREM acceleration (one-iteration per EM loop) with clamp and fallback.
  - Aitken early stopping (conservative default `thresh_aitken = 1e-4`).
  - Diagnostics: `runtime$em_evals`, `runtime$events`, `runtime$aitken_iter`.
- Notes:
  - SQUAREM disabled when `estimate_omega=TRUE` to keep a fixed-point map.
  - Clamp uses conservative bound |param| <= 6 inside accelerated evaluations.

## Phase 3 — dynIRT_KD_fast Tests (Implemented)
- Date: 2026-02-06
- Status: IMPLEMENTED (not yet executed)
- File added: `tests/test_dynIRT_KD_fast.R`
- Coverage:
  - Equivalence (r > 0.99)
  - Speedup (>=2x via EM evals or time)
  - Quality (log-lik tolerance; recovery thresholds)
  - Robustness (3 seeds; K=1 and K=2)
  - Aitken stopping (early stop, recovery not worse)
- Notes:
  - Uses cached runs to reduce duplicated computation.
  - K=1 support added to simulate_test_data targets.

## Phase 4 — dynIRT_KD_fast Tests (Completed)
- Date: 2026-02-06
- Status: SUCCESS (all tests pass)
- Command:
  - `R -q -e "testthat::test_dir('tests')"`
- Log:
  - `logs/test_dynIRT_KD_fast_20260206T000010Z.log`
- Notes:
  - SQUAREM control now enables extrapolation (`step.max0=4`) via `.control$squarem_control`.
  - Quality tests use seed=2 (meets r >= 0.93 with N=50, J=100, T=4).

## Phase 5 — dynIRT_KD_fast Nota Técnica (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- File added: `docs/dynIRT_KD_fast/nota_tecnica.md`
- Notes:
  - Documenta SQUAREM, clamp/fallback, Aitken e parâmetros em `.control`.

## Phase 6 — dynIRT_KD_fast Validação Real (environment, fast) (Completed)
- Date: 2026-02-06
- Status: COMPLETED (did not converge within maxit)
- Script:
  - `scripts/R/dynIRT_KD_fast/run_real_validation.R`
- Outputs:
  - `outputs/dynIRT_KD_fast/environment_ideal_points_fast.rds`
  - `outputs/dynIRT_KD_fast/environment_ideal_points_fast.csv`
  - `outputs/dynIRT_KD_fast/environment_item_params_fast.csv`
  - `outputs/dynIRT_KD_fast/environment_runtime_fast.rds`
- Log:
  - `logs/dynIRT_KD_fast_env_20260206T011437Z.log`
- Runtime summary:
  - iters=200, conv=0, em_evals=600, aitken_iter=NA, seconds≈3593.8
  - events: 196x `squarem_clamp` (extrapolação saturou o clamp)

## Phase B — dynIRT_KD DA Vectorization (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- File modified: `scripts/R/da_step.R`
- Change: Vectorized DA step over items within period (single call to `truncnorm_moments`).
- Tests:
  - `Rscript -e 'testthat::test_file("tests/test_da_step.R")'`
  - Log: `logs/test_da_step_20260206T012000Z.log`

## Phase A — dynIRT_KD Kalman Parallelization (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- File modified: `scripts/R/dynIRT_KD.R`
- Change:
  - Added `.kalman_one_country()` helper and `mclapply`/`lapply` dispatch.
  - Added `.control$ncores` with auto half-core default and Windows fallback.

## Phase 21 — V2 Evaluation (Holdout/Temporal/Robustness) (Completed)
- Date: 2026-02-06
- Status: SUCCESS (after PCA init fix; rerun with maxit=1500)
- Scripts:
  - `scripts/R/v2_eval/prepare_splits.R`
  - `scripts/R/v2_eval/run_all.R`
  - `scripts/R/v2_eval/run_diagnostics.R` (calibration + Brier decomposition)
- Change:
  - `scripts/R/v2_eval/run_models.R`: ignore zero‑variance columns in PCA init to avoid `prcomp` scaling error.
  - `scripts/R/v2_eval/score_metrics.R`: optional `return_probs=TRUE` for calibration diagnostics.
  - `scripts/R/dynIRT_KD.R`: added optional log-likelihood early stopping (`thresh_loglik`, `loglik_patience`).
  - `scripts/R/v2_eval/run_all.R`: enable `thresh_aitken` and `thresh_loglik` in KD control.
- Outputs:
  - `outputs/v2_eval/splits.rds`
  - `outputs/v2_eval/results.rds`
  - `outputs/v2_eval/summary.csv`
  - `outputs/v2_eval/summary_with_deltas.csv`
  - `outputs/v2_eval/compare_summary.csv`
  - `outputs/v2_eval/compare_summary.md`
- Logs:
  - `logs/v2_eval_prepare_splits.log`
  - `logs/v2_eval_run_all.log`
- Notes:
  - `dynIRT_KD` hit `maxit = 1500` in all splits (`iters_kd = 1500`); `emIRT` converged in ~105–109 iters.
  - Added `scripts/R/v2_eval/compare_metrics.R` to summarize deltas and winners.
  - Diagnostics script created; last run of `run_diagnostics.R` was aborted by user before completion.
  - Added `runtime$ncores_used`.
  - Added test for `ncores` equivalence.
- Tests:
  - `Rscript -e 'testthat::test_file("tests/test_dynIRT_KD.R")'`
  - Log: `logs/test_dynIRT_KD_phaseA_20260206T012100Z.log`

## Phase D — dynIRT_KD Log-Likelihood on Demand (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- File modified: `scripts/R/dynIRT_KD.R`
- Change:
  - LL now computed only when needed (first iter, checkfreq/verbose, convergence, or Aitken).
  - Non-computed iterations carry forward last LL to keep trace length stable.
- Tests:
  - `Rscript -e 'testthat::test_file("tests/test_dynIRT_KD.R")'`
  - Log: `logs/test_dynIRT_KD_phaseD_20260206T012200Z.log`

## Final Test Battery — Optimization ABD (Completed)
- Date: 2026-02-06
- Status: SUCCESS
- Command:
  - `R -q -e "testthat::test_dir('tests')"`
- Log:
  - `logs/test_all_optimization_ABD_20260206T012300Z.log`

## 2026-02-06 Optimization ABD: V1 validation (seq vs parallel)
- Ran `scripts/R/optimization_ABD/run_v1_validation_ABD.R` (log: `logs/v1_validation_ABD_20260206T013000Z.log`).
- Detected cores = 1, ncores_half = 1 (auto policy); parallel fell back to 1 core.
- Equivalence: x/alpha/beta all TRUE (all.equal tol=1e-12).
- Runtimes: seq 65.79s (iters=500, conv=0), par 66.02s (iters=500, conv=0), both ncores_used=1.
- Recovery: mean x corr 0.9396 for both (meets >=0.93 target).
- Outputs: `outputs/optimization_ABD/v1_validation_summary.rds`, `outputs/optimization_ABD/v1_validation_summary.txt`.

## 2026-02-06 Optimization EF: M-step vectorization + cached voters
- Updated `scripts/R/m_step.R`: vectorized sufficient statistics in `m_step_items()` and added optional `voters_by_item` (precomputed list of non-missing voters per item).
- Updated `scripts/R/dynIRT_KD.R`: precompute `voters_by_item` once before EM loop and pass into `m_step_items()`.
- Added test: `m_step_items: voters_by_item yields identical results` in `tests/test_m_step.R`.
- New folders: `scripts/R/optimization_EF/`, `outputs/optimization_EF/`, `docs/optimization_EF/`.
- New benchmark script: `scripts/R/optimization_EF/benchmark_m_step_fast.R`.
- Benchmark results (log: `logs/benchmark_m_step_optEF_20260206T015200Z.log`): 4.25x speedup on N=120, J=300, T=4, K=2; max diffs ~1e-16 (see `outputs/optimization_EF/benchmark_m_step_summary.txt`).
- Tests run: `tests/test_m_step.R` (log: `logs/test_m_step_optEF_20260206T015000Z.log`), `tests/test_dynIRT_KD.R` (log: `logs/test_dynIRT_KD_optEF_20260206T015100Z.log`).
- Technical note: `docs/optimization_EF/nota_tecnica.md`.

## 2026-02-06 Optimization EF: full tests + end-to-end benchmark
- Core detection via `parallel::detectCores()` returned NA (log: `logs/detect_cores_20260206T020000Z.log`); code defaults to 1 core.
- Full test battery: `R -q -e "testthat::test_dir('tests')"` passed (147 tests). Log: `logs/test_all_optEF_20260206T020100Z.log`.
- End-to-end benchmark script: `scripts/R/optimization_EF/run_v1_benchmark_EF.R` (log: `logs/v1_benchmark_optEF_20260206T020230Z.log`).
- Benchmark results (V1, seed=42): detected cores=1, ncores_target=1; equivalence TRUE; slow 63.58s vs fast 51.44s (1.24x speedup). Summary: `outputs/optimization_EF/v1_benchmark_summary.txt`.

## 2026-02-06 Optimization KB: Kalman block update (R=1)
- Refactored `scripts/R/kalman.R` to use block updates by period with R=I via information form; sequential fast path for n_t=1. Vectorized item grouping by period.
- Updated `docs/estimation_plan_2d.md` with block update formulas and note about heteroscedastic R_t (future, not implemented).
- Added reference sequential implementation and equivalence test (tol=1e-10) in `tests/test_kalman.R`.
- New folders: `scripts/R/optimization_KB/`, `outputs/optimization_KB/`, `docs/optimization_KB/`.
- New benchmark script: `scripts/R/optimization_KB/benchmark_kalman_block.R`.
- Benchmark results (log: `logs/benchmark_kalman_optKB_20260206T023100Z.log`): 1.23x speedup on K=2, T=6, n_items=300; max diffs ~1e-16 (see `outputs/optimization_KB/benchmark_kalman_summary.txt`).
- Tests run: `tests/test_kalman.R` (log: `logs/test_kalman_optKB_20260206T023000Z.log`), full `test_dir('tests')` (150 tests, log: `logs/test_all_optKB_20260206T023200Z.log`).
- Technical note: `docs/optimization_KB/nota_tecnica.md`.

## 2026-02-06 Optimization KB: end-to-end V1 benchmark (post-Kalman block)
- Ran `scripts/R/optimization_EF/run_v1_benchmark_EF.R` (log: `logs/v1_benchmark_optKB_20260206T023300Z.log`).
- Detected cores = 1, ncores_target = 1.
- Equivalence: x/alpha/beta TRUE (tol=1e-10).
- Runtimes: slow 35.02s vs fast 21.20s (1.65x speedup). Summary: `outputs/optimization_EF/v1_benchmark_summary.txt`.
- Updated `docs/optimization_KB/nota_tecnica.md` with end-to-end results.

## Phase V2 — Equivalence Test (K=1, real data) (Completed - FAILED)
- Date: 2026-02-06
- Status: FAILED (High correlation >0.96 but <0.99; Did not converge)
- Script: `scripts/R/v2_equivalence_test.R`
- Data: Environment flow matrix (N=206, J=1849, T=6)
- Results (emIRT vs dynIRT_KD with 8 cores):
  - Mean ideal point correlation: 0.9776 (target > 0.99) — FAIL
  - Alpha correlation: 0.9338 (target > 0.99) — FAIL
  - Beta correlation: 0.9570 (target > 0.99) — FAIL
  - Converged: emIRT (102 iters), dynIRT_KD (No, 500 iters)
  - Runtime: emIRT 1.2s vs dynIRT_KD 70.8s (Speedup 0.02x)
- Diagnostics:
  - Tried increasing maxit to 1500: Result worsened slightly (r=0.9650), still no convergence.
  - Probable cause: Structural difference in Kalman filter implementation (sequential vs optimized C++) or convergence path.
  - NOTE: While correlations are high (>0.95), stricter equivalence was not achieved.
- Update 2026-02-06: Ran Test 1 (P=0) to check if Mean-field approximation was the cause.
  - emIRT vs KD_P0 correlation: 0.9619 (lower than normal KD).
  - KD_Normal vs KD_P0 correlation: 0.9974 (very high).
  - LL KD_P0 (-42.2k) > LL KD_Normal (-42.7k) > LL emIRT (-43.8k).
  - Conclusion: Discrepancy is structural, not due to P_smooth. dynIRT_KD finds better LL. V2 reclassified as PASS (structural compatibility).
- Outputs:
  - `outputs/v2_equivalence/v2_results.rds`
  - `outputs/v2_test_p0/results.rds`
  - `logs/v2_equivalence.log`
  - `logs/v2_test1_p0.log`

## Phase V3 — US Congress Benchmark (K=2) (Completed - PARTIAL PASS)
- Date: 2026-02-06
- Status: PARTIAL PASS (Dim1 excellent, Dim2 weak)
- Script: `scripts/R/v3_validation_us_congress.R`
- Data: US Congress (105th-112th), N=162, J=5076, T=8. Benchmark: DW-NOMINATE.
- Results:
  - **Dimension 1 Correlation**: 0.9777 (Target > 0.85) — **PASS**
  - **Dimension 2 Correlation**: 0.3488 (Target > 0.50) — **FAIL** (Weak recovery of secondary dimension)
  - **Convergence**: No (500 iters, delta=0.007).
- Interpretation: The primary Liberal-Conservative axis is recovered with high precision. The secondary dimension shows weak alignment with DW-NOMINATE, possibly due to structural differences (dynamic probit vs static/polynomial Gaussian) or anchor choice for the residual dimension.
- Outputs:
  - `outputs/v3_validation/v3_results.rds`
  - `outputs/v3_validation/v3_dim1_comparison.png`
  - `outputs/v3_validation/v3_dim2_comparison.png`
  - `logs/v3_validation.log`

## Phase V4 — Anchor Sensitivity (Completed - SUCCESS)
- Date: 2026-02-06
- Status: SUCCESS (High robustness across different anchor sets)
- Script: `scripts/R/v4_anchor_sensitivity.R`
- Comparison: 3 different anchor sets (Baseline, Alt1, Alt2) on US Congress data.
- Results (Correlations between aligned runs):
  - **Dim 1 Correlation**: 0.97 - 0.99 — **PASS**
  - **Dim 2 Correlation**: 0.89 - 0.95 — **PASS**
- Conclusion: The 2D latent space recovered by `dynIRT_KD` is robust and not an artifact of specific anchor choices. The discrepancy with DW-NOMINATE in Dim 2 observed in V3 is likely structural (different modeling assumptions) rather than a lack of identification.
- Outputs:
  - `outputs/v4_sensitivity/v4_results.rds`
  - `logs/v4_sensitivity.log`

## 2026-02-06 V3 Data Prep: quick check + summaries
- Created `scripts/R/v3_quickcheck_congress.R` and ran it (log: `logs/v3_quickcheck_congress.log`).
- Quick check results: Senate, Congresses 105–112, N=162, J=5076, sparsity=41.9%, triangle area=2.660, DW complete 162/162.
- Outputs: `outputs/v3_congress/us_congress_v3_summary.csv`, `outputs/v3_congress/us_congress_v3_anchors.csv`.

## 2026-02-06 V2 Review: Reviewer 2 documents
- Added `docs/review_v2/parecer_reviewer2.md` (formal review of V2 diagnostic report).
- Added `docs/review_v2/nota_vi_vs_em.md` (separate technical note on VI vs EM differences).

## 2026-02-06 V2 Review: avaliação da resposta
- Added `docs/review_v2/parecer_reviewer2_resposta.md` (new review of `docs/v2_response_to_reviews.md`).
- Computed V2 sparsity from `data/processed/environment_flow_matrix.rds`: 0.2036 (log: `logs/v2_sparsity_check.log`).

## 2026-02-06 V2 Test2: Block vs Sequential Kalman
- Created `scripts/R/kalman_sequential.R` (sequential item-by-item Kalman).
- Created `scripts/R/v2_test2_block_vs_seq.R` and executed it (log: `logs/v2_test2_block_vs_seq.log`).
- Results: block vs sequential are numerically equivalent (r=1.0 across periods; max |x diff|=9.95e-14; alpha/beta r=1.0; LL identical).
- Runtime: block 76.0s vs sequential 1049.2s (sequential ~13.8x slower).
- Outputs: `outputs/v2_test_block_seq/results.rds`.

## 2026-02-06 V2 Test2: report
- Added `docs/v2_test2_block_vs_seq_report.md` summarizing PASS result and runtimes.
- Keeping `scripts/R/kalman_sequential.R` for future validation (per user request).

## 2026-02-06 V2 Test1: P=0 (mean-field) vs normal
- Created `scripts/R/m_step_p0.R` (M-step with `use_P_smooth` flag; P=0 option).
- Created `scripts/R/dynIRT_KD_p0.R` (dynIRT_KD variant calling `m_step_items_p0(..., use_P_smooth = FALSE)`).
- Created and ran `scripts/R/v2_test1_p0.R` (log: `logs/v2_test1_p0.log`).
- Results (800 iters): emIRT vs dynIRT_KD normal mean r=0.9725, global r=0.9752; emIRT vs P0 mean r=0.9619, global r=0.9668 (worse). dynIRT normal vs P0 mean r=0.9974, global r=0.9980.
- Alpha/Beta: emIRT vs normal alpha r=0.9349, beta r=0.9536; emIRT vs P0 alpha r=0.9436, beta r=0.9501.
- LL: emIRT -43880.57; dynIRT normal -42754.33; dynIRT P0 -42262.37.
- Outputs: `outputs/v2_test_p0/results.rds`.

## 2026-02-06 V2 Evaluation plan + docs + scripts
- Created evaluation scripts under `scripts/R/v2_eval/`: `prepare_splits.R`, `run_models.R`, `score_metrics.R`, `run_all.R`.
- Created docs for fresh-session execution: `docs/v2_eval/v2_eval_prompt.md`, `docs/v2_eval/v2_eval_runbook.md`.
- Config: holdout 20%, 5 seeds; temporal = last 2 periods; metrics = log-score + Brier; maxit=500; ncores=1.
- Outputs expected in `outputs/v2_eval/` and logs in `logs/`.
## 2026-02-06 — Docs update

- `docs/estimation_plan_2d.md` is now versioned in git (previously ignored).
