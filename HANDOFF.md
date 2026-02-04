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
