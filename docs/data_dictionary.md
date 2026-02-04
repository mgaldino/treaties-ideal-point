# Data Dictionary: country_year_long

File:
- `data/processed/country_year_long.csv`
- `data/processed/country_year_long.rds`
- `data/processed/country_year_long_deduped.csv`
- `data/processed/country_year_long_deduped.rds`

Unit of analysis:
- Country-year-event (one row per country, year, and event/treaty/organization observation).

Columns:
- `source`: Data source (DESTA, ATOP, IEADB, WTO RTA, UNCTAD IIA, UN HR Treaties, Arms Control, WIPO Treaties).
- `issue_area`: Issue area label (trade, investment, security, environment, human_rights, arms_control, intellectual_property).
- `iso3`: ISO3 country code (countries only; REIOs excluded in processed data).
- `country_name`: Country name (best-effort mapping from ISO3 or source names).
- `year`: Event year (integer), restricted to >= 1990.
- `event_type`: Event category (e.g., signature, entry_into_force, ratification_accession, alliance_*).
- `event_date`: Event date if available (string; may be NA).
- `org_or_treaty_id`: Source-specific treaty or organization identifier.
- `org_or_treaty_name`: Source-specific treaty or organization name/title.
- `status`: Source-specific status (e.g., treaty status or action type).
- `treaty_type`: Source-specific treaty or agreement type (or alliance type for ATOP).
- `source_url`: URL when available (may be NA).

Exclusions:
- REIOs / non-country entities are excluded from this processed dataset.
- Full lists are preserved in:
  - `data/processed/excluded_entities_wto_rta.csv`
  - `data/processed/excluded_entities_unctad_iia.csv`

Notes:
- DESTA does not provide explicit ratification dates; entry-into-force is used as the closest activation marker.
- UNCTAD IIA does not provide explicit ratification dates; entry-into-force is used as the closest activation marker.
- The deduped files remove overlap between DESTA and WTO RTA by matching treaty names and dropping DESTA rows for matched treaties (see `data/processed/trade_dedup_matches.csv` and `data/processed/trade_dedup_summary.csv`).

---

# Raw data: New issue-area sources (acquired 2026-02-03)

## `data/raw/un_hr_treaties/un_hr_ratifications.csv`
- Unit: country-treaty observation (one row per country per HR instrument)
- Columns: `treaty_id`, `treaty_name`, `country`, `signature_date`, `action_type`, `action_date`
- Coverage: 18 instruments (9 core UN HR treaties + optional protocols), ~200 countries
- Source: treaties.un.org Chapter IV

## `data/raw/arms_control/arms_control_ratifications.csv`
- Unit: country-treaty observation
- Columns: `treaty_id`, `treaty_name`, `country`, `signature_date`, `action_type`, `action_date`
- Coverage: 12 instruments from Chapter XXVI, ~197 countries
- Source: treaties.un.org Chapter XXVI

## `data/raw/wipo_treaties/wipo_ratifications.csv`
- Unit: country-treaty observation
- Columns: `treaty_id`, `treaty_name`, `country`, `signature_date`, `instrument_type`, `action_date`, `entry_into_force_date`
- Coverage: 26 WIPO-administered treaties, ~199 countries
- Source: www.wipo.int/wipolex

---

# Processed data: Flow matrices (new issue areas)

## `data/processed/human_rights_flow_matrix.rds`
- Dimensions: 197 countries x 35 phantom items (18 treaties x ~6 periods, minus closed items)
- Values: -1 (non-participant), 0 (not yet open), +1 (ratified/acceded)

## `data/processed/arms_control_flow_matrix.rds`
- Dimensions: 195 countries x 26 phantom items
- Values: -1 (non-participant), 0 (not yet open), +1 (ratified/acceded)

## `data/processed/intellectual_property_flow_matrix.rds`
- Dimensions: 179 countries x 26 phantom items
- Values: -1 (non-participant), 0 (not yet open), +1 (ratified/acceded)

---

# Estimates (new issue areas)

## `outputs/estimates/{human_rights,arms_control,intellectual_property}_ideal_points.csv`
- Unit: country-period
- Columns: `country_iso3`, `period`, `ideal_point`
- Periods: 1990-1994, 1995-1999, 2000-2004, 2005-2009, 2010-2014, 2015-2018

## `outputs/estimates/{human_rights,arms_control,intellectual_property}_item_params.csv`
- Unit: phantom item
- Columns: `item`, `alpha`, `beta`
- alpha = discrimination parameter, beta = difficulty parameter
