# Data Dictionary: country_year_long

File:
- `data/processed/country_year_long.csv`
- `data/processed/country_year_long.rds`
- `data/processed/country_year_long_deduped.csv`
- `data/processed/country_year_long_deduped.rds`

Unit of analysis:
- Country-year-event (one row per country, year, and event/treaty/organization observation).

Columns:
- `source`: Data source (DESTA, ATOP, IEADB, WTO RTA, UNCTAD IIA).
- `issue_area`: Issue area label (trade, investment, security, environment).
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
