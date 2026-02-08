# Ideal Points from IO Memberships and Treaty Participation (1990–2025)

## Project purpose
Build a reproducible dataset and analysis pipeline to estimate country ideal points using participation in international organizations (IOs) and treaty accession/signing, **not** UNGA/UNSC voting. UN-affiliated bodies (e.g., WIPO, ILO) are included.

## Tariff task objective (this task)
Construct a clean dataset of **MFN tariffs** and **effectively applied tariffs** at the importer–product–partner–year level, and compute **preferential margins**, to support latent-variable modeling of trade openness. The conceptual goal is to distinguish:
- general (MFN-based, non-discriminatory) openness, and
- discriminatory openness via preferential trade agreements (PTAs).

Definitions:
- MFN_ikt: MFN applied tariff schedule of importer i on product k
- APP_ijkt: effectively applied tariff by importer i on imports from partner j for product k
- Margin_ijkt = MFN_ikt − APP_ijkt (should be weakly non-negative; document and flag exceptions)

## Core research question
Can country ideal points be inferred from cross-IO and treaty participation patterns across issue areas from 1990 to 2025?

## Scope and constraints
- Unit of analysis: country-year, global coverage
- Time span: 1990–2025
- Data: open access only
- Sources: non-regional bodies; UN-affiliated bodies allowed; UN voting data excluded
- Output: a single country-year data frame with an issue-area identifier
- Reports: default output is PDF, fully reproducible

## Planned data product (target schema)
A single table where each row is a country-year-organization/treaty observation with an issue-area label. The long format supports later reshaping to a wide participation matrix for ideal-point estimation.

Minimum fields:
- country_name
- country_code (ISO3 preferred)
- year
- org_or_treaty_id
- org_or_treaty_name
- issue_area (e.g., trade, finance, security, environment, health, telecom, aviation, maritime, labor, IP)
- participation_status (e.g., member, observer, signatory, ratified)
- start_date
- end_date (if applicable)
- source_name
- source_url
- access_date

## Reproducibility principles
- All raw/extracted data files are preserved in-repo.
- All computation lives in separate R scripts.
- Data acquisition via Python for scraping/APIs when needed.
- All transformations are scripted and versioned.
- Sources and access dates are documented.

## Environment setup (new sessions)
1. Create a local `.env` from `.env.example` in the repo root.
2. Fill in `WITS_USER` and `WITS_PASS` with your WITS credentials.
3. Load variables in a new shell session before running acquisition scripts:
```bash
set -a
source .env
set +a
```
Alternatively, run acquisition scripts with the helper:
```bash
python3 scripts/python/run_with_env.py -- python3 scripts/python/acquire_wits_tariffs.py --reporter-iso3 BRA --year 1990
```
4. Do not commit `.env` (it is already in `.gitignore`).

## Workflow (planned)
1) Identify candidate sources and evaluate coverage, openness, and structure.
2) Acquire raw data (Python where needed), archive in `data/raw/`.
3) Standardize country names/codes, validate dates and logical rules.
4) Build country-year-issue participation table in R.
5) Estimate ideal points (Bayesian methods where appropriate).
6) Generate PDF reports and replication package.

## Key documentation
- `docs/estimation_plan.md` — full estimation methodology (dynIRT, anchoring, robustness checks)
- `docs/research_design_notes.md` — threats to credibility, agreed robustness checks, and design decisions (systemic-level framing, stock coding, anchor/item sensitivity, temporal resolution)
- `docs/source_registry.md` — all data sources with URLs and coverage
- `docs/data_dictionary.md` — raw and processed data schemas
- `docs/ilo_predictors_reflections.md` — reflections on predictors, interpretation risks, and strategy for Dim1 as ILO support

## Repository structure (planned)
- data/raw/            # raw downloads, snapshots
- data/processed/      # cleaned and standardized outputs
- scripts/python/      # web scraping, API pulls, text processing
- scripts/R/           # data cleaning, validation, analysis
- outputs/             # tables, figures, PDF reports
- docs/                # documentation, data dictionaries

## Validation requirements
- Check logical date rules (e.g., accession after signature).
- Flag impossible values and incompatible ranges for key variables.
- Document all validation steps in R scripts.

## Notes
This README is a living document and will be updated as sources are selected and the pipeline is implemented.
