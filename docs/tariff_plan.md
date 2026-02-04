# Plan: MFN + Applied Tariffs and Preferential Margins (WITS/TRAINS)

Date: 2026-02-02

## Objective
Build a reproducible dataset of MFN applied tariffs and effectively applied tariffs at the importer–partner–product–year level, then compute preferential margins. The outputs will support latent-variable modeling of trade openness without aggregating across products.

## Scope and core decisions
- Source: WITS/UNCTAD TRAINS API (open access, free account).
- Period: 1990–2024 (document earliest available year and any truncation).
- HS revision: HS2012 as the single target revision (document rationale and source).
- Product level: HS4, derived from line-level (HS6/NTL) data using simple averages.
- Tariff types: ad-valorem lines only (document share dropped).
- Pilot: Brazil 1990 first; then scale to USA, SAU, DNK.
- Partner groups: retain and flag partner groups; treat partner code `000` (World) as MFN baseline and exclude it from applied-tariff partners when computing margins.
- HS mapping: keep ambiguous mappings and report their share by year/country; consider Pierce & Schott approach later for refinement.
- Mapping weights: assign 1/n weight for one-to-many HS mappings so each original HS6 line retains total weight 1 in HS4 aggregation.

## Reproducibility and storage
- Preserve all raw downloads in `data/raw/tariffs/`.
- Log access dates, URLs, and failures in `logs/` and access logs.
- Keep computation in separate R scripts under `scripts/R/`.
- Provide a short README in `docs/` describing sources, coverage, and limitations.

## Phases

### Phase A — Acquisition (Python)
1. Implement a WITS/TRAINS API downloader (one reporter–year at a time).
2. Save raw SDMX-JSON responses by partner to `data/raw/tariffs/wits_trn/`.
3. Save metadata (country list, availability, nomenclature) alongside raw data.
4. Log access dates and any API errors.

### Phase B — Parsing and Standardization (R)
1. Parse SDMX-JSON to a line-level table (HS6/NTL), keeping:
   - reporter, partner, year, product code, tariff type, measure, and value.
2. Filter to ad-valorem observations only.
3. Standardize ISO3, year, and product codes.
4. Store processed line-level data in `data/processed/` for future weighting.

### Phase C — HS Revision Harmonization (R)
1. Map HS88/92 and later revisions to HS2012 using official concordances.
2. Document mapping logic, ambiguity handling, and data loss (if any).

Rationale for HS2012:
- WITS tariff metadata explicitly lists HS2012 among supported HS revisions for tariff indicators, making HS2012 the most stable target revision for reproducible mapping.
- Source: https://wits.worldbank.org/countryprofile/metadata/en/indicator/tariff

### Phase D — Aggregation and Margin Construction (R)
1. Aggregate to HS4 using simple averages from line-level data.
2. Construct MFN and applied tariffs, then compute margins:
   - margin = MFN − applied.
3. Flags:
   - `is_preferential` (margin > 0)
   - `negative_margin` (margin < 0)
4. Keep partner group identifiers as-is when supplied by WITS.

### Phase E — Validation and Outputs (R)
1. Validate logical ranges and missingness (tariff bounds, negative margins).
2. Validate date and value consistency.
3. Export:
   - full importer–partner–product–year dataset
   - MFN-only importer–product–year dataset
4. Generate coverage summaries by country and year.

## Pilot criteria (BRA 1990)
- Successfully download partner data and metadata.
- Parse SDMX-JSON into line-level table.
- Produce HS4 aggregates and preferential margins.
- Report raw file size and scaling estimate.

## Risks and mitigations
- HS revision changes: use documented concordances, report ambiguous mappings.
- Preferential partner groups: retain codes and document interpretation.
- API availability: log failures and retry with diagnostics if needed.
