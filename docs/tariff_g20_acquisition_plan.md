# Plan: WITS Tariff Acquisition with Credentials (G20, 1990–2022)

Date: 2026-02-05

## Objective
Re-attempt WITS/TRAINS tariff acquisition for BRA 2022 using credentials, then scale acquisition (raw data only) to G20 countries for 1990–2022. No parsing, mapping, or margin construction in this plan.

## Scope and decisions
- Countries (G20 ISO3): ARG, AUS, BRA, CAN, CHN, FRA, DEU, IND, IDN, ITA, JPN, KOR, MEX, RUS, SAU, ZAF, TUR, GBR, USA.
- EU is a G20 member but is excluded (not a country in WITS); treat separately only if requested.
- Years: 1990–2022 inclusive, skipping years already present.
- Working assumption: 2023–2024 are not available in WITS/TRAINS; do not attempt unless requested.
- Authentication: use `.env` with `WITS_USER`/`WITS_PASS` (loaded via `scripts/python/run_with_env.py`).
- Acquisition only: store raw JSON and XML metadata; no downstream parsing in this plan.
- Preserve raw data under `data/raw/tariffs/wits_trn/{ISO3}/{YEAR}/` and `access_log.json` per run.

## Outputs
- Raw SDMX-JSON files per partner: `data/raw/tariffs/wits_trn/{ISO3}/{YEAR}/tariffs_partner_*.json`
- Metadata per year: `countries_metadata.xml`, `data_availability.xml`, `access_log.json`
- Logs in `logs/` for each batch run
- Optional manifest CSV summarizing status by reporter-year (if enabled in batch driver)

## Phases

### Phase 0 — Preflight (Credentials + Environment)
1. Confirm `.env` exists and helper works: `python3 scripts/python/run_with_env.py -- env | rg WITS_`.
2. Validate credentialed access by fetching metadata for a known country-year.
3. Record preflight outcome in `HANDOFF.md`.

### Phase 1 — Batch Driver (Python)
1. Add a resumable batch driver (e.g., `scripts/python/acquire_wits_tariffs_batch.py`) that:
   - Iterates reporter-year pairs.
   - Skips existing year directories by default.
   - Uses `WITS_USER`/`WITS_PASS` from `.env` (calls existing acquire script or imports its logic).
   - Writes per-run logs in `logs/` with timestamp.
   - Supports a simple retry on transient HTTP errors (one retry after 30s) and logs failures.
   - Optionally writes a manifest CSV summarizing success/failure by reporter-year.
2. Add unit tests for the batch driver.
3. Update `HANDOFF.md` after Phase 1.

### Phase 2 — BRA 2022 Re-attempt (Acquisition)
1. Run batch driver for BRA 2022 with credentials.
2. Verify outputs exist (`countries_metadata.xml`, `data_availability.xml`, at least one `tariffs_partner_*.json` when available).
3. Log data-not-found or HTTP errors in `access_log.json` and `logs/`.
4. Update `HANDOFF.md` after Phase 2.

### Phase 3 — G20 Scale-out (Acquisition)
1. Run batch driver for all G20 countries, years 1990–2022, skipping existing years.
2. Use sequential or low-concurrency runs to reduce API strain.
3. If any run fails due to DNS/403/timeout, follow diagnostics + retry protocol (per project rules) and document in logs.
4. Update `HANDOFF.md` after Phase 3.

## Stop/Review Gates
- After Phase 1, confirm batch driver behavior and logs before running data acquisition.
- After Phase 2, review BRA 2022 outcome before scaling to all G20.

## Notes on updated acquisition strategy (credentials)
- Always run acquisition through `.env` to enable authenticated access.
- If “Data not found” persists with credentials, treat as unavailable and log explicitly.
- Use the helper CLI to avoid manual `source .env` in new sessions.
