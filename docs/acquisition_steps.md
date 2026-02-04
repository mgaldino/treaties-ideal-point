# Acquisition Steps (Core Sources)

Access date reference: 2026-01-31

This file documents how to acquire each core source in a reproducible way. When an automated download is possible, use the Python scripts in `scripts/python/`. When manual export is required, follow the steps below and store the raw file in the indicated folder.

## 1) DESTA v2.3 (trade agreements)
- Preferred: run `python3 scripts/python/acquire_desta.py`.
- Output folder: `data/raw/desta/`
- If the script does not find direct links, manually download from the DESTA downloads page and place the file in `data/raw/desta/`.
- Record access date in `data/raw/desta/access_log.json` (created by the script).

## 2) WTO RTA Database (trade agreements)
- Preferred: run `python3 scripts/python/acquire_wto_rta.py`.
- Output folder: `data/raw/wto_rta/`
- If no direct link is detected:
  1) Go to the WTO Data Portal dataset page for RTAs.
  2) Use the export feature to download the dataset as CSV or Excel.
  3) Save the file into `data/raw/wto_rta/`.
  4) Update `data/raw/wto_rta/access_log.json` with the access date and file name.

## 3) UNCTAD IIA Navigator (investment treaties)
- Preferred: run `python3 scripts/python/acquire_unctad_iia.py --scrape`.
- Output folder: `data/raw/unctad_iia/`
- The scraper pulls the country catalog from `advanced-search` and visits each country page.
- Raw HTML pages are saved in `data/raw/unctad_iia/html/`.
- Output CSV: `data/raw/unctad_iia/unctad_iia_country_treaties.csv`.
- If a run is interrupted, rerun with `--scrape --resume` or use `--parse-only` to parse existing HTML without fetching.
- If scraping fails, fall back to manual export:
  1) Use the IIA Navigator advanced search to query all treaties.
  2) Export the results (CSV or Excel).
  3) Save the export to a local path and run:
     `python3 scripts/python/acquire_unctad_iia.py --manual-file /path/to/export.xlsx`
  4) The script copies the file into `data/raw/unctad_iia/` and updates the access log.

## 4) ATOP 5.1 (security alliances)
- Preferred: run `python3 scripts/python/acquire_atop.py`.
- Output folder: `data/raw/atop/`
- If the site does not expose direct links:
  1) Download the dataset from the ATOP data page manually.
  2) Place the file(s) into `data/raw/atop/`.
  3) Update `data/raw/atop/access_log.json` with access date and file name.

## 5) IEADB (environment agreements)
- Preferred: run `python3 scripts/python/acquire_ieadb.py`.
- Output folder: `data/raw/ieadb/`
- If the download page changes, manually download the IEADB ZIP/CSV files and place them in `data/raw/ieadb/`.

## Manual logging rule
If you download a file manually, add an entry to the relevant `access_log.json` with:
- source_name
- landing_url
- access_date
- downloaded_files
- notes (optional)

This keeps provenance and access dates explicit for reproducibility.
