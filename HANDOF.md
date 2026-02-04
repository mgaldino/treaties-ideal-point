# Handoff: Core data sources decision

Date: 2026-01-31

## Decision summary
- Proceed with all core candidates for initial ingestion.
- Exclude COW IGO for now because it is not up to date enough (ends in 2014).
- Exclude text-based sources (EDIT, GPTAD) for the first build.

## Core sources for ingestion (first pass)
1) DESTA v2.3 (trade agreements)
2) WTO RTA Database (trade agreements)
3) UNCTAD IIA Navigator (investment treaties)
4) ATOP 5.1 (security alliances)
5) IEADB (environment agreements)

## Excluded for now
- COW IGO v3 (stops in 2014)
- EDIT (text-based investment treaties)
- GPTAD (text-based trade agreements)

## Rationale
- Focus on structured, open, global, and relatively up-to-date sources that provide treaty/membership timing for 1990-2025.
- Avoid heavy text processing in the first iteration.
- Preserve flexibility to add broader IO membership coverage later once a more current source is identified.

## Next steps (pending approval)
- Build reproducible acquisition scripts for each core source.
- Capture raw data snapshots in `data/raw/` and log access dates.
- Define issue-area labels and standardize country codes to ISO3.
- Implement data validation in R with logical date checks.
