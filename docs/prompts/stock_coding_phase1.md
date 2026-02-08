# Stock Coding — Phase 1: Build Stock-Coded Flow Matrices

**Do NOT ask questions. Just execute.**

Working directory: `cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"`

## Background

The project currently uses **flow coding**: a country gets +1 for a treaty only in the period when it ratifies. This creates "activity bias" — countries that ratified everything before 1990 appear inactive later.

**Stock coding** is the alternative: +1 means the country IS a party to the treaty in that period (cumulative). This preserves long-standing membership.

## What To Do

Create a script `scripts/R/build_stock_matrices.R` that:

1. Reads the existing flow matrices for all 6 domains:
   - `data/processed/investment_flow_matrix.rds`
   - `data/processed/security_flow_matrix.rds`
   - `data/processed/environment_flow_matrix.rds`
   - `data/processed/human_rights_flow_matrix.rds`
   - `data/processed/arms_control_flow_matrix.rds`
   - `data/processed/intellectual_property_flow_matrix.rds`

2. Reads the baseline events data:
   - `data/processed/baseline_events.csv`
   - `data/processed/item_codebook.csv`
   - `data/processed/country_codebook.csv`

3. For each domain, understands the existing flow matrix structure:
   - `flow$rc`: N × J matrix (rows=countries, cols=items). Values: +1 (event in period), -1 (no event, treaty available), 0 (not applicable / treaty not yet open)
   - `flow$startlegis`: 0-indexed first active period per country
   - `flow$endlegis`: 0-indexed last active period per country
   - `flow$bill.session`: 0-indexed period assignment per item
   - `flow$T`: number of periods (6)
   - `flow$country_codes`, `flow$item_labels`, `flow$period_labels`

4. **Key**: In the existing flow matrices, each item (treaty × period) is a separate column. An item in period t gets +1 if the country ratified in period t, -1 if the country could have but didn't. Under stock coding:
   - If a country ratified treaty X in period 2, then in the flow matrix items for treaty X in periods 3, 4, 5, 6, the stock matrix should show +1 (not -1 as flow coding would).
   - Concretely: for each underlying treaty, find all items (columns) across periods. If a country has +1 for period t, it should also have +1 for all subsequent periods t+1, t+2, ... for the same treaty.

5. To implement this:
   - Parse item labels to extract the treaty name and period (the item labels follow patterns like `source_treaty-name__period-range`)
   - Group items by underlying treaty
   - For each country and each treaty group: if +1 appears in period t, set +1 for all subsequent periods too
   - Keep the same matrix structure (N × J, same country/item ordering)

6. Save stock matrices with identical structure:
   - `data/processed/investment_stock_matrix.rds`
   - `data/processed/security_stock_matrix.rds`
   - `data/processed/environment_stock_matrix.rds`
   - `data/processed/human_rights_stock_matrix.rds`
   - `data/processed/arms_control_stock_matrix.rds`
   - `data/processed/intellectual_property_stock_matrix.rds`

7. Validate:
   - Each stock matrix should have **more +1 entries** than the corresponding flow matrix (stock ≥ flow)
   - Each stock matrix should have the **same dimensions** (N, J, T)
   - Print summary: for each domain, total +1 in flow vs stock, ratio

## What To Report

1. For each domain: dimensions (N, J), total +1 in flow, total +1 in stock, ratio (stock/flow)
2. A few spot checks: pick 3 well-known countries (DNK, USA, BRA) and show their treaty counts per period under flow vs stock for one domain
3. Confirm all 6 stock matrix .rds files saved
4. Any errors or edge cases encountered (e.g., treaties that don't follow the expected label pattern)

## Constraints

- Do NOT modify any existing files (especially `01_prepare_data.R`)
- Create a NEW script `scripts/R/build_stock_matrices.R`
- Do NOT ask questions
- If item label parsing is ambiguous, print warnings and use best-effort matching
