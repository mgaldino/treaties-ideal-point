# Block A — Baseline Estimation + Stock Coding

**Date**: 2026-02-08
**Status**: PLAN (awaiting dispatch)
**Goal**: Produce the core estimation results needed for the paper

---

## 0. What's Done

| Domain | V6 Country Anchors | V6 Item Anchors | V6 Comparison |
|--------|-------------------|-----------------|---------------|
| Investment | DONE (ESP dim2) | DONE | DONE |
| Security | DONE (UKR dim2) | DONE | Missing |
| Environment | Script not created | — | — |
| Human Rights | Script exists, not run | — | — |
| Arms Control | Script exists, not run | — | — |
| IP | Script exists, not run | — | — |

**V6 used PCA-based anchors (statistical, not substantive).** Investment + Security serve as a useful baseline. The remaining 4 domains need estimation.

**Tariff acquisition**: 9/10 non-G20 countries done (CHE pending). G20 acquisition complete from earlier sessions.

---

## 1. Design Decisions

### 1.1. Skip V6 completion, go straight to V7

V6 used statistical anchors (PCA extremes). V7 uses **substantive anchors** designed to separate confounders from ILO support. Since V6 for investment/security already exists as baseline, we go straight to V7 for all 6 domains. This is the **main estimation** for the paper.

### 1.2. V7 Anchor Design (substantive, confounder-separating)

The logic: Dim1 = ILO support. Dim2 separates the **main confounder** per domain.

| Domain | Dim1+ | Dim1- | Dim2 anchor | Dim2 pos | Confounder separated |
|--------|-------|-------|-------------|----------|---------------------|
| Investment | DNK (+2,0) | IRN (-2,0) | CHN (0,-2) | Geoeconomic BITs (Belt & Road), not ILO |
| Security | DNK (+2,0) | IRN (-2,0) | UKR (0,-2) | NATO/post-Soviet alignment, not ILO |
| Environment | DNK (+2,0) | SAU (-2,0) | AUS (0,-2) | Non-EU high-capacity Kyoto resistor |
| Human Rights | DNK (+2,0) | PRK (-2,0) | USA (0,-2) | Pro-ILO generally but HR exceptionalism |
| Arms Control | NZL (+2,0) | ISR (-2,0) | IND (0,-2) | Nuclear power, non-NPT regime |
| IP | DNK (+2,0) | AGO (-2,0) | BRA (0,-2) | Active in WIPO but anti-strong IP |

**Key design principle**: Dim2 anchors are countries that **break the ILO correlation** — they are NOT simply anti-ILO, but rather have a specific pattern in that domain that differs from their overall ILO posture.

### 1.3. Stock Coding

Flow coding: +1 = newly ratified in period. Stock coding: +1 = party to treaty in period (cumulative).

**Why it's critical**: Without stock coding, any temporal trend in dim1 could be artifact of activity bias. Countries that exhausted treaty space early (UK, France, Germany) appear "less committed" in later periods under flow coding. Stock coding preserves their cumulative membership signal.

**Implementation**: Build stock-coded flow matrices from existing `baseline_events.csv`, then re-estimate all 6 domains with K=2.

---

## 2. Task Breakdown

### A.1 — V7 Estimation: 6 domains × (country anchors + item anchors + comparison)

**Agents**: 6 Codex agents (one per domain), running in parallel.

Each agent:
1. Creates and runs country-anchor estimation with V7 substantive anchors
2. Selects 3 anchor items from estimated betas
3. Creates and runs item-anchor estimation (custom EM loop with fixed betas)
4. Compares both strategies
5. Saves results to `outputs/v7_country_anchors/`, `outputs/v7_item_anchors/`, `outputs/v7_comparison/`

**Estimated runtime**: 2–60 min per domain (investment/environment largest; HR/AC/IP smallest).

### A.2 — Stock Coding: build matrices + estimate

**Phase 1** (1 Codex agent): Build stock-coded flow matrices.
- Read `data/processed/baseline_events.csv` and `data/processed/item_codebook.csv`
- For each domain, construct N×J matrices where entry (i,j) = +1 if country i is a party to treaty j in period t (cumulative), -1 if treaty is available but country is not party, 0 if treaty not yet open
- Save as `data/processed/{domain}_stock_matrix.rds` with same structure as flow matrices
- Validate: compare dimensions with existing flow matrices, spot-check a few countries

**Phase 2** (6 Codex agents): Estimate K=2 for each domain using stock matrices.
- Use V7 anchors (same as above)
- Save to `outputs/v7_stock_country_anchors/`, `outputs/v7_stock_item_anchors/`

**Phase 3** (1 Codex agent): Compare flow vs stock across all domains.
- Compute correlations per domain per period
- Generate comparison table
- Save to `outputs/v7_flow_vs_stock/`

### A.3 — Validation: UNGA + V-Dem for all 6 domains

**1 Codex agent**: For each domain (all 6), compute:
- Correlation of mean dim1 ideal points with UNGA ideal points (per period + overall)
- Correlation of mean dim1 with V-Dem liberal democracy index (per period + overall)
- Cross-correlation: dim1 × UNGA vs dim1 × V-Dem (to test if dim1 = domestic liberalism or international alignment)
- Aggregate trend: mean(dim1), SD(dim1), skewness(dim1) per period
- Save table to `outputs/v7_validation/`

This uses existing `data/processed/unga_ideal_points_period.csv`. V-Dem data needs to be acquired (agent should download from V-Dem website or use existing if available).

---

## 3. Agent Budget Estimate

| Task | Agents | Type | Priority |
|------|--------|------|----------|
| A.1: V7 estimation (6 domains) | 6 | Codex | HIGH |
| A.2 Phase 1: Stock matrices | 1 | Codex | HIGH |
| A.2 Phase 2: Stock estimation (6 domains) | 6 | Codex | HIGH |
| A.2 Phase 3: Flow vs stock comparison | 1 | Codex | HIGH |
| A.3: Validation (UNGA + V-Dem) | 1 | Codex | HIGH |
| **Total** | **15** | | |

All 15 are HIGH priority. Dispatch order:
1. **A.1 + A.2 Phase 1** in parallel (7 agents) — estimation starts while stock matrices build
2. **A.2 Phase 2** after Phase 1 completes (6 agents)
3. **A.3** after A.1 completes (1 agent, needs V7 results)
4. **A.2 Phase 3** after Phase 2 completes (1 agent)

---

## 4. Outputs Expected

After Block A completes, we will have:

1. **V7 2D ideal points** (flow coding) for all 6 domains with substantive anchors — `outputs/v7_country_anchors/`
2. **V7 item-anchor robustness** for all 6 domains — `outputs/v7_item_anchors/`
3. **V7 stock-coded 2D ideal points** for all 6 domains — `outputs/v7_stock_country_anchors/`
4. **Flow vs stock comparison** — `outputs/v7_flow_vs_stock/`
5. **Validation table** (UNGA + V-Dem correlations per domain per period) — `outputs/v7_validation/`
6. **Aggregate trends** (mean, SD, skewness of dim1 per period per domain) — `outputs/v7_validation/`

These provide the core evidence for the paper:
- Do different domains show a common temporal pattern? (erosion/growth of ILO support)
- Is the pattern robust to coding (flow vs stock)?
- Is the pattern robust to identification (country vs item anchors)?
- Does dim1 correlate with UNGA (international alignment) more than V-Dem (domestic liberalism)?

---

## 5. What Comes After Block A

- **Block B**: Anchor sensitivity (R2) — multiple anchor pairs per domain
- **Block C**: omega2 sensitivity (R4), 3-year windows (R5)
- **Block D**: Trade domain continuous IRT (tariff data)
- **Block E**: Literature review for substantive grounding of dim2 anchors
