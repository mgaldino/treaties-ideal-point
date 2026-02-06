# Research Design Notes: Threats to Credibility and Improvements

**Date**: 2026-02-05
**Status**: Approved for implementation (except where noted as backlog)

---

## 0. Key Framing: Systemic-Level Measurement

The ideal points estimated in this project are **not** intended to measure individual country-level commitment to the International Liberal Order. The research goal is to measure **systemic-level support** — the aggregate behavior of the international community toward ILO institutions over time.

What matters is the overall pattern: e.g., that in the 1990s countries were broadly joining investment protection agreements and no one was denouncing them, whereas by the 2010s a wave of denunciations occurred. Individual country trajectories (Bolivia signing a BIT in 1995 then denouncing it in 2012) are not the quantity of interest; the shift in the aggregate distribution of ideal points across periods is.

This framing affects how we interpret validation results, choose robustness checks, and present findings. Aggregate statistics (mean ideal point per period, dispersion, skewness) are more relevant than individual country rankings.

---

## 0b. Scope Update (2026-02-06)

- The paper's main results will be **2D (K = 2)** using `dynIRT_KD`; 1D is diagnostic only.
- Both **joint 2D** and **per-domain 2D** specifications must be estimated before selecting the final presentation.
- The treaty-based **trade** dimension is paused for main results; the paper requires a **continuous-response IRT** trade dimension based on tariff data (WITS/TRAINS).
- Item anchoring uses **sign constraints** on beta (not tight priors), combined with country anchors informed by PCA + theory.
- Robustness runs default to **5 random seeds** unless explicitly overridden.

---

## 1. Robustness Check: Stock Coding

### Decision
Run **stock (cumulative) coding** as a full robustness check alongside the baseline flow coding.

### Reasoning

The baseline **flow coding** (+1 = newly ratified in period) creates an "activity bias": a country that joined all major treaties before 1990 and maintained membership scores **lower** in later periods than a country that joined them in 2005-2009. Flow coding rewards recent joiners over long-standing members.

**Concrete problem**: The UK, France, and Germany — core liberal order countries — may appear less committed in later periods simply because they exhausted the treaty space early. Meanwhile, a country joining late (e.g., post-Soviet states joining in waves after 1995) appears very committed in those periods.

**Stock coding** treats membership cumulatively: a country that ratified a treaty in 1993 is coded as a member (+1) in all subsequent periods. This preserves the information that long-standing members remain committed.

### Implementation

- Construct an alternative set of flow matrices using stock coding rules:
  - **+1**: Country is a party to the treaty in this period (whether newly joined or already a member)
  - **-1**: Treaty is available but country is not a party
  - **0**: Treaty not yet open, or country does not yet exist
- Re-run `dynIRT_KD()` (K = 2) for all issue areas with the stock matrices
- Compare flow-based and stock-based ideal points:
  - Compute Pearson correlation (per domain, per period, and overall)
  - If r > 0.8: the coding choice has minimal impact — report and move on
  - If r < 0.8: investigate which countries/periods diverge and theorize why
- Report both sets of results; use flow as baseline and stock as robustness

### Priority
**HIGH** — should be completed before paper submission.

---

## 2. Robustness Check: Anchor Sensitivity

### Decision
Run sensitivity analyses using (a) alternative anchor country pairs and (b) alternative anchor items.

### 2a. Country Anchor Sensitivity

**Problem**: Using Denmark as the positive anchor in 6 of 7 domains could introduce artificial cross-domain coherence. If Denmark's position is "imposed" across all domains, the ideal points may look more correlated across domains than they truly are.

**Implementation**:
- For each domain, identify the top 3 and bottom 3 countries on PCA PC1 (already available from Phase 2 EDA)
- Re-estimate each domain using at least **2 alternative anchor pairs**:
  - Pair 1: PCA-based extremes (top-1 and bottom-1 on PC1, excluding Denmark/Iran)
  - Pair 2: Theoretically motivated alternatives (e.g., Netherlands/North Korea, Norway/Syria)
- For each alternative anchor pair:
- Re-run `dynIRT_KD()` (K = 2) with updated priors
  - Compute correlation between baseline and alternative ideal points
  - Report as a sensitivity table: anchor pair → correlation with baseline
- If correlations are consistently > 0.9: anchoring is robust
- If correlations drop below 0.8 for some domains: report which domains are anchor-sensitive and discuss implications

### 2b. Item Anchor Sensitivity

**Problem**: The current identification relies on country-side anchoring (tight priors on Denmark/Iran). An alternative or complementary approach is item-side anchoring — constraining certain treaty items to have known discrimination signs.

**Implementation**:
- Select 2-3 theoretically unambiguous "anchor items" per domain — treaties where the direction of the ILO commitment signal is clear:

| Domain | Candidate anchor items | Expected beta sign |
|--------|----------------------|-------------------|
| Trade | WTO accession | Positive (joining WTO = pro-ILO) |
| Investment | ICSID Convention | Positive (investor-state arbitration = pro-liberal investment regime) |
| Security | NATO founding treaty | Positive (collective defense = pro-liberal security order) |
| Environment | Paris Agreement; Montreal Protocol | Positive |
| Human Rights | ICCPR; ICESCR | Positive |
| Arms Control | NPT; CTBT | Positive |
| IP | Berne Convention; TRIPS | Positive |

- For each anchor item, constrain its beta parameter to be positive by either:
  - Setting a tight informative prior on beta (mean = +1, variance = 0.1)
  - Post-estimation verification: check that anchor items have the correct sign; if not, flag the domain as potentially mis-identified
- Re-estimate models with item-side anchoring **instead of** country-side anchoring and compare results
- Re-estimate models with **both** country-side and item-side anchoring simultaneously
- Report the three specifications (country-only, item-only, both) as a robustness triangle

### 2c. Combined Sensitivity Report

Create a summary table:

```
Domain | Baseline r vs. Alt-anchors-1 | Baseline r vs. Alt-anchors-2 | Baseline r vs. Item-anchored | Baseline r vs. Both-anchored
Trade  | ...                           | ...                           | ...                           | ...
...
```

### Priority
**HIGH** — alternative country anchors and item anchor verification should be completed before paper submission. Full item-side re-estimation is medium priority.

---

## 3. Temporal Resolution: Annual and 3-Year Windows

### Decision
Test finer temporal resolutions as robustness checks.

### Reasoning

Five-year periods mask rapid shifts in the international system. A country that undergoes democratic transition mid-period (e.g., Myanmar 2011-2012) gets averaged with its autocratic years. Similarly, the Trump-era US withdrawal from the Paris Agreement (2017) is in the same period as the Obama-era signing (2016).

For a **systemic-level** analysis, temporal resolution matters even more: a wave of treaty withdrawals concentrated in 2017-2018 looks very different from a gradual decline across 2015-2018, but both are collapsed into a single period under the current scheme.

### Implementation

#### Option A: 3-year windows (preferred first test)
- Periods: 1990-92, 1993-95, 1996-98, 1999-01, 2002-04, 2005-07, 2008-10, 2011-13, 2014-16, 2017-18
- Yields ~10 periods (last may be 2 years only)
- Rebuild flow matrices with 3-year periodization
- Re-run `dynIRT_KD()` (K = 2) with the same priors and omega2
- Compare with 5-year baseline: correlation of ideal points at overlapping country-period midpoints

#### Option B: Annual estimation
- Each year 1990-2018 = 29 periods
- Very sparse: many countries will have zero new ratifications in most years
- Requires tighter evolution variance (omega2 = 0.01-0.05) to prevent erratic trajectories
- More informative for systemic dynamics but computationally heavier
- Try for one domain first (environment, which has the densest participation data)

#### Diagnostic: Within-period timing
Before choosing resolution, compute for each 5-year period:
- Distribution of ratification events by year within the period
- If events cluster in 1-2 years out of 5, the 5-year aggregation loses less information
- If events spread evenly, finer resolution adds value

### Priority
**MEDIUM** — run 3-year windows for at least 2-3 domains before submission. Annual estimation is exploratory.

---

## 4. Multi-Dimensional Estimation (Main Results)

### Decision
**Main results are 2D (K = 2)** using `dynIRT_KD`. The 1D model is diagnostic only. Both **joint 2D** and **per-domain 2D** specifications must be estimated before selecting the final presentation.

### Summary
A custom K-dimensional dynamic IRT estimator via EM with Albert–Chib data augmentation and Kalman filter–smoother has been fully specified. Key design choices:

- **Identification**: K+1 anchor countries with tight priors (not lower-triangular β constraint). For K=2, three anchors: Denmark (+2,+2), Iran (−2,−2), China (+1,−1).
- **Item anchors**: sign constraints on the relevant beta component (no tight priors).
- **Algorithm**: EM with sequential Kalman filter (efficient for many items per period) + RTS smoother.
- **Implementation**: Pure R first, same data structures as `emIRT::dynIRT()`.
- **K=1 reduction**: The algorithm reduces exactly to the existing 1D specification when K=1.

### Status
- Specification: **complete** (`docs/estimation_plan_2d.md`)
- Implementation: **in progress** — this is the main estimator for the paper
- Required runs: joint 2D and per-domain 2D, each with anchor sensitivity checks
- Deterministic EM avoids Stan/MCMC and supports reproducibility

### When to implement
- After data preparation is complete for all 7 domains
- After HIGH-priority robustness checks (stock coding, alternative anchors) are complete
- Use V3 US Congress validation as the external 2D benchmark

---

## 5. Non-Threat: Treaty Agenda Selection

### Decision
Not a threat to this study's credibility. No action needed.

### Reasoning
Liberal countries control the agenda of international treaties — they design the treaty architecture that other countries then join or decline. This could be seen as an endogeneity concern: participation in treaties mechanically correlates with liberal orientation because the menu was designed by liberals.

However, this is **not a threat to this study** because the research goal is precisely to measure support for the liberal international order's agenda. The fact that the treaty architecture reflects liberal preferences is a feature, not a bug. We want to measure whether countries are joining the institutions that liberal states created. If the treaty menu were neutral, it would not serve as a signal of ILO commitment.

### Scope condition
The ideal points measure positioning within the **existing liberal treaty architecture**. They do not measure countries' abstract preferences over all possible institutional arrangements — only their revealed preference for participating in the institutions that actually exist.

---

## 6. Descriptive Focus (Current Scope)

The current paper is **descriptive, not causal**. The ideal points measure latent systemic support for ILO over time. We do not claim to identify what causes changes in ideal points, nor do we use ideal points as explanatory variables in causal models.

Implications:
- Validation focuses on **convergent and discriminant validity** (correlations with UNGA, known-groups tests), not on instrument relevance or exclusion restrictions
- The paper should emphasize the measurement contribution: a new, multi-domain measure of ILO support that complements existing UNGA-based measures
- Causal applications (e.g., "does declining ILO support predict trade wars?") are deferred to future work
