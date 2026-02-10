# V7 Block A: 2D Ideal Point Estimation for the International Liberal Order

## What we did
We estimated two-dimensional dynamic ideal points for 203 countries across 6 treaty domains (investment, security, environment, human rights, arms control, intellectual property) over 6 five‑year periods (1990–2018). Dimension 1 is anchored to capture support for the International Liberal Order (ILO); Dimension 2 captures a residual confounder axis. We then ran three validation exercises: (1) external validation against UNGA voting and V‑Dem liberal democracy scores, (2) a stock‑vs‑flow coding robustness check, and (3) temporal trend analysis.

All figures referenced below were generated in `figs/`. The full set of per‑domain period‑wise scatterplots and flow‑vs‑stock distributions are listed in the Supplementary Figures table.

## Coverage note
External validation plots drop country‑periods with missing UNGA or V‑Dem values. This affects a small but non‑zero subset of observations and explains the point removals reported during plot generation. The heatmap and scatterplots therefore summarize **available** overlap rather than the full universe of country‑periods.

## Finding 1: Dim1 aligns more with V‑Dem than UNGA in 3/6 domains, and this pattern is stable over time
The domain × period correlation heatmap (Figure 1) shows the core result: **V‑Dem outperforms UNGA in three domains** (environment, security, arms_control). This pattern is not a single‑period artifact; period‑wise scatterplots (Supplementary Figures S1–S12) show consistently positive V‑Dem correlations across periods for those domains. The UNGA–V‑Dem trajectory plot (Figure 2) visualizes that several countries move in V‑Dem space without commensurate shifts in UNGA alignment, suggesting Dim1 partly tracks domestic liberalism rather than geopolitical voting alignment.

Key stability ranges across periods (from `outputs/v7_validation/correlation_table.csv`):
- **Environment:** r(Dim1, V‑Dem) ≈ 0.65–0.72; r(Dim1, UNGA) ≈ 0.49–0.58.
- **Security:** r(Dim1, V‑Dem) ≈ 0.47–0.55; r(Dim1, UNGA) ≈ 0.38–0.49.
- **Arms control:** r(Dim1, V‑Dem) ≈ 0.44–0.49; r(Dim1, UNGA) ≈ 0.29–0.47.

Two domains remain problematic:
- **Investment:** near‑zero correlations for both benchmarks (UNGA ≈ 0.07–0.14; V‑Dem ≈ −0.02–0.08), suggesting Dim1 reflects bilateral economic logic rather than alignment or domestic liberalism.
- **Human rights:** negative correlations with both benchmarks (UNGA ≈ −0.18 to −0.12; V‑Dem ≈ −0.33 to −0.25), indicating polarity inversion or a distinct latent dimension.

**Figure 1.** Correlation heatmap: Dim1 vs UNGA and V‑Dem, by domain and period. `figs/fig_dim1_corr_heatmap.png`

**Figure 2.** Trajectories in UNGA–V‑Dem space (color = Dim1) for key countries. `figs/fig_unga_vdem_dim1_trajectories.png`

## Finding 2: Activity bias (flow vs stock) materially changes levels, trends, and even polarity
Flow vs stock coding divergences are not cosmetic. The dispersion plot (Figure 3) shows that SD trajectories differ sharply across domains; the key‑country trajectories (Figure 4) reveal that for several countries the sign and shape of Dim1 differ under flow vs stock. Period‑wise flow‑vs‑stock scatterplots (Supplementary Figures S13–S18) and distributional overlays (Supplementary Figures S19–S24) show strong inconsistencies in arms_control and human_rights, while environment and investment remain comparatively robust.

These visuals reinforce that any claim about “erosion” or “growth” in ILO support depends on coding choice unless additional robustness checks resolve which representation best captures substantive meaning.

**Figure 3.** Dispersion over time (SD) under flow vs stock, by domain. `figs/fig_dispersion_flow_stock.png`

**Figure 4.** Flow vs stock trajectories (Dim1) for key countries. `figs/fig_flow_stock_trajectories_key_countries.png`

## Finding 3: Temporal dynamics are domain‑specific, not systemic
The domain trend plot with uncertainty (Figure 5) shows clear heterogeneity: investment and security increase, environment shows declining mean but rising dispersion, and arms_control/human_rights/IP remain largely flat. The aggregate trend (Figure 6) is dominated by these offsetting movements, and the slope comparison (Figure 7) makes explicit that flow and stock imply different directions in several domains.

This visual evidence supports the interpretation that there is **no single systemic trend** across domains; “ILO erosion” is domain‑specific rather than uniform.

**Figure 5.** Domain trends (mean ± SE), flow coding. `figs/fig_trends_domain_uncertainty.png`

**Figure 6.** Aggregate trend (equal weights) vs domain trends. `figs/fig_aggregate_trend.png`

**Figure 7.** Slope comparison (flow vs stock) by domain. `figs/fig_slope_comparison.png`

## Human rights diagnostic: polarity and item structure
The human rights domain shows negative correlations with both benchmarks. The polarity check (Figures 8–9) shows that flipping Dim1 restores positive association with V‑Dem, suggesting sign inversion. The outlier plot (Figure 10) indicates a small set of influential countries driving deviations. The item‑level diagnostic (Figure 11) shows that discrimination strength varies by treaty opening year, which may reflect selective ratification dynamics rather than a liberal‑authoritarian axis.

**Figure 8.** Human rights: Dim1 vs V‑Dem (original). `figs/fig_hr_dim1_vdem.png`

**Figure 9.** Human rights: Dim1 vs V‑Dem (polarity flipped). `figs/fig_hr_dim1_vdem_flipped.png`

**Figure 10.** Human rights: outliers vs V‑Dem. `figs/fig_hr_outliers_vdem.png`

**Figure 11.** Human rights: item discrimination vs treaty opening year. `figs/fig_hr_item_beta_year.png`

## Summary
**Table 1. Summary of findings.**

| Finding | Implication |
| --- | --- |
| V‑Dem > UNGA in 3/6 domains (stable across periods) | Dim1 partly captures domestic liberalism, not purely geopolitical alignment |
| 4/6 domains sensitive to flow vs stock | Temporal trend claims require robustness qualification |
| No uniform temporal trend | “ILO erosion” is domain‑specific, not systemic |
| HR negative validation | Dim1 polarity likely inverted or structurally distinct |

## Next steps
1. Alternative anchor sensitivity (R2): verify results under different anchor countries.
2. Omega sensitivity (R4): vary evolution variance to test trend robustness.
3. Item anchor sensitivity (R3): constrain anchor item discrimination parameters.
4. Trade domain: implement continuous‑response IRT on tariff data.

## Supplementary figures (index)
**Table 2. Supplementary figure index.**

| Figure | File | Caption |
| --- | --- | --- |
| S1 | `figs/fig_dim1_unga_arms_control_by_period.png` | Dim1 vs UNGA by period (arms_control) |
| S2 | `figs/fig_dim1_unga_environment_by_period.png` | Dim1 vs UNGA by period (environment) |
| S3 | `figs/fig_dim1_unga_human_rights_by_period.png` | Dim1 vs UNGA by period (human_rights) |
| S4 | `figs/fig_dim1_unga_intellectual_property_by_period.png` | Dim1 vs UNGA by period (intellectual_property) |
| S5 | `figs/fig_dim1_unga_investment_by_period.png` | Dim1 vs UNGA by period (investment) |
| S6 | `figs/fig_dim1_unga_security_by_period.png` | Dim1 vs UNGA by period (security) |
| S7 | `figs/fig_dim1_vdem_arms_control_by_period.png` | Dim1 vs V‑Dem by period (arms_control) |
| S8 | `figs/fig_dim1_vdem_environment_by_period.png` | Dim1 vs V‑Dem by period (environment) |
| S9 | `figs/fig_dim1_vdem_human_rights_by_period.png` | Dim1 vs V‑Dem by period (human_rights) |
| S10 | `figs/fig_dim1_vdem_intellectual_property_by_period.png` | Dim1 vs V‑Dem by period (intellectual_property) |
| S11 | `figs/fig_dim1_vdem_investment_by_period.png` | Dim1 vs V‑Dem by period (investment) |
| S12 | `figs/fig_dim1_vdem_security_by_period.png` | Dim1 vs V‑Dem by period (security) |
| S13 | `figs/fig_flow_stock_dim1_arms_control_by_period.png` | Flow vs stock Dim1 by period (arms_control) |
| S14 | `figs/fig_flow_stock_dim1_environment_by_period.png` | Flow vs stock Dim1 by period (environment) |
| S15 | `figs/fig_flow_stock_dim1_human_rights_by_period.png` | Flow vs stock Dim1 by period (human_rights) |
| S16 | `figs/fig_flow_stock_dim1_intellectual_property_by_period.png` | Flow vs stock Dim1 by period (intellectual_property) |
| S17 | `figs/fig_flow_stock_dim1_investment_by_period.png` | Flow vs stock Dim1 by period (investment) |
| S18 | `figs/fig_flow_stock_dim1_security_by_period.png` | Flow vs stock Dim1 by period (security) |
| S19 | `figs/fig_dim1_density_flow_stock_arms_control.png` | Dim1 density: flow vs stock (arms_control) |
| S20 | `figs/fig_dim1_density_flow_stock_environment.png` | Dim1 density: flow vs stock (environment) |
| S21 | `figs/fig_dim1_density_flow_stock_human_rights.png` | Dim1 density: flow vs stock (human_rights) |
| S22 | `figs/fig_dim1_density_flow_stock_intellectual_property.png` | Dim1 density: flow vs stock (intellectual_property) |
| S23 | `figs/fig_dim1_density_flow_stock_investment.png` | Dim1 density: flow vs stock (investment) |
| S24 | `figs/fig_dim1_density_flow_stock_security.png` | Dim1 density: flow vs stock (security) |
| S25 | `figs/fig_dim12_extremes_1990_1994.png` | Dim1 vs Dim2 with extremes labeled (1990-1994) |
| S26 | `figs/fig_dim12_extremes_1995_1999.png` | Dim1 vs Dim2 with extremes labeled (1995-1999) |
| S27 | `figs/fig_dim12_extremes_2000_2004.png` | Dim1 vs Dim2 with extremes labeled (2000-2004) |
| S28 | `figs/fig_dim12_extremes_2005_2009.png` | Dim1 vs Dim2 with extremes labeled (2005-2009) |
| S29 | `figs/fig_dim12_extremes_2010_2014.png` | Dim1 vs Dim2 with extremes labeled (2010-2014) |
| S30 | `figs/fig_dim12_extremes_2015_2018.png` | Dim1 vs Dim2 with extremes labeled (2015-2018) |

## Figures (rendered)
**Figure 1.** Correlation heatmap: Dim1 vs UNGA and V‑Dem, by domain and period.

![](figs/fig_dim1_corr_heatmap.png)

**Figure 2.** Trajectories in UNGA–V‑Dem space (color = Dim1) for key countries.

![](figs/fig_unga_vdem_dim1_trajectories.png)

**Figure 3.** Dispersion over time (SD) under flow vs stock, by domain.

![](figs/fig_dispersion_flow_stock.png)

**Figure 4.** Flow vs stock trajectories (Dim1) for key countries.

![](figs/fig_flow_stock_trajectories_key_countries.png)

**Figure 5.** Domain trends (mean ± SE), flow coding.

![](figs/fig_trends_domain_uncertainty.png)

**Figure 6.** Aggregate trend (equal weights) vs domain trends.

![](figs/fig_aggregate_trend.png)

**Figure 7.** Slope comparison (flow vs stock) by domain.

![](figs/fig_slope_comparison.png)

**Figure 8.** Human rights: Dim1 vs V‑Dem (original).

![](figs/fig_hr_dim1_vdem.png)

**Figure 9.** Human rights: Dim1 vs V‑Dem (polarity flipped).

![](figs/fig_hr_dim1_vdem_flipped.png)

**Figure 10.** Human rights: outliers vs V‑Dem.

![](figs/fig_hr_outliers_vdem.png)

**Figure 11.** Human rights: item discrimination vs treaty opening year.

![](figs/fig_hr_item_beta_year.png)

## Supplementary figures (rendered)
**Figure S1.** Dim1 vs UNGA by period (arms_control).

![](figs/fig_dim1_unga_arms_control_by_period.png)

**Figure S2.** Dim1 vs UNGA by period (environment).

![](figs/fig_dim1_unga_environment_by_period.png)

**Figure S3.** Dim1 vs UNGA by period (human_rights).

![](figs/fig_dim1_unga_human_rights_by_period.png)

**Figure S4.** Dim1 vs UNGA by period (intellectual_property).

![](figs/fig_dim1_unga_intellectual_property_by_period.png)

**Figure S5.** Dim1 vs UNGA by period (investment).

![](figs/fig_dim1_unga_investment_by_period.png)

**Figure S6.** Dim1 vs UNGA by period (security).

![](figs/fig_dim1_unga_security_by_period.png)

**Figure S7.** Dim1 vs V‑Dem by period (arms_control).

![](figs/fig_dim1_vdem_arms_control_by_period.png)

**Figure S8.** Dim1 vs V‑Dem by period (environment).

![](figs/fig_dim1_vdem_environment_by_period.png)

**Figure S9.** Dim1 vs V‑Dem by period (human_rights).

![](figs/fig_dim1_vdem_human_rights_by_period.png)

**Figure S10.** Dim1 vs V‑Dem by period (intellectual_property).

![](figs/fig_dim1_vdem_intellectual_property_by_period.png)

**Figure S11.** Dim1 vs V‑Dem by period (investment).

![](figs/fig_dim1_vdem_investment_by_period.png)

**Figure S12.** Dim1 vs V‑Dem by period (security).

![](figs/fig_dim1_vdem_security_by_period.png)

**Figure S13.** Flow vs stock Dim1 by period (arms_control).

![](figs/fig_flow_stock_dim1_arms_control_by_period.png)

**Figure S14.** Flow vs stock Dim1 by period (environment).

![](figs/fig_flow_stock_dim1_environment_by_period.png)

**Figure S15.** Flow vs stock Dim1 by period (human_rights).

![](figs/fig_flow_stock_dim1_human_rights_by_period.png)

**Figure S16.** Flow vs stock Dim1 by period (intellectual_property).

![](figs/fig_flow_stock_dim1_intellectual_property_by_period.png)

**Figure S17.** Flow vs stock Dim1 by period (investment).

![](figs/fig_flow_stock_dim1_investment_by_period.png)

**Figure S18.** Flow vs stock Dim1 by period (security).

![](figs/fig_flow_stock_dim1_security_by_period.png)

**Figure S19.** Dim1 density: flow vs stock (arms_control).

![](figs/fig_dim1_density_flow_stock_arms_control.png)

**Figure S20.** Dim1 density: flow vs stock (environment).

![](figs/fig_dim1_density_flow_stock_environment.png)

**Figure S21.** Dim1 density: flow vs stock (human_rights).

![](figs/fig_dim1_density_flow_stock_human_rights.png)

**Figure S22.** Dim1 density: flow vs stock (intellectual_property).

![](figs/fig_dim1_density_flow_stock_intellectual_property.png)

**Figure S23.** Dim1 density: flow vs stock (investment).

![](figs/fig_dim1_density_flow_stock_investment.png)

**Figure S24.** Dim1 density: flow vs stock (security).

![](figs/fig_dim1_density_flow_stock_security.png)

**Figure S25.** Dim1 vs Dim2 with extremes labeled (1990-1994).

![](figs/fig_dim12_extremes_1990_1994.png)

**Figure S26.** Dim1 vs Dim2 with extremes labeled (1995-1999).

![](figs/fig_dim12_extremes_1995_1999.png)

**Figure S27.** Dim1 vs Dim2 with extremes labeled (2000-2004).

![](figs/fig_dim12_extremes_2000_2004.png)

**Figure S28.** Dim1 vs Dim2 with extremes labeled (2005-2009).

![](figs/fig_dim12_extremes_2005_2009.png)

**Figure S29.** Dim1 vs Dim2 with extremes labeled (2010-2014).

![](figs/fig_dim12_extremes_2010_2014.png)

**Figure S30.** Dim1 vs Dim2 with extremes labeled (2015-2018).

![](figs/fig_dim12_extremes_2015_2018.png)
