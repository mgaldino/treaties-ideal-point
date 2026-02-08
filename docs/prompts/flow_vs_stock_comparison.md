# Stock Coding — Phase 3: Flow vs Stock Comparison

**Do NOT ask questions. Just execute.**

Working directory: `cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"`

## What To Do

Create and run `scripts/R/v7_flow_vs_stock_compare.R` that:

1. For each of the 6 domains, loads:
   - `outputs/v7_country_anchors/{domain}_results.rds` (flow coding)
   - `outputs/v7_stock_country_anchors/{domain}_results.rds` (stock coding)

2. For each domain, computes:
   - **Per-period correlation** of dim1 ideal points (flow vs stock) for each of the 6 periods
   - **Overall correlation** of mean ideal points (dim1 and dim2)
   - **Aggregate trend comparison**: mean(dim1) per period for flow vs stock — are the temporal trends similar?
   - **Delta table**: countries with largest discrepancy between flow and stock dim1 (these are the "activity bias" cases)

3. Produces a summary table:

```
domain | period | r_dim1 | r_dim2 | mean_dim1_flow | mean_dim1_stock | delta_mean
```

4. Saves:
   - `outputs/v7_flow_vs_stock/comparison_table.csv`
   - `outputs/v7_flow_vs_stock/comparison_report.txt` (formatted text with interpretation)
   - `outputs/v7_flow_vs_stock/top_discrepant_countries.csv` (per domain, top 10 countries with largest |flow - stock| dim1 difference)

## Interpretation Guide

Include in the report:
- If r > 0.9: coding choice has minimal impact → report and move on
- If r < 0.8: flag the domain and investigate which countries/periods diverge
- If temporal trends (mean_dim1 per period) differ in direction: this is critical — means the "erosion" finding depends on coding choice
- The "top discrepant" countries should be interpretable: expect early joiners (UK, France, Germany, Nordic countries) to show the largest positive shift under stock coding

## What To Report

1. The full comparison table
2. Which domains are robust (r > 0.9) vs sensitive (r < 0.8)?
3. Do temporal trends agree in direction across flow and stock?
4. Top 5 most discrepant countries per domain and why

## Constraints

- Do NOT modify existing files
- Do NOT ask questions
- If a domain's results file is missing, skip it and note the omission
