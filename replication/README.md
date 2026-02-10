# Replication Package

**Paper**: "Measuring Support for the International Liberal Order: A Multidimensional Dynamic IRT Approach"

## Requirements

- R >= 4.2
- R packages: `here`, `dplyr`, `readr`, `tidyr`, `purrr`, `ggplot2`, `ggrepel`, `mvtnorm`, `emIRT`, `Rcpp`, `RcppArmadillo`
- LaTeX distribution (for PDF rendering of the Rmd)

## Directory Structure

```
replication/
├── README.md                  # This file
├── paper_draft.Rmd            # Paper (R Markdown)
├── code/
│   ├── model/                 # 2D dynamic IRT algorithm
│   │   ├── dynIRT_KD.R        # Main estimation function
│   │   ├── da_step.R          # E-step (data augmentation)
│   │   ├── m_step.R           # M-step (item parameters)
│   │   └── kalman.R           # Kalman filter-smoother
│   ├── data_prep/             # Data construction
│   │   ├── 01_prepare_data.R  # Treaty flow/stock matrices
│   │   ├── build_stock_matrices.R
│   │   ├── aggregate_tariffs_trade.R  # Trade tariff aggregation
│   │   ├── calc_pcc.R         # Percent correctly classified
│   │   └── 04_prepare_unga.R  # UNGA ideal points
│   ├── estimation/            # V7 estimation runs
│   │   ├── v7_{domain}_country_anchors.R  # Flow estimation (×6)
│   │   ├── v7_stock_{domain}.R            # Stock estimation (×6)
│   │   └── v7_flow_vs_stock_compare.R     # R1 comparison
│   ├── robustness/            # Robustness checks R2--R5
│   │   ├── r2_*.R             # Alternative country anchors
│   │   ├── r3_*.R             # Item anchor sensitivity
│   │   ├── r4_omega2_sensitivity.R  # Evolution variance
│   │   └── r5_*.R             # 3-year temporal windows
│   ├── validation/            # External validation
│   │   ├── download_vdem.R    # V-Dem data download
│   │   └── v7_prepare_vdem_period.R  # V-Dem period matching
│   └── figures/               # Figure generation
│       ├── ideal_points_plot_paper.R  # Treaty domain figures
│       └── fig_trade_paper.R  # Trade figures
├── data/                      # Processed data
│   ├── *_flow_matrix.rds      # Flow matrices (×7 domains)
│   ├── *_stock_matrix.rds     # Stock matrices (×6 domains)
│   ├── *_flow_matrix_3year.rds  # 3-year windows (×6)
│   ├── country_codebook.csv   # Country-to-row mapping
│   ├── item_codebook.csv      # Item-to-column mapping
│   ├── trade_panel.csv        # Country-period tariff panel
│   ├── trade_panel_yearly.csv # Country-year tariff panel
│   └── trade_section_period.csv  # Section-period detail
└── outputs/                   # Estimation results
    ├── v7_country_anchors/    # Baseline estimates (×6 .rds)
    ├── r2_alt_anchors/        # Alt anchor results + findings
    ├── r3_item_anchors/       # Item anchor results + findings
    ├── r4_omega2_sensitivity/ # omega2 results + findings
    ├── r5_3year_windows/      # 3-year results + findings
    └── paper_figures/         # All paper figures (PNG + PDF)
```

## Replication Steps

### 1. Data Preparation

The processed data files in `data/` are ready for estimation. To rebuild them from source:

```r
# Treaty matrices (requires raw data in data/raw/)
source("code/data_prep/01_prepare_data.R")
source("code/data_prep/build_stock_matrices.R")

# Trade tariff panel (requires WITS data in data/processed/tariffs/)
source("code/data_prep/aggregate_tariffs_trade.R")
```

### 2. Baseline Estimation (V7 Flow)

Run each domain separately (each takes 1--3 seconds):

```r
source("code/estimation/v7_investment_country_anchors.R")
source("code/estimation/v7_security_country_anchors.R")
source("code/estimation/v7_environment_country_anchors.R")
source("code/estimation/v7_human_rights_country_anchors.R")
source("code/estimation/v7_arms_control_country_anchors.R")
source("code/estimation/v7_intellectual_property_country_anchors.R")
```

Results are saved to `outputs/v7_country_anchors/`.

### 3. Robustness Checks

```r
# R1: Stock coding
for (f in list.files("code/estimation", "v7_stock_", full.names=TRUE))
  source(f)
source("code/estimation/v7_flow_vs_stock_compare.R")

# R2: Alternative country anchors
source("code/robustness/r2_select_alt_anchors.R")
source("code/robustness/r2_estimate_alt_anchors.R")
source("code/robustness/r2_compare_with_baseline.R")

# R3: Item anchor sensitivity
source("code/robustness/r3_select_anchor_items.R")
source("code/robustness/r3_item_anchor_test.R")

# R4: omega2 sensitivity
source("code/robustness/r4_omega2_sensitivity.R")

# R5: 3-year windows
source("code/robustness/r5_prepare_3year_data.R")
source("code/robustness/r5_estimate_3year.R")
source("code/robustness/r5_compare_with_baseline.R")
```

### 4. Figures

```r
# Treaty domain figures (Figs 1--6)
source("code/figures/ideal_points_plot_paper.R")

# Trade figures (Figs 7--9)
source("code/figures/fig_trade_paper.R")
```

Figures are saved to `outputs/paper_figures/`.

### 5. Paper

Render the paper from the project root:

```r
rmarkdown::render("docs/paper_draft.Rmd")
```

## Data Sources

| Domain | Source | URL |
|--------|--------|-----|
| Investment | UNCTAD IIA Navigator | https://investmentpolicy.unctad.org/international-investment-agreements |
| Security | ATOP 5.1 | http://www.atopdata.org/ |
| Environment | IEADB | https://iea.uoregon.edu/ |
| Human Rights | UN Treaty Collection Ch. IV | https://treaties.un.org/ |
| Arms Control | UN Treaty Collection Ch. XXVI + NPT/BWC | https://treaties.un.org/ |
| Intellectual Property | WIPO Lex | https://www.wipo.int/treaties/ |
| Trade (tariffs) | WITS/TRAINS | https://wits.worldbank.org/ |
| Validation (UNGA) | Bailey, Strezhnev, Voeten | https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ |
| Validation (V-Dem) | V-Dem v13 | https://www.v-dem.net/ |
