# V7 Wave 1a Results — 2D Estimation per Domain (Country + Item Anchors)

Generated: 2026-02-08

## V7 Anchors (dim1 = ILO support, dim2 = confounder separation)

| Domain | Dim1+ | Dim1- | Dim2 anchor |
|--------|-------|-------|-------------|
| investment | DNK (+2,0) | IRN (-2,0) | CHN (0,-2) |
| security | DNK (+2,0) | IRN (-2,0) | UKR (0,-2) |
| environment | DNK (+2,0) | SAU (-2,0) | AUS (0,-2) |
| human_rights | DNK (+2,0) | PRK (-2,0) | USA (0,-2) |
| arms_control | NZL (+2,0) | ISR (-2,0) | IND (0,-2) |
| intellectual_property | DNK (+2,0) | AGO (-2,0) | BRA (0,-2) |

## Part A — Country Anchor Estimation (all converged)

| Domain | Iters | Runtime | dim1 trend (1990→2018) | dim1 SD trend | dim2 trend |
|--------|-------|---------|------------------------|---------------|------------|
| investment | 21 | 2.3s | ↑ 0.98 → 1.47 | ↑ 1.72 → 2.04 | ↑ 1.47 → 2.22 |
| security | 24 | 1.2s | ↑ 0.71 → 0.89 | ↑ 1.61 → 1.82 | ↑ 0.60 → 1.04 |
| environment | 17 | 1.5s | ↓ -0.53 → -0.65 | ↑ 1.59 → 2.17 | ↓ -0.86 → -1.16 |
| human_rights | 14 | 0.5s | flat 0.03 → 0.05 | ↑ 0.71 → 0.84 | flat 0.01 → 0.04 |
| arms_control | 19 | 0.6s | flat -0.07 → 0.005 | ↓ 0.88 → 0.36 | flat 0.12 → 0.002 |
| IP | 25 | 1.0s | flat -0.03 → -0.008 | flat 0.83 → 0.80 | flat -0.08 → -0.004 |

## Part B — Item Anchor Estimation

| Domain | Anchor items | Iters | Runtime |
|--------|-------------|-------|---------|
| investment | unctad EC-Ukraine, Korea-Ukraine BIT, Cotonou Agreement | 400 | 35.0s |
| security | ATOP 4675 consul, ATOP 4905 consul, ATOP 4953 nonagg | 400 | 16.0s |
| environment | IEA 3098, IEA 4681, IEA 4641 | 450 | 30.0s |
| human_rights | CRC-OP-SC 2010, CRC-OP-AC 2010, CRC-OP-AC 2000 | 56 | 2.3s |
| arms_control | CWC 1995, CTBT 2015, CWC 2010 | 6 | 0.2s |
| IP | WIPO PLT, Marrakesh, WCT | 300 | 8.9s |

## Part C — Country vs Item Anchor Comparison

| Domain | r(dim1) | r(dim2) | cross-dim | alpha r | beta r | Diagnosis |
|--------|---------|---------|-----------|---------|--------|-----------|
| **investment** | 0.60 | 0.46 | 0.60/0.46 | 0.38 | 0.82/0.75 | **Rotation** — dim1↔dim2 swap between identifications |
| **security** | 0.78 | 0.78 | 0.78/0.78 | -0.08 | 0.93/0.88 | **Collapse** — dim1 ≈ dim2 (identical top/bottom) |
| **environment** | 0.70 | 0.42 | 0.70/0.42 | 0.87 | 0.75/0.64 | **Rotation** — dim2 weakly identified |
| **human_rights** | 0.98 | 0.98 | 0.98/0.98 | 1.00 | 0.98/0.98 | **Collapse** — dim1 ≈ dim2 (identical top/bottom) |
| **arms_control** | **0.90** | **0.99** | 0.65/0.73 | 1.00 | 0.90/0.97 | **Robust** — both dims well separated |
| **IP** | 0.73 | -0.56 | 0.72/-0.58 | 0.99 | 0.98/0.99 | **Rotation** — dim2 sign flip + instability |

## Top/Bottom Countries on Dim1 (Country Anchors)

| Domain | Top 5 | Bottom 5 |
|--------|-------|----------|
| investment | TKM, TUN, MTQ, ITA, SAU | GEO, LBY, QAT, BRB, CUW |
| security | ESP, ITA, HUN, DEU, COD | BLR, KGZ, RUS, KAZ, UZB |
| environment | DEU, FRA, NLD, SWE, ESP | CUW, ABW, GUF, TWN, VGB |
| human_rights | STP, NIU, TON, SOM, FJI | URY, CRI, ECU, ESP, BOL |
| arms_control | NZL, AUT, SVN, HRV, SVK | ISR, AGO, SSD, NIU, DJI |
| IP | DNK, MDA, ROU, KGZ, LVA | SWZ, TZA, VNM, ZMB, AGO |

## Key Findings

1. **Arms control is the only domain with robust 2D structure.** Both dimensions reproduce across country and item anchor identification strategies.

2. **Security and human rights collapsed K=2 into K=1.** The dim2 anchors (UKR for security, USA for HR) did not generate enough separation. In both cases, dim1 and dim2 ideal points are essentially identical (r ≈ 0.78–0.98 within strategy, and top/bottom countries are the same for both dimensions).

3. **Investment, environment, and IP show rotation.** Dim2 is unstable — it swaps or partially rotates with dim1 when identification changes from country to item anchors. This means dim2 content is not robust in these domains.

4. **Dim1 trends are consistent within each domain** (same direction under both identifications where comparable), suggesting dim1 captures a real latent dimension even if dim2 does not.

5. **Implication for the paper**: For most domains, K=1 may be sufficient and the second dimension does not carry robust substantive content. The primary analysis should likely use K=1 dim1 estimates (or equivalently, treat the K=2 dim1 as the quantity of interest). Arms control is the exception where K=2 adds real information.

## Files

- Part A results: `outputs/v7_country_anchors/{domain}_results.rds`
- Part B results: `outputs/v7_item_anchors/{domain}_results.rds`
- Part C reports: `outputs/v7_comparison/{domain}_report.txt`
- Logs: `logs/v7_{domain}_*.log`
- Scripts: `scripts/R/v7_{domain}_{country_anchors,item_anchors,compare}.R`
