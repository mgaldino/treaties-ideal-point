# R3 Robustness Check: Item Anchor Sensitivity (2D dynamic IRT)

Date: 2026-02-09 12:01:51 (local)

## What R3 Tests
- Test A (Sign verification): whether theoretically motivated items (Set A) have positive `beta1` in V7 baseline.
- Test B (Constrained item starts): reruns V7 with the same country anchors, but forces Set A items to start with `beta1 >= 0` to test whether convergence depends on item-start sign.
- Test C (Swapped dim1 anchors): reruns V7 with dim1 positive/negative anchors swapped; identification implies dim1 ideal points should flip sign (correlation ~ -1 with baseline) and `beta1` should also flip (correlation between baseline `beta1` and `-beta1_swapped` should be high).

## Outputs
- Per domain sign checks: `outputs/r3_item_anchors/{domain}_sign_check.csv`
- Test B results: `outputs/r3_item_anchors/{domain}_constrained_results.rds`
- Test C results: `outputs/r3_item_anchors/{domain}_swapped_results.rds`
- Summary: `outputs/r3_item_anchors/summary_table.csv`

## Summary Table (Key Diagnostics)

Columns:
- `n_theory_pos_v7 / n_theory_items`: how many theory-driven items had `beta1>0` in V7 baseline.
- `testB_cor_dim1, testB_cor_dim2`: correlation of mean ideal points vs baseline after constrained starts (should be ~ +1 if robust).
- `testC_cor_dim1`: correlation of mean dim1 ideal points vs baseline under swapped anchors (should be ~ -1).
- `testC_beta1_flip_cor`: correlation of baseline `beta1` with `-beta1_swapped` across all items (should be high).
- `testC_setA_flip_prop`: share of Set A items whose `beta1` sign flips under swapped anchors (should be near 1 when Set A exists).

```
                domain n_theory_items n_theory_pos_v7 n_data_items
            investment              0               0            3
              security              0               0            3
           environment              2               2            3
          human_rights              1               0            3
          arms_control              2               2            3
 intellectual_property              1               1            3
 n_data_pos_v7 testB_cor_dim1 testB_cor_dim2 testC_cor_dim1 testC_cor_dim2
             0      0.9985227      0.9957745     -0.9985227      0.9957745
             0      0.7891195      0.6359462     -0.7891195      0.6359462
             3      0.9995136      0.9970888     -0.9995136      0.9970888
             0      0.9767244      0.9860208     -0.9786085      0.9851235
             3      0.9833787      0.9996942     -0.9833787      0.9996942
             3      0.9967443      0.9989784     -0.9967443      0.9989784
 testC_beta1_flip_cor testC_setA_flip_prop
            0.9976155                   NA
            0.9989114                   NA
            0.9973142                    1
            0.9932174                    1
            0.9817705                    1
            0.9962940                    1
```

## Interpretation Guidance
- If Test B correlations are ~1.0, item-start sign constraints do not materially affect the V7 solution (good robustness).
- If Test C dim1 correlations are ~-1.0 and `beta1` flips strongly, the model is behaving as expected under identification changes.
- Deviations in Test C (e.g., dim1 not flipping) are red flags for identification/configuration or a coding issue in anchoring.

## Domain-Level Diagnostics (From `summary_table.csv`)
- `investment`: theory items: none found in V7 (Set A empty) | Test B cor(mean IP): dim1=0.999 dim2=0.996 | Test C cor(mean IP): dim1=-0.999 dim2=0.996 | Test C beta1 flip cor (baseline vs -swapped): 0.998 | OK
- `security`: theory items: none found in V7 (Set A empty) | Test B cor(mean IP): dim1=0.789 dim2=0.636 | Test C cor(mean IP): dim1=-0.789 dim2=0.636 | Test C beta1 flip cor (baseline vs -swapped): 0.999 | LOW Test B dim1 correlation; WEAK dim1 flip under swapped anchors
- `environment`: theory items positive in V7: 2/2 | Test B cor(mean IP): dim1=1.000 dim2=0.997 | Test C cor(mean IP): dim1=-1.000 dim2=0.997 | Test C beta1 flip cor (baseline vs -swapped): 0.997 | OK
- `human_rights`: theory items positive in V7: 0/1 | Test B cor(mean IP): dim1=0.977 dim2=0.986 | Test C cor(mean IP): dim1=-0.979 dim2=0.985 | Test C beta1 flip cor (baseline vs -swapped): 0.993 | THEORY sign mismatch in V7
- `arms_control`: theory items positive in V7: 2/2 | Test B cor(mean IP): dim1=0.983 dim2=1.000 | Test C cor(mean IP): dim1=-0.983 dim2=1.000 | Test C beta1 flip cor (baseline vs -swapped): 0.982 | OK
- `intellectual_property`: theory items positive in V7: 1/1 | Test B cor(mean IP): dim1=0.997 dim2=0.999 | Test C cor(mean IP): dim1=-0.997 dim2=0.999 | Test C beta1 flip cor (baseline vs -swapped): 0.996 | OK

## Notes
- Some domains may not contain the theory-driven treaty names in `data/processed/item_codebook.csv` (e.g., investment/security codebooks may not carry those labels). In those cases, Set A can be empty by design and Test A becomes non-informative for that domain.
