# Task: V5 Per-Domain 2D Estimation — Batch 2

## Working Directory

```
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
```

## What To Do

Run the V5 estimation script on 3 treaty domains:

```bash
Rscript scripts/R/v5_per_domain_2d.R security environment intellectual_property
```

This runs `dynIRT_KD` with K=2 (2-dimensional dynamic probit IRT) on each domain's flow matrix. Expected runtime: 5–30 minutes total, depending on domain size.

- **security**: 164 countries × 862 items (MEDIUM)
- **environment**: 206 countries × 1,849 items (LARGE — will be slowest)
- **intellectual_property**: 179 countries × 26 items (SMALL — may warn about weak identification)

## What To Report

After the script finishes:

1. Show the last 30 lines of the log file in `logs/v5_per_domain_2d_*.log`
2. For each domain, report: converged? iterations? runtime?
3. If any domain FAILED, show the full error message from the log
4. Verify output files exist in `outputs/v5_per_domain_2d/`:
   - `security_2d_results.rds`
   - `environment_2d_results.rds`
   - `intellectual_property_2d_results.rds`

## Constraints

- Do **NOT** modify any source code files (`scripts/R/*.R`)
- Do **NOT** install packages — all dependencies are already available
- Do **NOT** create any new files other than what the script produces
- If R is not found, try `/usr/local/bin/Rscript` or report the error
