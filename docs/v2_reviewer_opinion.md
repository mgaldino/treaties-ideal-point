# Peer Review: V2 Equivalence Diagnostic Report

**Reviewer:** Reviewer 2 (Simulated Persona: Kosuke Imai, Harvard University)
**Date:** February 6, 2026
**Target Document:** `docs/v2_diagnostic_report.md`

## Overview

I have reviewed the diagnostic report regarding the failed replication of the dynamic IRT model (`dynIRT_KD` vs. `emIRT`). While the authors correctly identify that the replication failed—and I must emphasize that a correlation of $r \approx 0.97$ is unacceptable for a numerical equivalence study where $r > 0.999$ is the expected standard—the diagnosis presented lacks the methodological rigor required for statistical computing validation.

My assessment identifies several critical flaws in the current diagnostic approach:

## 1. The "Convergence" Analysis is Insufficient

The report states that `dynIRT_KD` "stalled" at a delta of $\sim 0.002$ while `emIRT` converged in 102 iterations. The authors attribute this to "surface flatness" or "numerical accumulation." This interpretation is likely incorrect.

In Variational Bayes, if the algorithm fails to converge to a tight tolerance (e.g., $10^{-6}$), it strongly suggests a **bug in the M-step derivation or implementation**, not merely a difficult optimization landscape. If the Evidence Lower Bound (ELBO) is not monotonically increasing, the code is fundamentally flawed. For a $K=1$ model, the Kalman filter reduces to scalar arithmetic; it is highly improbable that floating-point errors are responsible for a correlation gap of 0.03.

## 2. Identification and Missing Data

The report hypothesizes a discrepancy in missing data handling (`0` vs `NA`). This is not a "detail"—it is central to model identification.
*   In `emIRT`, data points coded as `0` are typically treated as missing (Missing At Random).
*   However, if `emIRT`'s C++ backend handles the normalization constants for the variational distribution differently than your R implementation for these missing entries, the resulting scales will differ.
*   **Action:** You cannot compare ideal points if the scale is unidentified. Verify the exact handling of `0` in `emIRT`'s source code immediately.

## 3. Prior Parametrization (The Most Plausible Error)

Hypothesis 3 in the report suggests a confusion between Prior Variance and Precision. This is the most plausible explanation for the failure, and it is a critical error.
*   If `emIRT` expects precision ($	au = 1/\sigma^2$) but receives variance ($\sigma^2$), or vice-versa, the regularization term will be incorrect by orders of magnitude (e.g., a factor of $25^2 = 625$).
*   This perfectly explains the "sluggish" convergence and the drift in $\beta$ parameters ($r=0.95$, the lowest metric).
*   **Critique:** This should be the *first* item investigated, not the third. A mismatch here invalidates the entire gradient descent path.

## 4. Computational Efficiency

The reported runtime difference (1.2s vs 70.8s, a ~60x slowdown) is alarming. While interpreted languages (R) incur overhead versus compiled languages (C++), a factor of 60x for a vectorized implementation suggests gross inefficiency, likely in the `m_step` memory allocation or loop structures. For a project aiming to scale to $K>1$, this performance is disqualifying.

## Recommendations

I cannot recommend accepting this module ("Phase V2") in its current state. The authors must strictly adhere to the following remedial plan:

1.  **Definitively Confirm Prior Parametrization:** Do not guess. Consult the `emIRT` C++ source code or documentation to confirm whether it minimizes $- \log P(	heta)$ (using precision) or simply adds a penalty term scaled by variance.
2.  **Force Convergence:** Run the algorithm with a strict threshold ($10^{-8}$) and very tight priors. If the parameters drift endlessly, the model is not identified.
3.  **Trace Plots:** Stop reporting single-point summaries. Plot the trace of the parameters over iterations. Linear drift indicates an identification problem; oscillation indicates a step-size or curvature problem.

**Decision:** **Major Revision Required.** Do not proceed to $K>1$ implementation until these fundamental discrepancies are resolved.

---
*Signed,*
*Reviewer 2*
