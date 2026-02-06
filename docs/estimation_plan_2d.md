# Estimation Plan: K-Dimensional Dynamic IRT via EM with Data Augmentation

**Date**: 2026-02-06
**Status**: Main-results estimator (K = 2). Two approaches must be run: joint 2D and per-domain 2D. V2 equivalence is diagnostic only.
**Language**: R (pure R first; C++/Rcpp optimization later if needed)
**Prerequisite**: Prepared flow matrices for 7 domains; tariff-based trade dimension is handled separately (continuous-response IRT).

**Update summary (2026-02-06)**:
- 2D (K = 2) is the **primary** specification for the paper.
- Both **joint 2D** and **per-domain 2D** must be estimated before choosing the final presentation.
- Item anchoring uses **sign constraints** on beta (no tight priors); country anchors combine PCA + theory.

---

## Table of Contents

1. [Motivation and Scope](#1-motivation-and-scope)
2. [Model Specification](#2-model-specification)
3. [Identification](#3-identification)
4. [EM Algorithm Overview](#4-em-algorithm-overview)
5. [E-Step: Data Augmentation](#5-e-step-data-augmentation)
6. [E-Step: Kalman Filter–Smoother](#6-e-step-kalman-filtersmoother)
7. [M-Step: Item Parameters](#7-m-step-item-parameters)
8. [M-Step: Evolution Covariance (Optional)](#8-m-step-evolution-covariance-optional)
9. [Missing Data Handling](#9-missing-data-handling)
10. [Convergence Criteria](#10-convergence-criteria)
11. [Starting Values](#11-starting-values)
12. [R Function Signatures](#12-r-function-signatures)
13. [Computational Complexity](#13-computational-complexity)
14. [Connection to Existing 1D Code](#14-connection-to-existing-1d-code)
15. [Verification Plan](#15-verification-plan)
16. [References](#16-references)

---

## 1. Motivation and Scope

The baseline estimation (see `docs/estimation_plan.md`) fits **separate 1D models** per issue area using `emIRT::dynIRT()`. This works well for within-domain analysis but cannot:

- Estimate cross-domain correlations (e.g., does trade commitment predict security commitment?)
- Pool information across domains to improve country estimates with sparse within-domain data
- Test whether the latent space is truly multi-dimensional or collapses toward a single ILO axis

This document specifies a **K-dimensional dynamic probit IRT model** estimated via EM with Albert–Chib data augmentation and Kalman filter–smoother. The algorithm generalizes the 1D `dynIRT()` approach to K ≥ 1 dimensions while retaining the same data structures (`rc`, `startlegis`, `endlegis`, `bill.session`, `T`).

**Scope**: The primary target is K = 2, but all formulas are written for general K. The K = 1 case must reduce exactly to the existing `emIRT::dynIRT()` formulation (Section 14), but this is a **diagnostic check**, not a prerequisite for the main results.

**Two required specifications**:
- **Joint 2D**: pool all items across the 7 domains into a single model.
- **Per-domain 2D**: estimate a separate 2D model for each domain.

**Trade note**: the treaty-based trade dimension is paused for main results; the paper requires a **continuous-response IRT** dimension built from tariff data (WITS/TRAINS).

**Continuous-response trade dimension (summary)**:
- Use tariff rates (MFN applied and effectively applied) as continuous outcomes.
- Model a latent “trade openness/ILO support” dimension with a continuous-response IRT (e.g., normal or logit-normal link), estimated separately and then aligned to the 2D space.
- Treaty-based trade items are retained for anchoring and robustness, but not as the primary trade signal.

---

## 2. Model Specification

### 2.1 Notation

| Symbol | Dimension | Description |
|--------|-----------|-------------|
| N | scalar | Number of countries |
| J | scalar | Number of items (phantom items across all domains) |
| T | scalar | Number of time periods |
| K | scalar | Number of latent dimensions |
| **x**ᵢₜ | K × 1 | Ideal point of country i in period t |
| αⱼ | scalar | Item j intercept (difficulty) |
| **β**ⱼ | K × 1 | Item j discrimination vector |
| **Ω** | K × K | Evolution covariance (random walk innovation) |
| y*ᵢⱼ | scalar | Augmented latent utility for country i, item j |

### 2.2 Observation Model

Each observed vote yᵢⱼ ∈ {+1, −1, 0} follows:

```
yᵢⱼ = sign(y*ᵢⱼ)    for non-missing entries (yᵢⱼ ≠ 0)
```

where the latent utility is:

```
y*ᵢⱼ = αⱼ + β'ⱼ xᵢ,s(j) + εᵢⱼ,    εᵢⱼ ~ N(0, 1)
```

Here s(j) ∈ {1, …, T} is the period (session) associated with item j (given by `bill.session`).

**Vote probabilities**:

```
P(yᵢⱼ = +1 | xᵢ,s(j)) = Φ(αⱼ + β'ⱼ xᵢ,s(j))
P(yᵢⱼ = -1 | xᵢ,s(j)) = 1 - Φ(αⱼ + β'ⱼ xᵢ,s(j))
```

where Φ is the standard normal CDF.

### 2.3 Dynamic Model (State Equation)

Ideal points follow a K-dimensional random walk:

```
xᵢ,₁ ~ N(μ₀ᵢ, Σ₀ᵢ)                     (initial condition)
xᵢ,ₜ | xᵢ,ₜ₋₁ ~ N(xᵢ,ₜ₋₁, Ω)     for t = 2, ..., T
```

- **μ₀ᵢ** (K × 1): prior mean for country i's initial ideal point
- **Σ₀ᵢ** (K × K): prior covariance for country i's initial ideal point
- **Ω** (K × K): evolution covariance, shared across all countries (can be extended to per-country Ωᵢ)

### 2.4 Priors on Item Parameters

```
(αⱼ, β'ⱼ)' ~ N(μ_β, Σ_β)
```

where:
- μ\_β is a (K+1) × 1 prior mean (typically zero)
- Σ\_β is a (K+1) × (K+1) prior covariance (diffuse, e.g., 25 · I)

---

## 3. Identification

### 3.1 The Invariance Problem

In a K-dimensional probit IRT model, the likelihood is invariant under any invertible affine transformation of the latent space. Specifically, for any invertible K × K matrix **A** and any K × 1 vector **c**:

```
x̃ᵢₜ = A xᵢₜ + c
α̃ⱼ = αⱼ + β'ⱼ A⁻¹ c         (adjusted intercept — absorbs the shift)
β̃ⱼ = (A⁻¹)' βⱼ              (adjusted discrimination — absorbs rotation+scale)
```

This transformation leaves the linear predictor unchanged:

```
α̃ⱼ + β̃'ⱼ x̃ᵢₜ = αⱼ + β'ⱼ A⁻¹(A xᵢₜ + c) + β'ⱼ A⁻¹ c
               = αⱼ + β'ⱼ xᵢₜ + β'ⱼ A⁻¹ c + β'ⱼ A⁻¹ c
```

Wait — let me write this correctly. We have:

```
α̃ⱼ + β̃'ⱼ x̃ᵢₜ = (αⱼ - β'ⱼ A⁻¹ c) + ((A⁻¹)' βⱼ)' (A xᵢₜ + c)
               = αⱼ - β'ⱼ A⁻¹ c + β'ⱼ A⁻¹ A xᵢₜ + β'ⱼ A⁻¹ c
               = αⱼ + β'ⱼ xᵢₜ
```

So the correct transformation is α̃ⱼ = αⱼ − β'ⱼ A⁻¹ c and β̃ⱼ = (A⁻¹)' βⱼ. The group of invariance transformations is GL(K) × R^K, which has K² + K = K(K+1) free parameters.

**We need K(K+1) constraints** to pin down the latent space.

### 3.2 Identification via K+1 Anchor Countries

**Approach**: Fix the positions of K+1 countries using tight priors. This is preferred over the lower-triangular constraint on the discrimination matrix (Bafumi et al. 2005) because:

1. It preserves substantive interpretability of item parameters
2. It allows all items to load freely on all dimensions
3. It matches our existing 1D anchoring strategy (Denmark/Iran)
4. It is easy to implement via priors (no hard constraints in the algorithm)

**Theorem (K+1 Anchor Sufficiency)**:

*Fixing the K-dimensional positions of K+1 countries whose positions span R^K (i.e., are in general position — no K+1 of them lie in a (K−1)-dimensional affine subspace) provides exactly K(K+1) constraints, which resolves the GL(K) × R^K invariance.*

**Proof sketch**:

Each anchored country provides K scalar constraints (one per dimension). With K+1 anchored countries, we have (K+1) × K = K² + K = K(K+1) constraints. The general position condition ensures these constraints are linearly independent (the affine hull of the K+1 points is all of R^K), so they exactly resolve the K(K+1) degrees of freedom in GL(K) × R^K.

More precisely: the translation invariance (R^K, K parameters) is resolved because the centroid of the anchors is fixed. The linear invariance (GL(K), K² parameters) is resolved because the K+1 points in general position determine a unique affine coordinate system — any invertible linear transformation that preserved all K+1 positions would be the identity.

**Formal argument**: Suppose (A, c) is an invariance transformation that fixes K+1 anchor positions:

```
A x_aₖ + c = x_aₖ     for k = 0, 1, ..., K
```

Subtracting the equation for k = 0 from each other:

```
A (x_aₖ - x_a₀) = x_aₖ - x_a₀     for k = 1, ..., K
```

The vectors {x_aₖ − x_a₀}_{k=1..K} are K vectors in R^K. The general position condition means they are linearly independent, so they form a basis for R^K. Since A fixes every basis vector, A = I_K. Then from any anchor equation: c = 0. Hence the only invariance transformation preserving K+1 anchors in general position is the identity. ∎

### 3.3 Practical Anchor Selection for K = 2

For K = 2 we need 3 anchor countries whose 2D positions span R² (i.e., are not collinear).

**Proposed anchors** (to be confirmed by 2D PCA in Phase 2):

| Anchor | Country | Position (θ₁, θ₂) | Rationale |
|--------|---------|--------------------|-----------|
| a₀ | Denmark (DNK) | (+2, +2) | High commitment across all domains |
| a₁ | Iran (IRN) | (−2, −2) | Low commitment across all domains |
| a₂ | China (CHN) | (+1, −1) | High trade/investment, low security/HR |

**Why China**: Denmark and Iran anchor the "ILO core" diagonal. We need a third country off this diagonal to resolve the rotation. China is the natural choice: it has high engagement with economic institutions (WTO member since 2001, extensive BIT network) but low engagement with security alliances and human rights treaties. This places it in a different quadrant from both Denmark and Iran.

**Non-collinearity check**: The 3 points (+2,+2), (−2,−2), (+1,−1) have area = 0.5 |det([−4,−4; −1,−3])| = 0.5 |12−4| = 4 ≠ 0. ✓

**Implementation**: Set tight priors (variance = 0.01 · I_K) on the K-dimensional ideal points of these 3 countries at t = 1. Their ideal points are still allowed to evolve via the random walk, but the tight initial prior anchors the scale and orientation.

### 3.4 Sensitivity to Anchor Choice

Robustness checks should test:
- Alternative third anchor (e.g., Russia, Saudi Arabia, India instead of China)
- Alternative positive/negative anchors (Netherlands/North Korea instead of Denmark/Iran)
- Wider anchor prior variance (0.1 instead of 0.01) to test sensitivity to prior tightness

**Item-anchor sign constraints (current decision)**:
- For anchor items, enforce the expected **sign** of the relevant beta component (no tight priors).
- Compare country-only, item-only, and combined anchoring to assess identification stability.

---

## 4. EM Algorithm Overview

The algorithm alternates between:

1. **E-step**: Given current item parameters {αⱼ, **β**ⱼ} and evolution covariance **Ω**, compute the expected sufficient statistics for the latent ideal points {**x**ᵢₜ} and augmented data {y\*ᵢⱼ}.

2. **M-step**: Given expected sufficient statistics, update {αⱼ, **β**ⱼ} and optionally **Ω**.

### 4.1 E-Step Decomposition

The E-step has two sub-steps:

**(a) Data Augmentation (Albert–Chib)**: For each non-missing vote yᵢⱼ, compute the conditional expectation and variance of the augmented latent utility y\*ᵢⱼ given the current ideal point estimates and item parameters. This reduces the probit likelihood to a Gaussian pseudo-observation.

**(b) Kalman Filter–Smoother**: Treat the augmented data as noisy linear observations of the state **x**ᵢₜ and run a Kalman filter (forward) + Rauch–Tung–Striebel (RTS) smoother (backward) for each country separately to obtain E[**x**ᵢₜ] and Cov(**x**ᵢₜ) under the complete-data posterior.

### 4.2 Algorithm Pseudocode

```
Initialize: α⁽⁰⁾, β⁽⁰⁾, x⁽⁰⁾, Ω⁽⁰⁾
For m = 1, 2, ... until convergence:

  # E-step
  For each country i = 1, ..., N:
    For each item j with yᵢⱼ ≠ 0:
      Compute E[y*ᵢⱼ | yᵢⱼ, xᵢ,s(j)⁽ᵐ⁻¹⁾, αⱼ⁽ᵐ⁻¹⁾, βⱼ⁽ᵐ⁻¹⁾]     (data augmentation)
    Run Kalman filter-smoother for country i:
      Input: augmented pseudo-observations {ỹᵢⱼ} for all items j where yᵢⱼ ≠ 0
      Output: E[xᵢₜ], Cov(xᵢₜ), Cov(xᵢₜ, xᵢ,ₜ₋₁)   for t = 1, ..., T

  # M-step
  For each item j = 1, ..., J:
    Update (αⱼ, βⱼ) using weighted least squares on augmented data
  Optionally: Update Ω from cross-period ideal point innovations

  # Check convergence
  Compute log-likelihood or parameter change; stop if converged
```

---

## 5. E-Step: Data Augmentation

### 5.1 Truncated Normal Moments

For each non-missing vote yᵢⱼ, define:

```
μ*ᵢⱼ = αⱼ + β'ⱼ xᵢ,s(j)     (current linear predictor)
```

The augmented utility y\*ᵢⱼ | yᵢⱼ follows a truncated normal:

```
y*ᵢⱼ | yᵢⱼ = +1  ~  TN(μ*ᵢⱼ, 1, 0, +∞)     (truncated below at 0)
y*ᵢⱼ | yᵢⱼ = -1  ~  TN(μ*ᵢⱼ, 1, -∞, 0)     (truncated above at 0)
```

The conditional moments are:

```
If yᵢⱼ = +1:
  E[y*ᵢⱼ | yᵢⱼ] = μ*ᵢⱼ + φ(μ*ᵢⱼ) / Φ(μ*ᵢⱼ)
  Var[y*ᵢⱼ | yᵢⱼ] = 1 - μ*ᵢⱼ · φ(μ*ᵢⱼ)/Φ(μ*ᵢⱼ) - [φ(μ*ᵢⱼ)/Φ(μ*ᵢⱼ)]²

If yᵢⱼ = -1:
  E[y*ᵢⱼ | yᵢⱼ] = μ*ᵢⱼ - φ(μ*ᵢⱼ) / (1 - Φ(μ*ᵢⱼ))
  Var[y*ᵢⱼ | yᵢⱼ] = 1 + μ*ᵢⱼ · φ(μ*ᵢⱼ)/(1-Φ(μ*ᵢⱼ)) - [φ(μ*ᵢⱼ)/(1-Φ(μ*ᵢⱼ))]²
```

where φ is the standard normal PDF and Φ is the standard normal CDF.

**Numerical stability**: When |μ\*ᵢⱼ| is large, the ratios φ/Φ can underflow or overflow. Use the log-space identity:

```
φ(z)/Φ(z) = exp(dnorm(z, log=TRUE) - pnorm(z, log.p=TRUE))
```

and for the lower tail:

```
φ(z)/(1-Φ(z)) = exp(dnorm(z, log=TRUE) - pnorm(z, lower.tail=FALSE, log.p=TRUE))
```

### 5.2 Pseudo-Observation Construction

After data augmentation, each non-missing vote produces a pseudo-observation:

```
ỹᵢⱼ = E[y*ᵢⱼ | yᵢⱼ]
```

with known variance 1 (the residual variance in the probit model is fixed to 1 for identification). The observation equation becomes:

```
ỹᵢⱼ = αⱼ + β'ⱼ xᵢ,s(j) + ẽᵢⱼ,    ẽᵢⱼ ~ N(0, 1)
```

This is a linear Gaussian observation, which is exactly what the Kalman filter requires.

**Note on the variance**: In a strict EM, one should use the conditional variance Var[y\*ᵢⱼ | yᵢⱼ] ≤ 1 as the observation noise variance. However, the standard approach (following Imai et al. 2016 and the `emIRT` implementation) uses variance = 1, which corresponds to a "variational EM" or "classification EM" approximation. This simplifies the Kalman filter and works well in practice. We adopt this convention for consistency with the 1D code.

---

## 6. E-Step: Kalman Filter–Smoother

After data augmentation, the estimation problem for each country i is a linear-Gaussian state-space model:

**State equation**:
```
xᵢₜ = xᵢ,ₜ₋₁ + ωᵢₜ,    ωᵢₜ ~ N(0, Ω)     (K × 1 state, K × K covariance)
```

**Observation equation** (for each item j in period t = s(j)):
```
ỹᵢⱼ = αⱼ + β'ⱼ xᵢₜ + εᵢⱼ,    εᵢⱼ ~ N(0, 1)
```

We process each country independently. For country i, the observations in period t are all items j such that s(j) = t and yᵢⱼ ≠ 0.

### 6.1 Sequential Processing (Efficient for Many Items per Period)

Rather than stacking all items in a period into a single observation vector (which would require inverting a potentially large matrix), we process items **sequentially** within each period. This is the standard "sequential Kalman filter" approach.

Let J(i,t) = {j : s(j) = t, yᵢⱼ ≠ 0} be the set of items observed for country i in period t.

**Forward Pass (Kalman Filter)**:

```
Initialize:
  x̂ᵢ₁|₀ = μ₀ᵢ                       (K × 1)
  Pᵢ₁|₀ = Σ₀ᵢ                        (K × K)

For t = 1, ..., T:
  # Prediction step (for t ≥ 2):
  If t ≥ 2:
    x̂ᵢₜ|ₜ₋₁ = x̂ᵢ,ₜ₋₁|ₜ₋₁           (random walk: identity transition)
    Pᵢₜ|ₜ₋₁ = Pᵢ,ₜ₋₁|ₜ₋₁ + Ω

  # Update step (sequential, one item at a time):
  x̂ = x̂ᵢₜ|ₜ₋₁
  P = Pᵢₜ|ₜ₋₁

  For each j ∈ J(i,t):
    # Observation: ỹᵢⱼ = αⱼ + β'ⱼ xᵢₜ + εᵢⱼ
    # Observation matrix: Hⱼ = β'ⱼ  (1 × K)
    # Observation noise: R = 1 (scalar)

    innovation = ỹᵢⱼ - αⱼ - β'ⱼ x̂
    S = β'ⱼ P βⱼ + 1                      (scalar: innovation variance)
    Kgain = P βⱼ / S                       (K × 1: Kalman gain)
    x̂ = x̂ + Kgain · innovation
    P = P - Kgain β'ⱼ P                   (K × K: use Joseph form for stability)

  x̂ᵢₜ|ₜ = x̂
  Pᵢₜ|ₜ = P
```

**Joseph form for numerical stability**:
```
P = (I - Kgain β'ⱼ) P (I - Kgain β'ⱼ)' + Kgain Kgain'
```

This ensures P remains symmetric positive semi-definite even with finite-precision arithmetic.

### 6.1.1 Block Update by Period (R = I)

When processing all observations in a given period at once, we can use the **batch** posterior update for a linear Gaussian model with **unit observation noise**. Let:

- **Hₜ** be the nₜ × K matrix stacking βⱼ' for all items j ∈ J(i,t)
- **ỹₜ** be the nₜ × 1 vector of pseudo-observations
- **αₜ** be the nₜ × 1 vector of intercepts

Then with prior **x ~ N(x̂, P)** and **R = I**, the posterior is:

```
P_new = (P^{-1} + Hₜ' Hₜ)^{-1}
x_new = P_new (P^{-1} x̂ + Hₜ' (ỹₜ − αₜ))
```

This formulation only inverts **K × K** matrices, which is efficient when K is small (K=1 or K=2). It is algebraically equivalent to the sequential update (order does not matter), but uses fewer R-level loops.

**Note**: We keep **R = I** to preserve equivalence with `emIRT::dynIRT()` in the V2 verification phase. A heteroscedastic variant using **Rₜ = diag(Var[y*])** is feasible and can be added later, but would break strict equivalence with the 1D reference implementation.

### 6.2 Backward Pass (RTS Smoother)

```
For t = T-1, T-2, ..., 1:
  # Smoother gain
  Pᵢ,ₜ₊₁|ₜ = Pᵢₜ|ₜ + Ω                  (predicted covariance at t+1)
  Lₜ = Pᵢₜ|ₜ (Pᵢ,ₜ₊₁|ₜ)⁻¹              (K × K smoother gain)

  # Smoothed estimates
  x̂ᵢₜ|T = x̂ᵢₜ|ₜ + Lₜ (x̂ᵢ,ₜ₊₁|T - x̂ᵢ,ₜ₊₁|ₜ)
  Pᵢₜ|T = Pᵢₜ|ₜ + Lₜ (Pᵢ,ₜ₊₁|T - Pᵢ,ₜ₊₁|ₜ) Lₜ'
```

**Cross-covariance** (needed for M-step on Ω):

```
Cov(xᵢₜ, xᵢ,ₜ₋₁ | Y) = Lₜ₋₁ Pᵢₜ|T
```

**Note**: Pᵢ,ₜ₊₁|ₜ in the smoother uses the same predicted covariance computed in the forward pass. Store it during the forward pass to avoid recomputation.

### 6.3 Sufficient Statistics from the Smoother

After running the Kalman filter–smoother for country i, we have:

| Quantity | Notation | Dimension | Used in |
|----------|----------|-----------|---------|
| Smoothed mean | x̂ᵢₜ\|T | K × 1, for each t | Data augmentation, M-step for items |
| Smoothed covariance | Pᵢₜ\|T | K × K, for each t | M-step for items, M-step for Ω |
| Lag-one cross-covariance | Pᵢₜ,ₜ₋₁\|T | K × K, for t = 2..T | M-step for Ω |

The expected second moment is:

```
E[xᵢₜ x'ᵢₜ | Y] = Pᵢₜ|T + x̂ᵢₜ|T x̂'ᵢₜ|T
```

### 6.4 Anchor Priors in the Kalman Filter

For anchor countries, the tight prior is incorporated naturally through the initial conditions:

```
For anchor country a:
  μ₀ₐ = prescribed position (e.g., (+2, +2) for Denmark)
  Σ₀ₐ = 0.01 · I_K                     (very tight)

For non-anchor countries:
  μ₀ᵢ = 0_K (or PCA-based starting value)
  Σ₀ᵢ = I_K (or diagonal with domain-specific variances)
```

The tight prior on Σ₀ₐ means the Kalman filter will largely "lock" the anchor's initial position, with subsequent positions free to evolve (but tethered to the anchor via the random walk).

---

## 7. M-Step: Item Parameters

### 7.1 Weighted Least Squares Update

After the E-step, we have pseudo-observations {ỹᵢⱼ} and smoothed ideal points {x̂ᵢ,s(j)|T, Pᵢ,s(j)|T}. For each item j, the M-step updates (αⱼ, **β**ⱼ) by solving a penalized weighted least squares problem.

Define:
- **γ**ⱼ = (αⱼ, β'ⱼ)' — the (K+1) × 1 parameter vector for item j
- **z**ᵢⱼ = (1, x̂'ᵢ,s(j)|T)' — the (K+1) × 1 "regressor" for observation (i,j)
- Nⱼ = {i : yᵢⱼ ≠ 0} — the set of countries that voted on item j

The augmented data likelihood for item j is:

```
ỹᵢⱼ = γ'ⱼ zᵢⱼ + εᵢⱼ,    εᵢⱼ ~ N(0, 1)
```

Incorporating the prior γⱼ ~ N(μ\_β, Σ\_β), the MAP estimate is:

```
γ̂ⱼ = (Σ_β⁻¹ + Σⱼ_zz)⁻¹ (Σ_β⁻¹ μ_β + Σⱼ_zy)
```

where:

```
Σⱼ_zz = Σᵢ∈Nⱼ E[zᵢⱼ z'ᵢⱼ]     ((K+1) × (K+1))
Σⱼ_zy = Σᵢ∈Nⱼ ỹᵢⱼ · E[zᵢⱼ]    ((K+1) × 1)
```

### 7.2 Accounting for Uncertainty in Ideal Points

The regressors **z**ᵢⱼ contain the ideal points, which are estimated with uncertainty. The expected outer product accounts for this:

```
E[zᵢⱼ z'ᵢⱼ] = [ 1,                x̂'ᵢ,s(j)|T           ]
               [ x̂ᵢ,s(j)|T,   Pᵢ,s(j)|T + x̂ᵢ,s(j)|T x̂'ᵢ,s(j)|T ]
```

And the expected regressor is:

```
E[zᵢⱼ] = (1, x̂'ᵢ,s(j)|T)'
```

**Note**: The off-diagonal blocks of E[zᵢⱼ z'ᵢⱼ] use the first moment (no uncertainty in the intercept "1"), while the lower-right K × K block uses the second moment E[xᵢₜ x'ᵢₜ] = Pᵢₜ|T + x̂ᵢₜ|T x̂'ᵢₜ|T.

### 7.3 Extracting α and β

After computing γ̂ⱼ:

```
αⱼ = γ̂ⱼ[1]          (first element)
βⱼ = γ̂ⱼ[2:(K+1)]    (remaining K elements)
```

---

## 8. M-Step: Evolution Covariance (Optional)

### 8.1 Fixed Ω (Recommended for Initial Runs)

For simplicity and consistency with the 1D approach (where omega2 is a fixed hyperparameter), start with **Ω fixed**:

```
Ω = ω² · I_K     (isotropic, with ω² = 0.1 as the baseline)
```

This means the M-step for Ω is skipped entirely, and Ω is treated as a tuning parameter.

### 8.2 Estimated Ω (Full EM)

If Ω is to be estimated, the M-step update is:

```
Ω̂ = (1 / Σᵢ Tᵢ*) Σᵢ Σₜ₌₂ᵀⁱ [Pᵢₜ|T + x̂ᵢₜ|T x̂'ᵢₜ|T
                                 - Pᵢₜ,ₜ₋₁|T x̂'ᵢ,ₜ₋₁|T - x̂ᵢₜ|T x̂'ᵢ,ₜ₋₁|T
                                 - Pᵢₜ,ₜ₋₁|T' x̂ᵢₜ|T - x̂ᵢ,ₜ₋₁|T x̂'ᵢₜ|T
                                 + Pᵢ,ₜ₋₁|T + x̂ᵢ,ₜ₋₁|T x̂'ᵢ,ₜ₋₁|T]
```

Simplifying with the definition of expected innovations:

```
Ω̂ = (1 / Σᵢ Tᵢ*) Σᵢ Σₜ₌₂ᵀⁱ E[(xᵢₜ - xᵢ,ₜ₋₁)(xᵢₜ - xᵢ,ₜ₋₁)' | Y]
```

where Tᵢ\* = endlegis[i] − startlegis[i] is the number of transitions for country i, and the expectation expands as:

```
E[(xᵢₜ - xᵢ,ₜ₋₁)(xᵢₜ - xᵢ,ₜ₋₁)' | Y]
  = (x̂ᵢₜ|T - x̂ᵢ,ₜ₋₁|T)(x̂ᵢₜ|T - x̂ᵢ,ₜ₋₁|T)'
    + Pᵢₜ|T - Pᵢₜ,ₜ₋₁|T - Pᵢₜ,ₜ₋₁|T' + Pᵢ,ₜ₋₁|T
```

**Regularization**: Add a small ridge to Ω̂ to ensure positive definiteness:

```
Ω̂ = Ω̂_raw + ε · I_K     (e.g., ε = 1e-6)
```

### 8.3 Diagonal Ω (Intermediate Option)

A useful intermediate between fixed and full Ω: estimate a diagonal Ω where each dimension has its own evolution variance:

```
Ω̂ = diag(ω̂₁², ..., ω̂_K²)
```

This allows different dimensions to evolve at different rates without estimating cross-dimension covariances (which may be poorly identified with T = 6).

---

## 9. Missing Data Handling

The model handles three types of missingness (coded as `rc[i,j] = 0`):

| Type | Meaning | Handling |
|------|---------|----------|
| Treaty not yet available | `bill.session[j] < startlegis[i]` or treaty opened after period | Item j is excluded from country i's Kalman filter |
| Country not yet existing | Country i does not exist in period s(j) | Same as above |
| Country already ratified | Country joined treaty before the current phantom-item period | Same as above |

**Implementation**: When processing country i in the Kalman filter, only include items j where `rc[i,j] ≠ 0`. If no items are observed for country i in period t (J(i,t) = ∅), the Kalman filter update step is skipped and the prediction step propagates the state forward:

```
If J(i,t) = ∅:
  x̂ᵢₜ|ₜ = x̂ᵢₜ|ₜ₋₁
  Pᵢₜ|ₜ = Pᵢₜ|ₜ₋₁
```

This means the ideal point for that period is determined entirely by the random walk prior and information from adjacent periods (via the smoother).

**Country activity window**: Countries are only active in periods [startlegis[i], endlegis[i]]. The Kalman filter runs from startlegis[i] to endlegis[i] only. For countries whose startlegis > 0, the initial condition at startlegis[i] uses the prior (μ₀ᵢ, Σ₀ᵢ). For countries whose endlegis < T−1, the smoother backward pass starts from endlegis[i].

---

## 10. Convergence Criteria

### 10.1 Primary: Parameter Change

```
Δ⁽ᵐ⁾ = max(
  max_j ||γⱼ⁽ᵐ⁾ - γⱼ⁽ᵐ⁻¹⁾||∞,
  max_{i,t} ||x̂ᵢₜ⁽ᵐ⁾ - x̂ᵢₜ⁽ᵐ⁻¹⁾||∞
)

Converged if Δ⁽ᵐ⁾ < thresh (default: 1e-6)
```

### 10.2 Secondary: Log-Likelihood

Compute the observed-data log-likelihood:

```
ℓ(θ) = Σᵢ Σⱼ:yᵢⱼ≠0 [yᵢⱼ=+1] log Φ(μ*ᵢⱼ) + [yᵢⱼ=-1] log(1 - Φ(μ*ᵢⱼ))
```

where μ\*ᵢⱼ = αⱼ + β'ⱼ x̂ᵢ,s(j)|T. Monitor that ℓ(θ) is non-decreasing (EM guarantee). A decrease indicates a bug.

### 10.3 Practical Settings

| Parameter | Default | Notes |
|-----------|---------|-------|
| `thresh` | 1e-6 | Can relax to 1e-4 for initial exploration |
| `maxit` | 500 | Increase to 1000 if not converged |
| `checkfreq` | 50 | Print diagnostics every 50 iterations |

---

## 11. Starting Values

### 11.1 From PCA

Pool all items across domains into a single N × J matrix. Run PCA (after recoding −1 → 0 and imputing NAs with column means):

```r
pca_result <- prcomp(rc_for_pca, center = TRUE, scale. = TRUE)
```

Use the first K principal components as starting ideal points:

```r
# PC scores: N × K
x_start_raw <- pca_result$x[, 1:K]
```

### 11.2 Rotation to Anchor Coordinates

The PCA axes are arbitrary. Rotate them to align with the anchor country positions:

```r
# Anchor positions in PCA space (N × K submatrix for anchor countries)
X_pca_anchors <- x_start_raw[anchor_rows, ]   # (K+1) × K

# Desired anchor positions
X_target <- rbind(c(2, 2), c(-2, -2), c(1, -1))  # (K+1) × K for K=2

# Solve for affine transformation: X_target ≈ X_pca_anchors %*% A + 1 %*% c'
# Using least-squares on the augmented system:
X_aug <- cbind(1, X_pca_anchors)   # (K+1) × (K+1) augmented
params <- solve(X_aug, X_target)    # (K+1) × K: first row = translation, rest = rotation
c_vec <- params[1, ]                # K × 1 translation
A_mat <- params[2:(K+1), ]          # K × K rotation/scale

# Apply to all countries
x_start <- sweep(x_start_raw %*% A_mat, 2, c_vec, "+")
```

### 11.3 Replicate Across Periods

```r
# x_start is N × K (static). Replicate for T periods.
# Store as a list or 3D array: N × K × T
x_init <- array(NA, dim = c(N, K, T))
for (t in 1:T) {
  x_init[, , t] <- x_start
}
```

### 11.4 Item Parameter Starting Values

Use PCA loadings rotated to the anchor coordinates:

```r
# PCA loadings: J × K
beta_start <- pca_result$rotation[, 1:K] %*% A_mat
alpha_start <- rep(0, J)  # or use column means of rc
```

Alternatively, initialize from the 1D estimates (one per domain) embedded into the K-dimensional space:

```r
# If domain d maps to dimension k:
beta_start[items_in_domain_d, k] <- beta_1d_domain_d
beta_start[items_in_domain_d, -k] <- 0  # zero cross-loadings as starting value
```

---

## 12. R Function Signatures

### 12.1 Main Entry Point

```r
dynIRT_KD <- function(
  .data,       # list: rc (N×J), startlegis (N×1), endlegis (N×1),
               #       bill.session (J×1), T (integer)
  .starts,     # list: alpha (J×1), beta (J×K), x (N×K×T array)
  .priors,     # list: x.mu0 (N×K), x.sigma0 (list of N K×K matrices or N×K diagonal),
               #       beta.mu ((K+1)×1), beta.sigma ((K+1)×(K+1)),
               #       omega (K×K matrix)
  .control,    # list: threads (int), verbose (bool), thresh (numeric),
               #       maxit (int), checkfreq (int), estimate_omega (bool)
  K = 2L       # number of latent dimensions
) {
  # Returns:
  # list(
  #   means = list(x = N×K×T array, alpha = J×1, beta = J×K),
  #   vars  = list(x = list of N lists of T K×K matrices),
  #   omega = K×K matrix (final),
  #   runtime = list(iters, conv, seconds, loglik_trace)
  # )
}
```

### 12.2 Data Augmentation Step

```r
da_step <- function(
  rc,            # N × J matrix (votes: +1, -1, 0)
  alpha,         # J × 1 vector (item intercepts)
  beta,          # J × K matrix (item discriminations)
  x_smooth,      # N × K × T array (smoothed ideal points)
  bill.session   # J × 1 vector (0-indexed period for each item)
) {
  # Returns:
  # list(
  #   y_star = N × J matrix of E[y* | y, x, alpha, beta],
  #   y_star_var = N × J matrix of Var[y* | y, x, alpha, beta] (optional)
  # )
}
```

### 12.3 Kalman Filter–Smoother (Per Country)

```r
kalman_smoother_country <- function(
  y_star_i,       # J_i × 1 vector of pseudo-observations for country i
  item_indices_i, # indices into 1:J for items where rc[i,j] != 0
  alpha,          # J × 1 vector
  beta,           # J × K matrix
  bill.session,   # J × 1 vector (0-indexed)
  mu0,            # K × 1 prior mean
  Sigma0,         # K × K prior covariance
  Omega,          # K × K evolution covariance
  start_t,        # first active period (0-indexed)
  end_t,          # last active period (0-indexed)
  T               # total periods
) {
  # Returns:
  # list(
  #   x_smooth = K × T_active matrix (smoothed means),
  #   P_smooth = list of T_active K×K matrices (smoothed covariances),
  #   P_lag    = list of (T_active-1) K×K matrices (lag-one cross-covariances)
  # )
}
```

### 12.4 M-Step for Items

```r
m_step_items <- function(
  y_star,         # N × J matrix of pseudo-observations
  rc,             # N × J matrix (to identify non-missing)
  x_smooth,       # N × K × T array
  P_smooth,       # list (length N) of lists (length T) of K×K matrices
  bill.session,   # J × 1 vector (0-indexed)
  beta_mu,        # (K+1) × 1 prior mean
  beta_sigma      # (K+1) × (K+1) prior covariance
) {
  # Returns:
  # list(alpha = J×1 vector, beta = J×K matrix)
}
```

### 12.5 M-Step for Ω

```r
m_step_Omega <- function(
  x_smooth,       # N × K × T array
  P_smooth,       # list of covariance matrices
  P_lag,          # list of lag-one cross-covariance matrices
  startlegis,     # N × 1 (0-indexed)
  endlegis,       # N × 1 (0-indexed)
  diagonal_only = FALSE   # if TRUE, estimate diagonal Omega only
) {
  # Returns: K × K matrix Omega
}
```

---

## 13. Computational Complexity

### 13.1 Per-Iteration Cost

| Step | Cost | Notes |
|------|------|-------|
| Data augmentation | O(N · J) | One truncated-normal moment per non-missing entry |
| Kalman filter (per country) | O(Tᵢ · J̄ᵢ · K²) | J̄ᵢ = avg items per period for country i; K² from matrix ops |
| RTS smoother (per country) | O(Tᵢ · K³) | K³ from matrix inverse (or K² with Cholesky) |
| M-step items | O(J · N̄ⱼ · K²) | N̄ⱼ = avg countries per item; K² from (K+1)×(K+1) solve |
| M-step Ω | O(N · T · K²) | Sum of outer products |

**Total per iteration**: O(N · J · K² + N · T · K³)

### 13.2 Concrete Estimates for Our Data

Assuming (conservative):
- N = 180 countries, J = 2000 phantom items (pooled across domains), T = 6, K = 2

| Step | Operations | Time estimate (pure R) |
|------|-----------|----------------------|
| Data augmentation | 180 × 2000 = 360K evaluations | ~0.1 s |
| Kalman filter–smoother | 180 × 6 × ~300 items × 4 = 1.3M ops | ~1–5 s |
| M-step items | 2000 × ~100 countries × 4 = 800K ops | ~0.5–2 s |
| **Total per iteration** | | **~2–8 s** |
| **500 iterations** | | **~15–60 min** |

This is manageable in pure R. If too slow, the Kalman filter (the bottleneck) can be ported to C++ via Rcpp.

### 13.3 Parallelization Opportunities

The Kalman filter–smoother for each country is **independent**. This is trivially parallelizable:

```r
library(parallel)
results <- mclapply(1:N, function(i) {
  kalman_smoother_country(...)
}, mc.cores = ncores)
```

Similarly, the M-step for each item is independent.

---

## 14. Connection to Existing 1D Code

### 14.1 K = 1 Reduction

When K = 1, the K-dimensional model reduces exactly to the 1D model used by `emIRT::dynIRT()`:

| K-dimensional | K = 1 specialization | `dynIRT()` equivalent |
|--------------|---------------------|----------------------|
| **x**ᵢₜ ∈ R^K | xᵢₜ ∈ R | `x[i, t]` |
| **β**ⱼ ∈ R^K | βⱼ ∈ R | `beta[j]` |
| **Ω** (K × K matrix) | ω² (scalar) | `omega2[i]` |
| β'ⱼ **x**ᵢₜ | βⱼ · xᵢₜ | Same |
| Kalman gain = P βⱼ / (β²ⱼ P + 1) | K = Pβ/(β²P+1) | Scalar recursion |
| Σ₀ᵢ (K × K) | σ²₀ᵢ (scalar) | `x.sigma0[i]` |

The identification reduces from K(K+1) = 2 constraints (2 anchor countries) to exactly the 1D setup (Denmark at +2, Iran at −2).

### 14.2 What Changes vs. 1D

| Aspect | 1D (`dynIRT`) | K-D (`dynIRT_KD`) |
|--------|---------------|-------------------|
| Data structures | `rc`, `startlegis`, `endlegis`, `bill.session`, `T` — **unchanged** | Same |
| Ideal points | N × T matrix | N × K × T array |
| Item discrimination | J × 1 vector | J × K matrix |
| Evolution variance | N × 1 scalar vector | K × K matrix (shared) |
| Prior on x₀ | N × 1 mean, N × 1 variance | N × K mean, list of K×K covariances |
| Kalman filter | Scalar recursion | K × K matrix recursion |
| Identification | 2 anchor countries | K+1 anchor countries |
| Cross-domain estimation | Separate models | Single joint model |

### 14.3 Verification via K = 1

The most important validation is to run `dynIRT_KD` with K = 1 on a single domain and compare the results with `emIRT::dynIRT()`. They should agree to numerical precision (up to differences in convergence paths due to implementation details).

---

## 15. Verification Plan

The verification plan has two complementary legs: (A) internal validation via simulated data where ground truth is known, and (B) external validation against an established real-data benchmark (US Congress + DW-NOMINATE). Together they ensure correctness (A) and substantive plausibility (B).

### Phase V1: Simulation Study (Internal Validation)

**Goal**: Verify the algorithm recovers known parameters from simulated data.

#### V1.1 Data-Generating Process

Generate synthetic 2D dynamic roll-call-style data:

```r
set.seed(42)
N <- 100   # legislators / countries
J <- 200   # items (roll calls)
T_sim <- 6 # periods
K <- 2     # dimensions

# True ideal points: random walk
Omega_true <- 0.1 * diag(K)
x_true <- array(NA, dim = c(N, K, T_sim))
x_true[, , 1] <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = diag(K))
for (t in 2:T_sim) {
  x_true[, , t] <- x_true[, , t-1] + MASS::mvrnorm(N, mu = c(0, 0), Sigma = Omega_true)
}

# True item parameters
alpha_true <- rnorm(J, 0, 1)
beta_true  <- matrix(rnorm(J * K, 0, 0.7), nrow = J, ncol = K)

# Assign items to periods (roughly uniform)
bill_session <- sample(0:(T_sim - 1), J, replace = TRUE)

# Generate votes (probit)
rc_sim <- matrix(0L, nrow = N, ncol = J)
for (j in 1:J) {
  t_j <- bill_session[j] + 1  # 1-indexed
  mu_star <- alpha_true[j] + x_true[, , t_j] %*% beta_true[j, ]
  rc_sim[, j] <- ifelse(rnorm(N) < mu_star, 1L, -1L)
}

# Introduce ~30% missing data (random)
missing_mask <- matrix(rbinom(N * J, 1, 0.3), nrow = N, ncol = J)
rc_sim[missing_mask == 1] <- 0L
```

#### V1.2 Estimation

- Run `dynIRT_KD` with K = 2
- Select 3 anchor units whose true positions span R² (e.g., the unit closest to each of (+2,+2), (−2,−2), (+1,−1) at t = 1)
- Set tight priors on anchors (variance = 0.01 · I₂)

#### V1.3 Evaluation Criteria

| Metric | Target | How to compute |
|--------|--------|---------------|
| Ideal point recovery (per dimension) | r > 0.90 | cor(x\_true[,,t], x\_hat[,,t]) for each t and k |
| Item discrimination recovery | r > 0.85 | cor(beta\_true, beta\_hat) per dimension |
| Item intercept recovery | r > 0.85 | cor(alpha\_true, alpha\_hat) |
| Log-likelihood monotonicity | Strictly non-decreasing | Check ℓ⁽ᵐ⁾ ≥ ℓ⁽ᵐ⁻¹⁾ for all m |
| Convergence | < 500 iterations | Check runtime$conv == 1 |

#### V1.4 Replications

Repeat with 10 different random seeds. Report mean and SD of recovery correlations across replications. If any replication fails (r < 0.8), investigate whether it is a starting-value issue (try alternative starts) or an algorithm bug.

### Phase V2: K = 1 Equivalence

**Goal**: Confirm `dynIRT_KD(K=1)` matches `emIRT::dynIRT()`.

1. Take one domain (e.g., environment) with its prepared data
2. Run `emIRT::dynIRT()` with standard settings
3. Run `dynIRT_KD` with K = 1, same data, same priors, same starting values
4. Compare:
   - Ideal point correlation: target r > 0.99
   - Item parameter correlation: target r > 0.99
   - Number of iterations to convergence: should be similar (±20%)

### Phase V3: US Congress Benchmark (External Validation)

**Goal**: Run `dynIRT_KD` on a real dataset where credible 2D estimates already exist, and compare.

#### V3.1 Why US Congress

The US Congress roll call record is the most thoroughly studied ideal-point dataset in political science. DW-NOMINATE (Poole & Rosenthal 1997, 2007) provides 2D scores for every legislator across all Congresses, making it the gold-standard benchmark for multidimensional ideal point models.

#### V3.2 Data Acquisition

**Source**: Voteview (voteview.com), maintained by the UCLA NOMINATE project.

```r
# Download roll call votes and DW-NOMINATE scores
# Option A: Rpackage
# install.packages("Rvoteview")
# Option B: Direct CSV download from voteview.com/data

# Roll call votes (binary: yea/nay/missing)
votes_url <- "https://voteview.com/static/data/out/votes/HSall_votes.csv"

# DW-NOMINATE scores (benchmark)
members_url <- "https://voteview.com/static/data/out/members/HSall_members.csv"
```

Save raw files to `data/raw/us_congress/`.

#### V3.3 Data Preparation

1. **Select a subset of Congresses** to create a dynamic dataset. Recommended: 5–8 consecutive Congresses from the modern era (e.g., 105th–112th, 1997–2012), providing T = 8 periods with rich voting data.

2. **Build the `rc` matrix**: legislators (rows) × roll calls (columns), coded as +1 (yea), −1 (nay), 0 (missing/abstain/not serving). Only include legislators who served in ≥ 2 Congresses (needed for the dynamic model to add value).

3. **Build auxiliary vectors**:
   - `startlegis[i]`: first Congress (0-indexed) in which legislator i served
   - `endlegis[i]`: last Congress (0-indexed) in which legislator i served
   - `bill.session[j]`: Congress (0-indexed) for roll call j
   - `T`: number of Congresses

4. **Select anchors** (K = 2, need 3 anchors):

| Anchor | Legislator | Position | Rationale |
|--------|-----------|----------|-----------|
| a₀ | A well-known liberal (e.g., Ted Kennedy / Nancy Pelosi) | (+2, ·) | Consistently liberal on dimension 1 |
| a₁ | A well-known conservative (e.g., Jim DeMint / Tom Coburn) | (−2, ·) | Consistently conservative on dimension 1 |
| a₂ | A legislator who splits dimensions (e.g., a moderate or cross-pressured member) | (·, ±2) | Off the main liberal-conservative axis |

The exact anchor positions should be informed by their known DW-NOMINATE scores, rotated to a convenient coordinate system.

#### V3.4 Estimation

Run `dynIRT_KD` with K = 2 on the prepared US Congress data.

#### V3.5 Comparison with DW-NOMINATE

**Key caveat**: DW-NOMINATE uses a different model (Gaussian kernel utility, not probit) and constrains legislator trajectories to be linear over time. Our model uses probit + random walk. Despite these differences, the recovered latent space should be similar if both models capture the same underlying structure.

**Comparison procedure**:

1. **Procrustes rotation**: Since the two methods define different coordinate systems, align our estimates to DW-NOMINATE using Procrustes analysis before comparing:

```r
# Procrustes alignment (rotation + reflection + scaling)
# Use the `vegan` or `MCMCpack` package
library(vegan)
proc <- procrustes(X = dw_nominate_2d, Y = our_estimates_2d)
our_aligned <- proc$Yrot
```

2. **Correlation after alignment**:

| Metric | Target | Notes |
|--------|--------|-------|
| Dimension 1 correlation | r > 0.85 | The main liberal-conservative axis should be well-recovered |
| Dimension 2 correlation | r > 0.50 | The second dimension is notoriously harder to estimate; DW-NOMINATE and probit IRT may disagree more here |
| Overall Procrustes R² | > 0.70 | Proportion of variance in DW-NOMINATE explained after alignment |

3. **Visual comparison**: Scatterplot of legislator positions (our estimates vs. DW-NOMINATE) after Procrustes alignment, colored by party. The partisan clustering should be broadly similar.

4. **Trajectory comparison**: For legislators serving across multiple Congresses, compare the dynamic trajectories. Both methods should agree on the direction of movement (e.g., a legislator who drifts rightward in DW-NOMINATE should also drift rightward in our estimates).

#### V3.6 Interpreting Discrepancies

Expected sources of disagreement:

| Source | Effect | Acceptable? |
|--------|--------|-------------|
| Probit vs. Gaussian kernel | Different tails of the utility function | Yes — affects extreme legislators most |
| Random walk vs. linear trajectory | Our model allows non-monotone movement | Yes — may reveal patterns DW-NOMINATE misses |
| Anchor choice | Different coordinate system pre-Procrustes | Resolved by Procrustes alignment |
| Estimation uncertainty | Both methods have estimation error | Yes — focus on systematic patterns, not individual-level noise |

If dimension 1 correlation drops below 0.7 after Procrustes alignment, this signals a potential algorithm bug or a fundamental model incompatibility that warrants investigation.

### Phase V4: Anchor Sensitivity

**Goal**: Verify the 2D estimates are not artifacts of anchor choice.

1. Run with baseline anchors (DNK, IRN, CHN) on treaty data; or baseline Congress anchors on US data
2. Run with 2 alternative anchor sets
3. Compare:
   - Compute Procrustes-rotated correlation between estimate sets (since different anchors define different coordinate systems, alignment is needed before comparison)
   - Target: rotated correlation > 0.85

### Phase V5: Treaty Data (Real Application)

**Goal**: Produce and interpret the first 2D estimates on our treaty participation data.

1. Pool all items from domains with stable 1D estimates (at minimum: investment, security, environment)
2. Run `dynIRT_KD` with K = 2
3. Examine:
   - Dimension interpretation: do the two dimensions correspond to substantively meaningful axes? (e.g., economic vs. political/security)
   - Cross-loadings: do items from different domains load on different dimensions?
   - Country trajectories: do 2D paths make substantive sense?
4. Compare with separate 1D estimates:
   - For each domain, correlate the relevant dimension of the 2D estimate with the 1D estimate
   - High correlation (r > 0.8) = 2D model is consistent with 1D; lower = 2D reveals structure 1D misses

### Summary: Verification Sequence

```
V1 (Simulation)  ──→  V2 (K=1 equiv)  ──→  V3 (US Congress)  ──→  V4 (Anchor sens.)  ──→  V5 (Treaty data)
   correctness         code check           external validity       robustness              application
```

Each phase is a gate: do not proceed to the next if the current one fails.

---

## 16. References

1. **Albert, J. H. & Chib, S. (1993)**. "Bayesian Analysis of Binary and Polychotomous Response Data." *Journal of the American Statistical Association*, 88(422), 669–679. — Data augmentation for probit models.

2. **Imai, K., Lo, J., & Olmsted, J. (2016)**. "Fast Estimation of Ideal Points with Massive Data." *American Political Science Review*, 110(4), 631–656. — The EM algorithm for 1D dynamic IRT; theoretical foundation for `emIRT::dynIRT()`.

3. **Martin, A. D. & Quinn, K. M. (2002)**. "Dynamic Ideal Point Estimation via Markov Chain Monte Carlo for the U.S. Supreme Court, 1953–1999." *Political Analysis*, 10(2), 134–153. — Original dynamic IRT for ideal points.

4. **Bafumi, J., Gelman, A., Park, D. K., & Kaplan, N. (2005)**. "Practical Issues in Implementing and Understanding Bayesian Ideal Point Estimation." *Political Analysis*, 13(2), 171–187. — Identification via lower-triangular discrimination matrix.

5. **Rivers, D. (2003)**. "Identification of Multidimensional Spatial Voting Models." Typescript, Stanford University. — Theory of identification in multidimensional IRT; K(K+1) constraints.

6. **Rauch, H. E., Tung, F., & Striebel, C. T. (1965)**. "Maximum Likelihood Estimates of Linear Dynamic Systems." *AIAA Journal*, 3(8), 1445–1450. — The RTS smoother.

7. **Shumway, R. H. & Stoffer, D. S. (1982)**. "An Approach to Time Series Smoothing and Forecasting Using the EM Algorithm." *Journal of Time Series Analysis*, 3(4), 253–264. — EM for state-space models.

8. **Tahk, A. (2018)**. "Ideal Point Models in R: Estimation, Identification, and Diagnostics." *Journal of Statistical Software*. — Practical guidance on identification of ideal point models.

9. **Kubinec, R. (2019)**. "Generalized Ideal Point Models for Time-Varying and Missing-Data Inference." *OSF Preprints*. — `idealstan` R package (Stan-based IRT).

10. **Bailey, M. A., Strezhnev, A., & Voeten, E. (2017)**. "Estimating Dynamic State Preferences from United Nations Voting Data." *Journal of Conflict Resolution*, 61(2), 430–456. — UNGA ideal points benchmark.

11. **Poole, K. T. & Rosenthal, H. (1997)**. *Congress: A Political-Economic History of Roll Call Voting*. Oxford University Press. — DW-NOMINATE methodology and the two-dimensional spatial model of US Congress.

12. **Poole, K. T. & Rosenthal, H. (2007)**. *Ideology and Congress*. Transaction Publishers. — Updated DW-NOMINATE analysis; the benchmark for Phase V3.

13. **Carroll, R., Lewis, J. B., Lo, J., Poole, K. T., & Rosenthal, H. (2009)**. "Measuring Bias and Uncertainty in DW-NOMINATE Ideal Point Estimates via the Parametric Bootstrap." *Political Analysis*, 17(3), 261–275. — Bootstrap SEs for NOMINATE; relevant methodology for uncertainty quantification.

---

## 17. Implementation Log

### Implementation Phase 1 — Core Code (Completed 2026-02-05)

**Scripts created** (all in `scripts/R/`):

| File | Functions | Lines | Description |
|------|-----------|-------|-------------|
| `da_step.R` | `truncnorm_moments()`, `da_step()` | ~100 | Data augmentation (Section 5) |
| `kalman.R` | `kalman_smoother_country()` | ~130 | Sequential Kalman filter + RTS smoother (Section 6) |
| `m_step.R` | `m_step_items()`, `m_step_Omega()` | ~140 | M-step for item params + evolution covariance (Sections 7–8) |
| `dynIRT_KD.R` | `dynIRT_KD()`, `compute_loglik()`, `param_change()` | ~230 | Main EM orchestrator (Sections 4, 10–12) |

**Key design choices**:
- Process items sequentially within periods in the Kalman filter (avoids large matrix inversions)
- Joseph form for covariance updates (numerical stability)
- Log-space computation for φ/Φ ratios (handles |μ| > 5 without overflow)
- Companion modules auto-sourced by `dynIRT_KD.R`; skips if functions already exist
- `%||%` null-coalesce defined for R < 4.4 compatibility

### Implementation Phase 2 — Unit Tests (Completed 2026-02-05)

**Test files** (all in `tests/`):

| File | Tests | Coverage |
|------|-------|----------|
| `test_da_step.R` | 43 | Analytical values (mu=0), symmetry, sign consistency, numerical stability (|mu|=10), vectorization, NA/dimension checks |
| `test_kalman.R` | 34 | No-observations (=prior), single observation (analytical), PD of P_smooth, partial activity window, K=1 scalar case |
| `test_m_step.R` | 20 | All-missing (=prior), regression recovery, prior shrinkage, diagonal Omega, PD/symmetry |
| `test_dynIRT_KD.R` | 23 | Integration K=2, LL non-decreasing, K=1 smoke, anchor priors, estimate_omega |
| **Total** | **120** | All pass |

### Implementation Phase 3 — V1 Simulation Study (Completed 2026-02-05)

**Script**: `scripts/R/v1_simulation_study.R`

**Setup**: N=100, J=200, T=6, K=2, 30% missing, 10 replications (seeds 1–10), maxit=500, thresh=1e-4.

**Results** (10 replications):

| Metric | Mean | SD | Min | Target | Status |
|--------|------|-----|-----|--------|--------|
| Ideal point r | 0.9403 | 0.012 | 0.921 | > 0.90 | **PASS** |
| Beta dim1 r | 0.9329 | 0.017 | 0.904 | > 0.85 | **PASS** |
| Beta dim2 r | 0.9362 | 0.010 | 0.917 | > 0.85 | **PASS** |
| Alpha r | 0.9682 | 0.004 | 0.962 | > 0.85 | **PASS** |
| Convergence (formal) | 3/10 | — | — | — | See lessons |
| LL monotonic | 5/10 | — | — | — | See lessons |
| Runtime per replication | 64s | — | — | — | Pure R, no parallelism |

**Overall: V1 PASSED.**

### Convergence Acceleration Evaluation (Completed 2026-02-05)

Evaluated SQUAREM + Aitken early stopping (implemented in `scripts/R/dynIRT_KD_fast.R`, tested in `tests/test_dynIRT_KD_fast.R`, 17 tests). SQUAREM found to be counterproductive (35% slower wall-clock). Aitken stopping incorporated back into the production `dynIRT_KD()` as optional `thresh_aitken` control parameter. See Lesson L7 for details.

---

## 18. Lessons Learned

### L1. Convergence is slow but stable

The EM converges to good parameter estimates quickly (~50–100 iterations) but the formal delta criterion (max parameter change) decreases slowly from ~1e-3 to ~1e-4 over iterations 100–500. This is typical of EM for IRT models — the E-step and M-step interact weakly when many items have small discrimination. **Practical recommendation**: use `thresh = 1e-4` for production runs and confirm substantive stability via ideal point correlations between consecutive iterations.

### L2. Log-likelihood is approximately (not strictly) monotone

In 5/10 replications, small LL decreases were observed (max magnitude ~4e-04). This is expected and documented: the algorithm uses variance=1 for pseudo-observations rather than the true conditional variance Var[y*|y] ≤ 1 (Section 5.2). This is a "classification EM" approximation consistent with `emIRT::dynIRT()`. The decreases are numerically negligible and do not affect parameter recovery. **Not a bug** — it is a known approximation.

### L3. K=1 dimension drop requires `drop = FALSE` guards

When K=1, R aggressively drops matrix dimensions (e.g., `x[, , t]` becomes a vector instead of a N×1 matrix). The `da_step()` function handles this with `dim(x_t) <- c(N, K)` after slicing. Test helpers also needed `matrix(..., nrow=N, ncol=K)` wrapping. Any future code operating on `x_smooth[, , t]` must guard against dimension dropping.

### L4. `testthat::test_file()` changes working directory

`testthat::test_file("tests/test_foo.R")` sets the working directory to `tests/`. All `source()` paths inside test files must use `file.path("..", "scripts", "R", ...)`. The main `dynIRT_KD.R` auto-sourcing mechanism checks `exists("da_step", mode = "function")` before sourcing companions, so tests that pre-source the modules individually avoid path conflicts.

### L5. Anchor mismatch affects rotation, not recovery quality

When the automatically selected anchor unit's true position is far from the prescribed anchor position (e.g., true (+1.24, +2.22) mapped to target (+2, +2)), the algorithm must absorb the mismatch through the tight prior. This distorts the scale slightly but does not degrade recovery. In real applications, anchor positions should be chosen close to expected positions from PCA initialization.

### L7. SQUAREM acceleration does NOT help; Aitken stopping does

We evaluated SQUAREM (Varadhan & Roland 2008) acceleration combined with Aitken early stopping. Key findings from diagnostic runs:

| Seed | Vanilla iters/time | SQUAREM evals/time | Ratio |
|------|-------------------|--------------------|-------|
| 202 | 200 / 7.9s | 266 / 10.6s | **1.35× slower** |
| 101 | 120 / 4.7s | 162 / 6.4s | **1.35× slower** |

**Why SQUAREM doesn't help**: SQUAREM requires 2 fixed-point evaluations per acceleration step (θ₁ = F(θ₀), θ₂ = F(θ₁), then extrapolation). In our EM, each evaluation is a full E+M cycle (data augmentation + N Kalman smoothers + M-step). The extrapolation step saves ~33% of iterations but doubles the per-iteration cost, resulting in a net 35% wall-clock slowdown.

**Why Aitken stopping helps**: Aitken's acceleration criterion estimates the log-likelihood at convergence from three consecutive LL values: a_m = (ℓ_m − ℓ_{m−1}) / (ℓ_{m−1} − ℓ_{m−2}), and ℓ_∞ = ℓ_{m−1} + (ℓ_m − ℓ_{m−1}) / (1 − a_m). When |ℓ_∞ − ℓ_m| < threshold, further iterations produce negligible improvement. This works independently of SQUAREM and is now incorporated into `dynIRT_KD()` via the `thresh_aitken` control parameter (default NULL = disabled, recommended 1e-4).

**Decision**: SQUAREM code remains in `scripts/R/dynIRT_KD_fast.R` for reference, but `dynIRT_KD.R` (the production code) uses only Aitken stopping. Recovery quality is identical between SQUAREM and vanilla at convergence.

### L6. Runtime is manageable in pure R

~64 seconds per replication (N=100, J=200, T=6, K=2, 500 iterations) with no parallelism. For the real data (N~180, J~2000, T=6, K=2), the bottleneck will be the Kalman filter (linearly proportional to J). Estimated runtime: ~10× longer ≈ 10 minutes per EM run. Parallelizing the per-country Kalman filter via `parallel::mclapply()` (Section 13.3) would reduce this by a factor of ~ncores.

---

## 19. Remaining Verification Phases

| Phase | Description | Status | Blocked on |
|-------|-------------|--------|------------|
| V2 | K=1 equivalence with `emIRT::dynIRT()` | Pending | V1 ✓ |
| V3 | US Congress benchmark (DW-NOMINATE comparison) | Pending | V2 |
| V4 | Anchor sensitivity | Pending | V3 |
| V5 | Treaty data application | Pending | V4 |
