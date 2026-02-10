# The Kalman Filter: A Didactic Note

*For someone who knows regression well but not time series.*

---

## 1. The Problem We're Solving

You have a quantity that changes over time — say, a country's ideal point $x_t$ — but you never observe it directly. Instead, you observe noisy signals: treaty ratification decisions, each of which gives you imperfect information about where $x_t$ is.

The question: **Given all the noisy signals up to (and including) period $t$, what is your best guess of $x_t$?**

This is exactly what the Kalman filter computes.

## 2. Start With What You Know: Regression

### 2.1 One period, one observation

Suppose you have a single noisy observation of $x$:

$$\tilde{y} = \beta x + \alpha + \varepsilon, \quad \varepsilon \sim N(0, 1)$$

If you knew $\alpha$ and $\beta$, you'd solve for $x$:

$$x = \frac{\tilde{y} - \alpha}{\beta}$$

But that's just a point estimate with no uncertainty. In Bayesian terms, suppose you have a prior $x \sim N(\hat{x}_0, P_0)$. Then the posterior is also Gaussian:

$$x \mid \tilde{y} \sim N(\hat{x}_1, P_1)$$

where (and you should verify this — it's just Bayes' rule for two Gaussians):

$$P_1 = \left(\frac{1}{P_0} + \beta^2\right)^{-1}$$

$$\hat{x}_1 = P_1 \left(\frac{\hat{x}_0}{P_0} + \beta \cdot (\tilde{y} - \alpha)\right)$$

**This is the update step of the Kalman filter in its simplest form.** You combine prior information ($\hat{x}_0, P_0$) with new data ($\tilde{y}$) to get a posterior ($\hat{x}_1, P_1$).

### 2.2 The "Kalman gain" is just a weight

Rewrite the update as:

$$\hat{x}_1 = \hat{x}_0 + K \cdot (\tilde{y} - \alpha - \beta \hat{x}_0)$$

where the **Kalman gain** is:

$$K = \frac{P_0 \beta}{1 + \beta^2 P_0}$$

Read this as: the Kalman gain $K$ tells you **how much to adjust your prior guess** when you see a surprise in the data. The term $(\tilde{y} - \alpha - \beta \hat{x}_0)$ is the **innovation** — it's the residual between what you observed and what you predicted. This is just like a regression residual.

- If $P_0$ is large (you're very uncertain about $x$), $K$ is large: you trust the data more.
- If $P_0$ is small (you're very confident), $K$ is small: you barely adjust.
- If $\beta$ is large (the observation is very informative about $x$), $K$ is large.

**Analogy to regression**: Think of $K$ as similar to $(X'X + \lambda I)^{-1} X'$ in ridge regression. It balances the data signal ($\beta$) against your prior precision ($1/P_0$).

### 2.3 Multiple observations in the same period

Now suppose you have $J$ observations in the same period:

$$\tilde{y}_j = \alpha_j + \beta_j x + \varepsilon_j, \quad j = 1, \ldots, J$$

You could stack these into a big regression and update all at once. Or — and this is what we do — you can **process them one at a time**. Start with your prior $(\hat{x}_0, P_0)$, update with observation 1 to get $(\hat{x}_1, P_1)$, then use that as the prior for observation 2, and so on.

The result is **identical** to processing them all at once (because Gaussians are closed under sequential updating). This sequential approach is computationally cheaper when $J$ is large, because you never need to invert a $J \times J$ matrix — each update is a scalar operation (in the 1D case) or a small $K \times K$ operation (in the $K$-dimensional case).

**This is the core of the Kalman filter within a single period**: sequential Bayesian updating of a Gaussian posterior, one observation at a time.

## 3. Adding Time: The Prediction Step

Now the key extension. Suppose the latent state **moves** between periods:

$$x_t = x_{t-1} + \omega_t, \quad \omega_t \sim N(0, \Omega)$$

This is a random walk. Between period $t-1$ and period $t$, the ideal point drifts by a random amount.

After processing all observations in period $t-1$, you have a posterior:

$$x_{t-1} \mid \text{data up to } t-1 \sim N(\hat{x}_{t-1|t-1}, P_{t-1|t-1})$$

**What's your belief about $x_t$ before seeing any period-$t$ data?** Since $x_t = x_{t-1} + \omega_t$ and both are Gaussian:

$$x_t \mid \text{data up to } t-1 \sim N(\hat{x}_{t-1|t-1}, \; P_{t-1|t-1} + \Omega)$$

The mean stays the same (your best guess is that the country stays put), but the **variance increases by $\Omega$** — you become more uncertain because the state might have moved.

This is the **prediction step**. After it, you're back to having a prior for period $t$, and you process that period's observations using the update step from Section 2.

### The full forward pass

The Kalman filter is just the alternation of these two steps:

```
For t = 1, 2, ..., T:

  PREDICT (for t >= 2):
    x̂_{t|t-1} = x̂_{t-1|t-1}           # best guess: same as last period
    P_{t|t-1} = P_{t-1|t-1} + Ω         # uncertainty grows

  UPDATE (for each observation j in period t):
    innovation = ỹ_j - α_j - β_j x̂     # surprise
    K = P β_j / (1 + β_j² P)            # Kalman gain
    x̂ = x̂ + K · innovation              # adjust estimate
    P = P - K β_j P                      # shrink uncertainty
```

That's it. That's the Kalman filter. It's Bayesian updating of a Gaussian state, with a prediction step that inflates uncertainty between periods.

## 4. The Smoother: Using Future Data Too

The Kalman filter gives you $\hat{x}_{t|t}$ — the estimate of $x_t$ using data **up to period $t$**. But we also have data from periods $t+1, \ldots, T$. Shouldn't that help us estimate $x_t$ too?

Yes. The **Rauch-Tung-Striebel (RTS) smoother** is a backward pass that refines each estimate using future information.

**Intuition**: After the forward pass, you know $\hat{x}_{T|T}$ (the estimate at the last period using all data). Now go backwards. Your smoothed estimate of $x_{T-1}$ should be a compromise between:

1. What the filter said: $\hat{x}_{T-1|T-1}$ (using data up to $T-1$)
2. What the next period implies: since $x_T = x_{T-1} + \omega_T$, knowing $x_T$ gives you information about $x_{T-1}$

The RTS smoother computes exactly this compromise:

```
For t = T-1, T-2, ..., 1:

  # How much should we trust the future?
  L_t = P_{t|t} (P_{t+1|t})^{-1}         # "smoother gain"

  # Blend filter estimate with smoothed future
  x̂_{t|T} = x̂_{t|t} + L_t (x̂_{t+1|T} - x̂_{t+1|t})
  P_{t|T} = P_{t|t} + L_t (P_{t+1|T} - P_{t+1|t}) L_t'
```

The term $(\hat{x}_{t+1|T} - \hat{x}_{t+1|t})$ is key: it's the **revision** that future data made to the next period's estimate. The smoother gain $L_t$ propagates that revision backwards.

### 4.1 Why do we need the smoother at all?

The Kalman filter processes data **left to right** (period 1, then 2, then 3...). So your estimate of $x_3$ uses data from periods 1, 2, and 3 — but **not** from periods 4, 5, 6. This is wasteful. If Brazil ratified five environmental treaties in period 4, that's evidence that Brazil's environmental commitment was probably already high in period 3. The filter ignores this; the smoother doesn't.

More formally: the filter gives you $\hat{x}_{t|t} = E[x_t \mid y_1, \ldots, y_t]$. The smoother gives you $\hat{x}_{t|T} = E[x_t \mid y_1, \ldots, y_T]$ — conditioning on **all** data. The smoother is always at least as precise as the filter (smaller $P$).

### 4.2 How the backward pass works, step by step

Suppose $T = 4$ periods. The forward pass (Kalman filter) gave you:

| Period | Filter estimate | Filter variance |
|--------|:-:|:-:|
| $t=1$ | $\hat{x}_{1|1} = 0.5$ | $P_{1|1} = 0.8$ |
| $t=2$ | $\hat{x}_{2|2} = 0.7$ | $P_{2|2} = 0.6$ |
| $t=3$ | $\hat{x}_{3|3} = 0.3$ | $P_{3|3} = 0.9$ |
| $t=4$ | $\hat{x}_{4|4} = 1.2$ | $P_{4|4} = 0.4$ |

And the predicted (before-update) values were:

| Period | Predicted estimate | Predicted variance |
|--------|:-:|:-:|
| $t=2$ | $\hat{x}_{2|1} = 0.5$ | $P_{2|1} = 0.9$ |
| $t=3$ | $\hat{x}_{3|2} = 0.7$ | $P_{3|2} = 0.7$ |
| $t=4$ | $\hat{x}_{4|3} = 0.3$ | $P_{4|3} = 1.0$ |

Now the backward pass:

**Start at $t = 4$**: The smoothed estimate is just the filter estimate (there's no future data):

$$\hat{x}_{4|T} = \hat{x}_{4|4} = 1.2, \quad P_{4|T} = P_{4|4} = 0.4$$

**Back to $t = 3$**: The filter said $\hat{x}_{3|3} = 0.3$, but we now know $\hat{x}_{4|T} = 1.2$. The filter's prediction for period 4 was $\hat{x}_{4|3} = 0.3$. But the smoothed value is 1.2 — much higher! The revision is:

$$\hat{x}_{4|T} - \hat{x}_{4|3} = 1.2 - 0.3 = 0.9$$

Future data says "period 4 was much higher than you predicted from period 3." The smoother gain determines how much of that surprise to propagate backward:

$$L_3 = P_{3|3} / P_{4|3} = 0.9 / 1.0 = 0.9$$

So the smoothed estimate for period 3 is:

$$\hat{x}_{3|T} = 0.3 + 0.9 \times 0.9 = 0.3 + 0.81 = 1.11$$

The smoother **pulled $x_3$ up** from 0.3 toward 1.11 because future data revealed the ideal point was high in period 4. The logic: if $x_4 = 1.2$ and $x_4 = x_3 + \omega$ (random walk), then $x_3$ was probably close to 1.2, not 0.3.

**Back to $t = 2$, then $t = 1$**: Same logic, recursively. Each period's estimate gets revised based on what we learned from the future.

### 4.3 When does the smoother matter most?

The smoother makes the biggest difference when:

1. **A period has few observations** (filter estimate is uncertain, $P_{t|t}$ is large). Future and past data fill in the gap.
2. **$\Omega$ is small** (ideal points don't move much). Knowing $x_{t+1}$ is very informative about $x_t$.
3. **The filter prediction was wrong** (large revision $\hat{x}_{t+1|T} - \hat{x}_{t+1|t}$).

In our application, many countries have sparse ratification records in some periods. Without the smoother, those periods would have high-variance estimates driven by noise. The smoother borrows strength from neighboring periods, producing much more stable trajectories.

### 4.4 Regression analogy

The filter is like **recursive OLS** — you estimate $\beta$ using data points 1 through $t$, updating sequentially. Each estimate only uses past data.

The smoother is like **full-sample OLS** — you go back and re-estimate using all $T$ data points. The full-sample estimate is always more efficient.

The difference: in OLS, $\beta$ is fixed, so you just use the full sample and you're done. In a state-space model, $x_t$ is *different* at each $t$, so you can't just pool everything. The smoother handles this by propagating information backward through the random walk structure, respecting the fact that $x_t$ and $x_{t+1}$ are close but not identical.

## 5. The Multi-Dimensional Case ($K > 1$)

Everything above generalizes to $K$ dimensions. The state $\mathbf{x}_t \in \mathbb{R}^K$ is now a vector, and:

- **Prior/posterior**: $\mathbf{x}_t \sim N(\hat{\mathbf{x}}, \mathbf{P})$ where $\mathbf{P}$ is a $K \times K$ covariance matrix
- **Observation**: $\tilde{y}_j = \alpha_j + \boldsymbol{\beta}_j' \mathbf{x}_t + \varepsilon_j$ where $\boldsymbol{\beta}_j \in \mathbb{R}^K$
- **Update**:

$$\mathbf{K} = \frac{\mathbf{P} \boldsymbol{\beta}_j}{1 + \boldsymbol{\beta}_j' \mathbf{P} \boldsymbol{\beta}_j}$$

$$\hat{\mathbf{x}} \leftarrow \hat{\mathbf{x}} + \mathbf{K} \cdot (\tilde{y}_j - \alpha_j - \boldsymbol{\beta}_j' \hat{\mathbf{x}})$$

$$\mathbf{P} \leftarrow \mathbf{P} - \mathbf{K} \boldsymbol{\beta}_j' \mathbf{P}$$

The only change is that $K$ is now a $K \times 1$ vector (Kalman gain) and $P$ is a $K \times K$ matrix. The scalar denominator $1 + \boldsymbol{\beta}_j' \mathbf{P} \boldsymbol{\beta}_j$ stays scalar because each observation is one-dimensional (a single treaty ratification decision).

The prediction step is:

$$\hat{\mathbf{x}}_{t|t-1} = \hat{\mathbf{x}}_{t-1|t-1}$$
$$\mathbf{P}_{t|t-1} = \mathbf{P}_{t-1|t-1} + \boldsymbol{\Omega}$$

Same logic: the mean stays, the covariance inflates by $\Omega$.

## 6. Why Does This Matter For Our Model?

In our EM algorithm, the Kalman filter-smoother serves a specific role:

1. **The M-step** gives us updated item parameters $(\alpha_j, \boldsymbol{\beta}_j)$
2. **The data augmentation step** gives us pseudo-observations $\tilde{y}_{ij}$ (expected values of the latent utilities under the truncated normal)
3. **The Kalman filter-smoother** takes these pseudo-observations and produces the best estimates of each country's ideal point trajectory $\{\mathbf{x}_{it}\}_{t=1}^T$, accounting for both the noisy observations and the dynamic structure (the random walk)

Without the Kalman filter, we'd be treating each period independently — like running a separate cross-sectional IRT for each period. The Kalman filter **borrows strength across time**: if a country ratified nothing in period 3 (no information), the filter interpolates between the period-2 and period-4 estimates via the random walk. This is especially important for our application, where many country-treaty cells are missing.

**Each country is processed independently** (countries don't interact in the state equation), so we run $N$ separate Kalman filter-smoothers in the E-step. This is embarrassingly parallel.

## 7. The Role of $\Omega$

The evolution covariance $\Omega$ controls the speed of change:

- **Large $\Omega$**: Ideal points can move a lot between periods. The filter puts more weight on current-period observations and less on neighboring periods. Extreme case: $\Omega \to \infty$ reduces to independent cross-sectional estimation.
- **Small $\Omega$**: Ideal points are nearly frozen. The filter heavily smooths across time. Extreme case: $\Omega = 0$ forces $x_{it} = x_{i1}$ for all $t$ (static model).

In our application, we set $\Omega = 0.1 \cdot I_K$, which allows moderate movement between 5-year periods. Our robustness check R4 varies $\Omega$ across a 50-fold range and finds results are stable for 4/6 domains.

## 8. Summary Table

| Concept | Regression analogy | Kalman filter |
|---------|-------------------|---------------|
| Unknown | $\beta$ in $y = X\beta + \varepsilon$ | $x_t$ (latent ideal point) |
| Data | $(y, X)$ | Treaty ratification pseudo-observations $\tilde{y}_j$ |
| Prior | Ridge penalty $\lambda$ | Prior $(\hat{x}_0, P_0)$ |
| Estimate | $\hat{\beta} = (X'X + \lambda I)^{-1}X'y$ | $\hat{x}_{t|t}$ after sequential updates |
| Uncertainty | $(X'X + \lambda I)^{-1}$ | $P_{t|t}$ |
| Key difference | Static | Dynamic: prediction step inflates uncertainty between periods; smoother uses future data |

## 9. Recommended Reading

- Petris, Petrone, and Campagnoli (2009). *Dynamic Linear Models with R*. Chapters 2-3.
- Durbin and Koopman (2012). *Time Series Analysis by State Space Methods*. Chapter 4.
- For the connection to IRT specifically: Imai, Lo, and Olmsted (2016), Section 3 and Appendix.
