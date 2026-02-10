# Methodology

## Model

We estimate country positions on a K-dimensional latent space using a dynamic probit item response theory (IRT) model. Each country $i$ ($i = 1, \ldots, N$) has a latent position $\mathbf{x}_{it} \in \mathbb{R}^K$ in period $t$ ($t = 1, \ldots, T$). Each treaty $j$ has a difficulty parameter $\alpha_j$ (intercept) and a discrimination vector $\boldsymbol{\beta}_j \in \mathbb{R}^K$ that determines how the latent dimensions map onto the probability of ratification:

$$P(y_{ij} = 1 \mid \mathbf{x}_{i,s(j)}) = \Phi\left(\alpha_j + \boldsymbol{\beta}_j' \mathbf{x}_{i,s(j)}\right)$$

where $\Phi$ is the standard normal CDF, $s(j)$ is the period in which treaty $j$ opened for signature, and $y_{ij} \in \{+1, -1\}$ indicates whether country $i$ ratified treaty $j$ during that period (flow coding). Ideal points evolve over time as a random walk:

$$\mathbf{x}_{i,t} \mid \mathbf{x}_{i,t-1} \sim \mathcal{N}(\mathbf{x}_{i,t-1}, \Omega)$$

where $\Omega$ is a $K \times K$ evolution covariance matrix that governs how much positions can shift between periods. We set $K = 2$ for the main specification, allowing each issue domain to have both a primary ILO-support dimension and a secondary dimension that captures confounding patterns (e.g., geographic or regime-type clustering that is orthogonal to ILO engagement).

## Identification

The probit IRT likelihood is invariant under affine transformations of the latent space. For $K$ dimensions, this invariance has $K(K+1)$ degrees of freedom ($K^2$ from rotation/scaling plus $K$ from translation). We resolve this by anchoring $K+1 = 3$ countries per domain with tight Gaussian priors ($\sigma = 0.01$) on their initial positions. So long as the three anchor positions are non-collinear, this provides exactly $K(K+1) = 6$ constraints that uniquely pin down the latent coordinate system.

For each domain, we select anchors guided by theory and PCA of the ratification matrix: one country with high expected ILO support as the positive dim-1 anchor (e.g., Denmark), one with low expected support as the negative dim-1 anchor (e.g., Iran), and a third country whose behavior is theoretically orthogonal to the ILO-support axis as the dim-2 anchor (e.g., China for investment, anchoring the distinction between economic engagement and broader liberal order commitment). Anchor configurations are reported in [Table X] and sensitivity to alternative anchors is assessed in Section [Robustness].

## Estimation

We estimate the model via an EM algorithm with Albert and Chib (1993) data augmentation, following the approach of Martin and Quinn (2002) and Imai, Lo, and Olmsted (2016). The algorithm iterates between:

**E-step.** (a) *Data augmentation*: For each observed vote, compute the expected value of the latent utility $y^*_{ij}$ under the truncated normal posterior implied by the current parameters. This converts the probit likelihood into Gaussian pseudo-observations. (b) *Kalman filter-smoother*: For each country independently, run a forward Kalman filter and backward Rauch-Tung-Striebel smoother to obtain posterior means and covariances of $\mathbf{x}_{it}$ across all periods. Items within each period are processed sequentially, avoiding the inversion of large observation matrices.

**M-step.** Update item parameters $(\alpha_j, \boldsymbol{\beta}_j)$ via penalized weighted least squares using the augmented data and smoothed ideal point estimates.

The algorithm converges when the observed-data log-likelihood changes by less than 0.01 between iterations (with a patience window of 5 iterations to accommodate the non-monotonicity inherent in the classification EM approximation). Starting values for ideal points are obtained from PCA of the ratification matrix. We implement the algorithm in R with Rcpp acceleration of the computationally intensive steps; the code and replication data are available at [repository].

## Validation

We validate the estimator through three exercises:

**Monte Carlo simulation.** We generate data from the model with $N = 100$ countries, $J = 200$ items, $T = 6$ periods, and $K = 2$ dimensions, with 30% missing data. Across 10 replications, recovered ideal points correlate with true values at $r = 0.94$, item discrimination parameters at $r = 0.93$, and difficulty parameters at $r = 0.97$.

**U.S. Congress benchmark.** As an external validation on well-studied data, we apply the estimator to roll-call votes from the 105th--112th U.S. Senate ($N = 162$ legislators, $J = 5{,}076$ votes, $T = 8$ sessions). The first recovered dimension correlates with DW-NOMINATE scores at $r = 0.978$, confirming that the algorithm recovers the dominant liberal-conservative axis with high fidelity.

**Anchor sensitivity.** We re-estimate the U.S. Congress model under three different anchor configurations. The first dimension is robust across all configurations ($r > 0.97$), and the second dimension shows stability at $r = 0.89$--$0.95$, indicating that the recovered latent space is not an artifact of specific anchor choices.

## Classification Accuracy

Table [X] reports the percent correctly classified (PCC) for the treaty data, decomposed by dimension. The model classifies 95.2% of environment votes correctly (baseline: 86.8%), 86.6% of human rights votes (baseline: 80.5%), and 81.8% of arms control votes (baseline: 68.5%). For investment and security, the baseline classification rate exceeds 97% due to data sparsity (most country-treaty pairs are non-ratification under flow coding), leaving limited room for improvement. The marginal contribution of the second dimension is largest for arms control (+6.8pp over dim-1 alone) and human rights (+1.9pp), consistent with the theoretical expectation that these domains have the strongest confounding structure.

### PCC Table (Flow, 5-year periods, V7 baseline)

| Domain | Dim 1 | Dim 2 | Both | Modal baseline | n |
|--------|:-----:|:-----:|:----:|:--------------:|------:|
| investment | 98.8% | 98.5% | 98.9% | 98.5% | 491,493 |
| security | 97.7% | 97.6% | 97.9% | 97.5% | 86,765 |
| environment | 93.7% | 90.4% | 95.2% | 86.8% | 303,329 |
| human_rights | 84.7% | 85.5% | 86.6% | 80.5% | 5,062 |
| arms_control | 75.0% | 81.1% | 81.8% | 68.5% | 2,632 |
| intellectual_property | 90.4% | 90.0% | 91.2% | 88.7% | 3,587 |

## References

- Albert, J. H., & Chib, S. (1993). Bayesian analysis of binary and polychotomous response data. *Journal of the American Statistical Association*, 88(422), 669--679.
- Imai, K., Lo, J., & Olmsted, J. (2016). Fast estimation of ideal points with massive data. *American Political Science Review*, 110(4), 631--656.
- Martin, A. D., & Quinn, K. M. (2002). Dynamic ideal point estimation via Markov chain Monte Carlo for the U.S. Supreme Court, 1953--1999. *Political Analysis*, 10(2), 134--153.
- Poole, K. T., & Rosenthal, H. (2007). *Ideology and Congress*. Transaction Publishers.
