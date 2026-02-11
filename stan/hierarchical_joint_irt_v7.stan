// Hierarchical Joint IRT v7 — USA/CUB anchors, V-Dem as predictor
// theta_{i,d,t} = mu_{i,t} + gamma_{i,d}
// y_{ij} ~ Bernoulli_logit(alpha_j + beta_j * theta)
// mu_{i,1} ~ N(0, 1)
// mu_{i,t} ~ N(mu_{i,t-1}, omega_fixed)  [random walk]
// mu_{i,t} ~ N(b_v * VDem_std_{i,t}, sigma_reg)  [V-Dem soft prior, additive]
// gamma_{i,d} ~ N(delta_d * Z_{i,d}, sigma_d^2)
//
// Identification: mu[USA, all t] = +2, mu[CUB, all t] = -2

data {
  int<lower=1> N;
  int<lower=1> N_free;
  int<lower=1> J;
  int<lower=1> T_periods;
  int<lower=1> D;
  int<lower=1> N_obs;

  array[N_obs] int<lower=1, upper=N> ii;
  array[N_obs] int<lower=1, upper=J> jj;
  array[N_obs] int<lower=1, upper=D> dd;
  array[N_obs] int<lower=1, upper=T_periods> tt;
  array[N_obs] int<lower=0, upper=1> y;

  array[N] int<lower=1, upper=T_periods> startlegis;
  array[N] int<lower=1, upper=T_periods> endlegis;

  int<lower=1, upper=N> anchor_pos_idx;
  int<lower=1, upper=N> anchor_neg_idx;
  real anchor_pos_val;
  real anchor_neg_val;

  array[N_free] int<lower=1, upper=N> free_idx;

  // V-Dem (standardized) for free countries
  matrix[N_free, T_periods] vdem_std_free;
  matrix[N_free, T_periods] vdem_obs;

  // Domain predictors
  matrix[N, D] Z_domain;

  // Fixed constants
  real<lower=0> omega_fixed;
  real<lower=0> sigma_reg;
}

parameters {
  matrix[N_free, T_periods] mu_free;
  matrix[N, D] gamma_raw;

  vector[J] alpha;
  vector[J] beta;

  vector<lower=0>[D] sigma_d;
  vector[D] delta;

  real b_v;
}

transformed parameters {
  matrix[N, T_periods] mu;

  for (t in 1:T_periods) {
    mu[anchor_pos_idx, t] = anchor_pos_val;
    mu[anchor_neg_idx, t] = anchor_neg_val;
  }
  for (k in 1:N_free) {
    mu[free_idx[k]] = mu_free[k];
  }

  matrix[N, D] gamma;
  for (d in 1:D) {
    gamma[, d] = delta[d] * Z_domain[, d] + sigma_d[d] * gamma_raw[, d];
  }
}

model {
  // --- Priors ---
  alpha ~ normal(0, 2);
  beta ~ normal(0, 1);
  sigma_d ~ normal(0, 0.5);
  to_vector(gamma_raw) ~ std_normal();
  delta ~ normal(0, 1);
  b_v ~ normal(0, 2);

  // --- Random walk on mu_free ---
  for (k in 1:N_free) {
    mu_free[k, 1] ~ normal(0, 1.0);
    for (t in 2:T_periods) {
      mu_free[k, t] ~ normal(mu_free[k, t - 1], omega_fixed);
    }
  }

  // --- V-Dem as predictor: soft prior on mu at each observed period ---
  for (k in 1:N_free) {
    for (t in 1:T_periods) {
      target += vdem_obs[k, t] * normal_lpdf(mu_free[k, t] | b_v * vdem_std_free[k, t], sigma_reg);
    }
  }

  // --- Likelihood ---
  {
    vector[N_obs] eta;
    for (n in 1:N_obs) {
      real theta = mu[ii[n], tt[n]] + gamma[ii[n], dd[n]];
      eta[n] = alpha[jj[n]] + beta[jj[n]] * theta;
    }
    y ~ bernoulli_logit(eta);
  }
}

generated quantities {
  matrix[N, D] theta_mean;
  for (d in 1:D) {
    for (i in 1:N) {
      real total = 0;
      int count = 0;
      for (t in startlegis[i]:endlegis[i]) {
        total += mu[i, t] + gamma[i, d];
        count += 1;
      }
      theta_mean[i, d] = total / count;
    }
  }

  vector[N] mu_mean;
  for (i in 1:N) {
    real total = 0;
    int count = 0;
    for (t in startlegis[i]:endlegis[i]) {
      total += mu[i, t];
      count += 1;
    }
    mu_mean[i] = total / count;
  }
}
