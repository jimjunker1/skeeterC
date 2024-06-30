// generated with brms 2.21.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K] b;  // regression coefficients
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b[1] | 0, 10);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += poisson_log_glm_lpmf(Y | X, 0, b);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
}
