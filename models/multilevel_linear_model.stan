// Multilevel Linear Model 
// Random intercept for one type of cluster (Gorilla ID)
// Reparameterization
data {
  int<lower=0> N;               // Sample size
  int<lower=0> P;               // Number of predictors (intercept + covariates)
  int<lower=0> K;               // Number of Gorilla IDs
  vector[N] Y;                  // Variate (response variable)
  matrix[N, P] X;               // Design matrix without a column for intercept (1s)
  int<lower=0, upper=K> GID[N]; // Gorilla ID
}

parameters {
  real a;                // Mean of random intercept
  vector[K] a_raw;       // A parameter for Reparameterization
  vector[P] b;           // Slopes
  real<lower=0> sigma_u; // Hyper parameter
  real<lower=0> sigma_Y; // SD of normal distribution
}

transformed parameters {
  vector[K] u = sigma_u * a_raw; // Random intercept
  vector[N] mu;                  // Mean of normal distribution
  mu = X * b + a + u[GID];       // Linear predictor
}

model {
  // Priors
  a ~ normal(0, 10);
  b ~ normal(0, 10);
  sigma_u ~ exponential(1); // Hyper prior
  sigma_Y ~ exponential(1);
  
  // Likelihood
  a_raw ~ normal(0, 1);
  Y ~ normal(mu, sigma_Y);
}

generated quantities {
  vector[N] Y_sim;   // Simulated data from posterior for prediction check
  vector[N] log_lik; // Log likelihood for model comparison using WAIC and PSIS-LOOCV
  
  for (i in 1:N)
    Y_sim[i] = normal_rng(mu[i], sigma_Y);
  
  for (i in 1:N)
    log_lik[i] = normal_lpdf(Y[i] | mu[i], sigma_Y);
}
