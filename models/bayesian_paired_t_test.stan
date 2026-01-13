// Bayesian alternative to paired t-test
data {
	int<lower=0> N;
	real<lower=0> Y1[N];
	real<lower=0> Y2[N];
}

parameters {
	real mu;
	real<lower=0> sigma;
}

model {
  // Priors
  mu ~ normal(0, 10);
  sigma ~ normal(0, 10);
  
  // Likelihood
	for (i in 1:N)
		(Y2[i] - Y1[i]) ~ normal(mu, sigma);
}


generated quantities {
  real diff_pred[N]; // Array to store predicted differences (Y2 - Y1)
  
  for (i in 1:N) {
    diff_pred[i] = normal_rng(mu, sigma); // Predict the difference (Y2 - Y1) based on the estimated parameters
  }
}
