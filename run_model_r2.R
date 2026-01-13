#### Setup ####
source("./src/utils.R")
setup(use_stan=TRUE, is_rmd=FALSE)

#### Compile stan model ####
stan_model = rstan::stan_model(file="./models/multilevel_linear_model.stan")

#### Data ####
path = "./data/fgcms.csv"
df = load_and_preprocess_data(path, group_list=c("R"))

#### Stan data ####
# create model matrix
X_tmp = model.matrix(
  object = ~ Sex + Age_Category + s_Mean_temp + Tourist_visit_2d, data = df
)
head(X_tmp)
X = X_tmp[, -c(1)] # remove a column with 1s for intercept
head(X)

N = nrow(X)
P = ncol(X)
K = length(unique(as.factor(df$GID)))
GID = df$GID
Y = df$log_FGCMs

# stan data
stan_data = list(
  N = N,     # Sample size
  P = P,     # Number of explanatory variables
  K = K,     # Number of Gorillas (Gorilla_ID)
  GID = GID, # Gorilla ID Index
  Y = Y,     # Response variable
  X = X      # Design matrix of explanatory variables
)

#### Run MCMC sampling ####
# Initial values
set.seed(1234)
inits = function() 
  list(
    b = runif(P, -1, 1),
    a_raw = runif(K, -1, 1),
    a = runif(1, 3, 8),
    sigma_u = runif(1, 0, 1),
    sigma_Y = runif(1, 0, 1)
  )

# call stan from R
fit = rstan::sampling(
  object = stan_model, 
  data = stan_data, 
  init = inits,
  seed = 1234,
  chains = 4, 
  iter = 4000, 
  warmup = 2000, 
  thin = 1,
  open_progress = FALSE,
  control = list(adapt_delta=0.99, max_treedepth=10)
)

# Print summary of posteriors (Mean, se_mean, sd, Median, (67% BCI), 89% BCI, 97% BCI)
params = c("b", "a", "sigma_u", "sigma_Y", "lp__", "u", "a_raw") # Specify parameters to print
print(fit, pars = params, probs = c(0.015, 0.055, 0.500, 0.945, 0.985), digits_summary = 3)
# write.csv(x = summary(fit_R01)$summary, file = "output/model-output/multilevel_linear_regression_01_R.csv")

#### Save stan fit object ####
# save model output
saveRDS(object = fit, file = 'output/model-output/lmm_R02.RData')

# remove all objects
remove(list = ls())