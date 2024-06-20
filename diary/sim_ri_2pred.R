##### START OF CODE #####

# R version 4.4.0
options(scipen = 99999)
set.seed(0)

##### choose true parameters #####

# pick sample-level data-generating parameters
# i.e., fixed effects
gamma00 = 1     # fixed intercept
gamma01 = 1     # fixed slope of X1i
gamma10 = 1     # fixed slope of X1di-X1i
gamma02 = 1     # fixed slope of X2i
gamma20 = 1     # fixed slope of X2di-X2i

# pick L2 data-generating parameters
sigma_u0i = 1   # SD of random intercept
mu_X1i = 1      # mean of X1i
sigma_X1i = 1   # SD of X1i
mu_X2i = 1      # mean of X2i
sigma_X2i = 1   # SD of X2i

# pick L1 data-generating parameters
# (within individuals; assumed to be same for all individuals)
sigma_X1di = 1      # SD of X1di
sigma_X2di = 1      # SD of X2di
sigma_epsilondi = 1 # SD of residual

##### generate data #####

# set sample size
N_individuals = 500
N_days_per_individual = 10

# generate L2 data
dataL2 = data.frame(
  ID = 1:N_individuals,
  X1i = rnorm(n = N_individuals, mean = mu_X1i, sd = sigma_X1i),
  X2i = rnorm(n = N_individuals, mean = mu_X2i, sd = sigma_X2i),
  u0i = rnorm(n = N_individuals, mean = 0, sd = sigma_u0i)
) 

# generate L1 data
dataL1 = data.frame(); for(i in 1:N_individuals) {
  X1di      = rnorm(n = N_days_per_individual, mean = dataL2$X1i[i], sd = sigma_X1di)
  X2di      = rnorm(n = N_days_per_individual, mean = dataL2$X2i[i], sd = sigma_X2di)
  epsilondi = rnorm(n = N_days_per_individual, mean = 0, sd = sigma_epsilondi)
  dataL1 = rbind(
    dataL1,
    data.frame(
      ID = rep(i, N_days_per_individual),
      day = 1:N_days_per_individual,
      X1di,
      X1di_mean = mean(X1di),
      X2di,
      X2di_mean = mean(X2di),
      epsilondi))
}

# merge data and derive Ydi
dataFull = merge(dataL2, dataL1)
dataFull$Ydi = dataFull |> with(
  (gamma00+u0i) + 
    gamma01*X1i + gamma10*(X1di-X1i) + 
    gamma02*X2i + gamma20*(X2di-X2i) +
    epsilondi)

# run model and check output
lme4::lmer(
  Ydi ~ 1 + X1di_mean + I(X1di-X1di_mean) + X2di_mean + I(X2di-X2di_mean) + (1 | ID),
  data = dataFull) |> summary(correlation = FALSE)

##### END OF CODE #####
