##### START OF CODE #####

# R version 4.3.1
options(scipen = 99999)
set.seed(0)

##### choose true parameters #####

# pick sample-level data-generating parameters
# i.e., fixed effects
gamma00 = 1     # fixed intercept
gamma01 = 1     # fixed slope of Xi
gamma10 = 1     # fixed slope of Xdi

# pick L2 data-generating parameters
sigma_u0i = 1   # SD of random intercept
mu_Xi = 1       # mean of Xi
sigma_Xi = 1    # SD of Xi

# pick L1 data-generating parameters
# (within individuals; assumed to be same for all individuals)
sigma_Xdi = 1       # SD of Xdi
sigma_epsilondi = 1 # SD of residual

##### generate data #####

# set sample size
N_individuals = 514
N_days_per_individual = 7

# generate L2 data
dataL2 = data.frame(
  ID = 1:N_individuals,
  Xi = rnorm(n = N_individuals, mean = mu_Xi, sd = sigma_Xi),
  u0i = rnorm(n = N_individuals, mean = mu_Xi, sd = sigma_u0i)
) 

# generate L1 data
dataL1 = data.frame(); for(i in 1:N_individuals) {
  Xdi       = rnorm(n = N_days_per_individual, mean = dataL2$Xi[i], sd = sigma_Xdi)
  epsilondi = rnorm(n = N_days_per_individual, mean = 0, sd = sigma_epsilondi)
  dataL1 = rbind(
    dataL1,
    data.frame(
      ID = rep(i, N_days_per_individual),
      day = 1:N_days_per_individual,
      Xdi,
      Xdi_mean = mean(Xdi),
      epsilondi))
}

# merge data and derive Ydi
dataFull = merge(dataL2, dataL1)
dataFull$Ydi = dataFull |> with(
  (gamma00+u0i) + gamma01*Xi + gamma10*I(Xdi-Xi) + epsilondi)

# run model and check output
lme4::lmer(
  Ydi ~ 1 + Xdi_mean + I(Xdi-Xdi_mean) + (1 | ID),
  data = dataFull) |> summary()

##### END OF CODE #####
