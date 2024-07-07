##### START OF CODE #####

# R version 4.4.0
options(scipen = 99999)
set.seed(0)

##### set sample size #####

N_individuals = 1000
N_days_per_individual = 10

##### choose true parameters #####

# pick sample-level data-generating parameters
# i.e., fixed effects
gamma00 = 0.5   # fixed intercept
gamma01 = 0.5   # fixed slope of Xi
gamma10 = 0.5   # fixed slope of Xdi

# pick L2 data-generating parameters
sigma_u0i = 0.5 # SD of random intercept
mu_Xi = 0.5     # mean of Xi
sigma_Xi = 0.5  # SD of Xi

# pick L1 data-generating parameters
# (within individuals; assumed to be same for all individuals)
sigma_Xdi = 0.5       # SD of Xdi
sigma_epsilondi = 0.5 # SD of residual
rho_AR1 = 0.9         # AR1

# generate AR1 matrix
# referenced from https://stackoverflow.com/questions/18740796/generate-covariance-matrix-from-correlation-matrix
cormx_AR1  = MixMatrix::ARgenerate(N_days_per_individual, rho_AR1)
vcovmx_AR1 = cormx_AR1 * (rep(sigma_Xdi, N_days_per_individual) %*% t(rep(sigma_Xdi, N_days_per_individual)))

##### generate data #####

# generate L2 data
dataL2 = data.frame(
  ID = 1:N_individuals,
  Xi = rnorm(n = N_individuals, mean = mu_Xi, sd = sigma_Xi),
  u0i = rnorm(n = N_individuals, mean = 0, sd = sigma_u0i)
) 

# generate L1 data
dataL1 = data.frame(); for(i in 1:N_individuals) {
  Xdi       = rnorm(n = N_days_per_individual, mean = dataL2$Xi[i], sd = sigma_Xdi)
  epsilondi = MASS::mvrnorm(
    n = 1,
    mu = rep(0, N_days_per_individual),
    Sigma = vcovmx_AR1) 
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
lme4::lmer(
  Ydi ~ 1 + Xdi_mean + I(Xdi-Xdi_mean) + day + (1 + day | ID),
  data = dataFull) |> summary()

##### END OF CODE #####
