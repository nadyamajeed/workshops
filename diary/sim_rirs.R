##### START OF CODE #####

# R version 4.3.1
options(scipen = 99999)

##### choose true parameters #####

# pick sample-level data-generating parameters
# i.e., fixed effects
gamma00 = 1.93 # fixed intercept
gamma01 = 0.10 # fixed slope of Xi
gamma10 = 0.09 # fixed slope of Xdi

# pick L2 data-generating parameters
sigma_u0i = 0.69    # SD of random intercept
mu_Xi = 4.21        # mean of Xi
sigma_Xi = 2.03     # SD of Xi
sigma_u1i = 0.08    # SD of random slope of Xdi
rho_u0i_u1i = -0.25 # correlation between random intercept and random slope

# generate vcov matrix between random intercept and random slope
vcovmx_u0i_u1i = matrix(c(
  sigma_u0i^2, rho_u0i_u1i*sigma_u0i*sigma_u1i,
  rho_u0i_u1i*sigma_u0i*sigma_u1i, sigma_u1i^2
), nrow = 2, byrow = TRUE)

# pick L1 data-generating parameters
# (within individuals; assumed to be same for all individuals)
sigma_Xdi = 2.03       # SD of Xdi
sigma_epsilondi = 0.58 # SD of residual

##### generate data #####

# set seed for reproducibility
set.seed(100)

# set sample size
N_individuals = 514
N_days_per_individual = 7

# generate L2 data
dataL2 = data.frame(
  ID = 1:N_individuals,
  Xi = rnorm(n = N_individuals, mean = mu_Xi, sd = sigma_Xi)
) |> cbind(
  MASS::mvrnorm(
    n = N_individuals,
    mu = c(u0i = 0, u1i = 0),
    Sigma = vcovmx_u0i_u1i)
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
  (gamma00+u0i) + gamma01*Xi + (gamma10+u1i)*I(Xdi-Xi) + epsilondi)

# keep only data that researcher would have
dataCollected = dataFull[,c("ID", "day", "Xdi", "Xdi_mean", "Ydi")]

# run model and check output
lme4::lmer(
  Ydi ~ 1 + Xdi_mean + I(Xdi-Xdi_mean) + (1 + I(Xdi-Xdi_mean) | ID),
  data = dataCollected) |> summary()

##### END OF CODE #####
