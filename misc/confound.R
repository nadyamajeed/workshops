# START

# 1. simulate data with X->Y and C being a confound (C->X and C->Y)

# set sample size
N = 10000 

# set ref coeffs
beta_XC = 0.5 # C->X
beta_YC = 0.5 # C->Y
beta_YX = 0.5 # X->Y (coeff of interest)

# generate C (independent)
C = rnorm(N) 

# generate X (dependent on C)
X = beta_XC * C + rnorm(N) 

# generate Y (dependent on C and X)
Y = beta_YX * X + beta_YC * C + rnorm(N) 

# collect all data into single data.frame
dataSim = data.frame(
  C = C,
  X = X,
  Y = Y
)

# 2. check results of various analytic models

library(lavaan)

# 2a. correct model

sem(
  "Y ~ X + C
  X ~ C",
  data = dataSim
) |>
  summary() # accurate results for X->Y

# 2b. incorrect model, models C~~X instead of C->X

sem(
  "Y ~ X + C
  X ~~ C",
  data = dataSim
) |>
  summary() # accurate results for X->Y

# 2ci. incorrect model, omits C->X

sem(
  "Y ~ X + C",
  orthogonal = TRUE,
  data = dataSim
) |>
  summary() # accurate results for X->Y

# 2cii. can also be done using lm

lm(
  Y~ X + C,
  data = dataSim) |>
  summary() # same results

# 2di. incorrect model, omits C entirely

sem(
  "Y ~ X",
  data = dataSim
) |>
  summary() # inaccurate results for X->Y

# 2dii. can also be done using lm

lm(
  Y ~ X,
  data = dataSim
) |>
  summary() # same results

# END
