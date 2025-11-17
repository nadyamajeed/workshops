# START

# 1. simulate data

N = 10000

beta_XC = 0.5
beta_YC = 0.5
beta_YX = 0.5

C = rnorm(N)
X = beta_XC * C + rnorm(N)
Y = beta_YX * X + beta_YC * C + rnorm(N)

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

# 2bi. incorrect model, omits C->X

sem(
  "Y ~ X + C",
  orthogonal = TRUE,
  data = dataSim
) |>
  summary() # accurate results for X->Y

# 2bii. can also be done using lm

lm(
  Y~ X + C,
  data = dataSim) |>
  summary() # same results

# 2c. incorrect model, models C~~X instead of C->X

sem(
  "Y ~ X + C
  X ~~ C",
  data = dataSim
) |>
  summary() # accurate results for X->Y

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
