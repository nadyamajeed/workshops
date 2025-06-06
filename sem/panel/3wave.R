##### START OF CODE #####

# R version 4.5.0
library(dplyr)      # version 1.1.4
library(lavaan)     # version 0.6-19
library(lavaanPlot) # version 0.8.1

# generate artificial data
set.seed(3); myData = 
  data.frame(
    PID = 1:1500,
    common = rnorm(n = 1500, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
  ) %>%
  dplyr::mutate(
    traitA = 0.2*common + rnorm(n = 1500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1)),
    traitB = 0.2*common + rnorm(n = 1500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)),
    A_t1 = (0.5*traitA + rnorm(n = 1500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    B_t1 = (0.5*traitB + rnorm(n = 1500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    A_t2 = (0.5*traitA + 0.4*A_t1 + 0.3*B_t1 + rnorm(n = 1500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    B_t2 = (0.5*traitB + 0.6*B_t1 + 0.2*A_t1 + rnorm(n = 1500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    A_t3 = (0.5*traitA + 0.3*A_t2 + 0.4*B_t2 + rnorm(n = 1500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    B_t3 = (0.5*traitB + 0.5*B_t2 + 0.3*A_t2 + rnorm(n = 1500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    common = NULL
  ) %>%
  round()

##### CLPM #####

# test CLPM (all parameters free)
clpm1 = lavaan::sem(
  "
  # structural paths
  # i.e., autoregressions and cross lags
  B_t3 ~ A_t2 + B_t2
  A_t3 ~ A_t2 + B_t2
  B_t2 ~ A_t1 + B_t1
  A_t2 ~ A_t1 + B_t1
  # exogenous covariance
  A_t1 ~~ B_t1
  # residual covariance
  A_t3 ~~ B_t3
  A_t2 ~~ B_t2
  ",
  data = myData
) 

# test CLPM (enforce constraints over time)
clpm2 = lavaan::sem(
  "
  # structural paths
  # i.e., autoregressions and cross lags
  B_t3 ~ ba*A_t2 + bb*B_t2
  A_t3 ~ aa*A_t2 + ab*B_t2
  B_t2 ~ ba*A_t1 + bb*B_t1
  A_t2 ~ aa*A_t1 + ab*B_t1
  # exogenous covariance
  A_t1 ~~ B_t1
  # residual covariance
  A_t3 ~~ rescov*B_t3
  A_t2 ~~ rescov*B_t2
  ",
  data = myData
) 

# look at which CLPM fits better
# lower aic & lower bic = better
sapply(
  list(clpm1, clpm2),
  function(x) lavaan::fitmeasures(x, c("aic", "bic"))
) 

# look at plots
clpm1 %>%
  lavaanPlot::lavaanPlot2(include = "all")
clpm2 %>%
  lavaanPlot::lavaanPlot2(include = "all")

##### RI-CLPM (better) #####
# reference to https://jeroendmulder.github.io/RI-CLPM/lavaan.html

# test RI-CLPM (all parameters free)
riclpm1 = lavaan::lavaan(
  "
  # between/trait components (random intercepts)
  betA =~ 1*A_t1 + 1*A_t2 + 1*A_t3
  betB =~ 1*B_t1 + 1*B_t2 + 1*B_t3
  betA ~~ betA # variance of trait A
  betB ~~ betB # variance of trait B
  betA ~~ betB # covariance of trait A and trait B
  # within/state components
  witA_t1 =~ 1*A_t1
  witA_t2 =~ 1*A_t2
  witA_t3 =~ 1*A_t3
  witB_t1 =~ 1*B_t1
  witB_t2 =~ 1*B_t2
  witB_t3 =~ 1*B_t3
  # structural paths
  # i.e., autoregressions and cross lags
  witB_t3 ~ witA_t2 + witB_t2
  witA_t3 ~ witA_t2 + witB_t2
  witB_t2 ~ witA_t1 + witB_t1
  witA_t2 ~ witA_t1 + witB_t1
  # exogenous covariance
  witA_t1 ~~ witB_t1
  # residual covariance
  witA_t3 ~~ witB_t3
  witA_t2 ~~ witB_t2
  # exogenous variance
  witA_t1 ~~ witA_t1
  witB_t1 ~~ witB_t1
  # residual variance
  witA_t2 ~~ witA_t2
  witA_t3 ~~ witA_t3
  witB_t2 ~~ witB_t2
  witB_t3 ~~ witB_t3
  ",
  data = myData,
  orthogonal = TRUE
) 

# test RI-CLPM (enforce constraints over time)
riclpm2 = lavaan::lavaan(
  "
  # between/trait components (random intercepts)
  betA =~ 1*A_t1 + 1*A_t2 + 1*A_t3
  betB =~ 1*B_t1 + 1*B_t2 + 1*B_t3
  betA ~~ betA # variance of trait A
  betB ~~ betB # variance of trait B
  betA ~~ betB # covariance of trait A and trait B
  # within/state components
  witA_t1 =~ 1*A_t1
  witA_t2 =~ 1*A_t2
  witA_t3 =~ 1*A_t3
  witB_t1 =~ 1*B_t1
  witB_t2 =~ 1*B_t2
  witB_t3 =~ 1*B_t3
  # structural paths
  # i.e., autoregressions and cross lags
  witB_t3 ~ ba*witA_t2 + bb*witB_t2
  witA_t3 ~ aa*witA_t2 + ab*witB_t2
  witB_t2 ~ ba*witA_t1 + bb*witB_t1
  witA_t2 ~ aa*witA_t1 + ab*witB_t1
  # exogenous covariance
  witA_t1 ~~ witB_t1
  # residual covariance
  witA_t3 ~~ covab*witB_t3
  witA_t2 ~~ covab*witB_t2
  # exogenous variance
  witA_t1 ~~ witA_t1
  witB_t1 ~~ witB_t1
  # residual variance
  witA_t2 ~~ witA_t2
  witA_t3 ~~ witA_t3
  witB_t2 ~~ witB_t2
  witB_t3 ~~ witB_t3
  ",
  data = myData,
  orthogonal = TRUE
) 

# look at plots
riclpm1 %>%
  lavaanPlot::lavaanPlot2()

##### END OF CODE #####
