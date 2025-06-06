##### START OF CODE #####

# R version 4.5.0
library(dplyr)  # version 1.1.4
library(lavaan) # version 0.6-19

# generate artificial data
set.seed(0); myData = 
  data.frame(
    PID = 1:500,
    common = rnorm(n = 500, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
  ) %>%
  dplyr::mutate(
    A_t1 = (0.2*common + rnorm(n = 500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    B_t1 = (0.2*common + rnorm(n = 500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    A_t2 = (0.4*A_t1 + 0.3*B_t1 + rnorm(n = 500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    B_t2 = (0.6*B_t1 + 0.2*A_t1 + rnorm(n = 500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1)))*2.5,
    common = NULL
  ) %>%
  round()

# test a simple CLPM
lavaan::sem(
  "
  # structural paths
  # i.e., autoregressions and cross lags
  B_t2 ~ A_t1 + B_t1
  A_t2 ~ A_t1 + B_t1
  # exogenous correlation
  A_t1 ~~ B_t1
  # residual correlation
  A_t2 ~~ B_t2
  ",
  data = myData
) %>% summary(standardized = TRUE)

##### END OF CODE #####
