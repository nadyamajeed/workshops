##### START OF CODE #####

# R version 4.4.0
library(lavaan) # version 0.6-17

# generate data
set.seed(0)
PID = 1:1100
possibleFactorLoadings = seq(0.4, 0.7, 0.05)
eta1 = rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
eta2 = 0.5*eta1 + rnorm(n = 1100, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))
eta3 = 0.3*eta2 - 0.2*eta1 + rnorm(n = 1100, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))
eta4 = 0.3*eta1 - 0.2*eta2 + rnorm(n = 1100, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))

# organise all data
myData = 
  data.frame(
    PID = PID,
    i01 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i02 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i03 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i04 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i05 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i06 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i07 = 3*(sample(possibleFactorLoadings, 1)*eta3 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i08 = 3*(sample(possibleFactorLoadings, 1)*eta3 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i09 = 3*(sample(possibleFactorLoadings, 1)*eta3 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i10 = 3*(sample(possibleFactorLoadings, 1)*eta4 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i11 = 3*(sample(possibleFactorLoadings, 1)*eta4 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i12 = 3*(sample(possibleFactorLoadings, 1)*eta4 + rnorm(n = 1100, m = 0, sd = sample(seq(0.1, 1, 0.1), 1)))
  ) |>
  round()

# test a simple cfa model
# (check for any convergence issues)
# (don't want a complicated example that can't run)
lavaan::sem(
  "
  eta1 =~ i01 + i02 + i03 
  eta2 =~ i04 + i05 + i06
  eta3 =~ i07 + i08 + i09
  eta4 =~ i10 + i11 + i12
  eta4 + eta3 ~ eta2 + eta1
  eta2 ~ eta1
  eta4 ~~ eta3
  ",
  data = myData
) |> summary(fit.measures = TRUE)

# write data
write.csv(myData, "data3.csv", row.names = FALSE)

##### END OF CODE #####
