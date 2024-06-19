##### START OF CODE #####

# R version 4.4.0
library(lavaan) # version 0.6-17

# generate data
set.seed(2024)
PID = 1:400
possibleFactorLoadings = seq(0.4, 0.7, 0.05)
eta1 = rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
eta2 = 0.3*eta1 + rnorm(n = 400, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))
eta3 = 0.3*eta2 - 0.2*eta1 + rnorm(n = 400, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))

# organise all data
myData = 
  data.frame(
    PID = PID,
    i1 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i2 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i3 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i4 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i5 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i6 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i7 = 3*(sample(possibleFactorLoadings, 1)*eta3 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i8 = 3*(sample(possibleFactorLoadings, 1)*eta3 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))),
    i9 = 3*(sample(possibleFactorLoadings, 1)*eta3 + rnorm(n = 400, m = 0, sd = sample(seq(0.1, 1, 0.1), 1)))
  ) |>
  round()

# test a simple cfa model
# (check for any convergence issues)
# (don't want a complicated example that can't run)
lavaan::sem(
  "
  eta1 =~ i1 + i2 + i3 
  eta2 =~ i4 + i5 + i6
  eta3 =~ i7 + i8 + i9
  eta3 ~ eta2 + eta1
  eta2 ~ eta1
  ",
  data = myData
) |> summary(fit.measures = TRUE)

# write data
write.csv(myData, "data3.csv", row.names = FALSE)

##### END OF CODE #####
