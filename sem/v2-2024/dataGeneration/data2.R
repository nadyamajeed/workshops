##### START OF CODE #####

# R version 4.4.0
library(lavaan) # version 0.6-17

# generate data
set.seed(0)
PID = 1:750
possibleFactorLoadings = seq(0.4, 0.7, 0.05)
temp = rnorm(n = 750, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
eta1 = 0.4*temp + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))
eta2 = 0.4*temp + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))

# organise all data
myData = 
  data.frame(
    PID = PID,
    i1 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i2 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i3 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i4 = 3*(sample(possibleFactorLoadings, 1)*eta1 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i5 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i6 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i7 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1))),
    i8 = 3*(sample(possibleFactorLoadings, 1)*eta2 + rnorm(n = 750, m = 0, sd = sample(seq(0.2, 1, 0.1), 1)))
  ) |>
  round()

# test a simple cfa model
# (check for any convergence issues)
# (don't want a complicated example that can't run)
lavaan::sem(
  "
  eta1 =~ i1 + i2 + i3 + i4
  eta2 =~ i5 + i6 + i7 + i8
  eta1 ~~ eta2
  ",
  data = myData
) |> summary(fit.measures = TRUE)

# write data
write.csv(myData, "data2.csv", row.names = FALSE)

##### END OF CODE #####
