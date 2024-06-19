##### START OF CODE #####

# R version 4.4.0
library(lavaan) # verion 0.6-17

# generate data
set.seed(0)
PID = 1:500
temp = rnorm(n = 500, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
X1 = 0.5*temp + rnorm(n = 500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1))
X2 = 0.5*temp + rnorm(n = 500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1))
M1 = 0.4*X1 + rnorm(n = 500, m = 0.1, sd = sample(seq(0.1, 1, 0.1), 1))
M2 = 0.4*X2 + rnorm(n = 500, m = -0.1, sd = sample(seq(0.1, 1, 0.1), 1))
Y1 = 0.4*M1 + 0.4*X1 + rnorm(n = 500, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))
Y2 = 0.4*X1 + rnorm(n = 500, m = 0, sd = sample(seq(0.1, 1, 0.1), 1))

# organise all data
myData = 
  data.frame(
    PID = PID,
    X1 = X1*2.5,
    X2 = X2*2.5,
    M1 = M1*2.5,
    M2 = M2*2.5,
    Y1 = Y1*2.5,
    Y2 = Y2*2.5
  ) |>
  round()

# test a simple mediation model
# (check for any convergence issues)
# (don't want a complicated example that can't run)
lavaan::sem(
  "
  Y2 + Y1 ~ M2 + M1 + X2 + X1
  M2 + M1 ~ X2 + X1
  Y2 ~~ Y1
  M2 ~~ M1
  X2 ~~ X1
  ",
  data = myData
) |> summary()

# write data
write.csv(myData, "data1.csv", row.names = FALSE)

##### END OF CODE #####
