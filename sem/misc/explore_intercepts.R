testData = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/sem/v2-2024/dataFiles/data1.csv")

# does lm intercept match lavaan::sem residual intercept

# for one dv one iv

lm(
  Y1 ~ X1, 
  data = testData
) |> summary()

lavaan::sem(
  "Y1 ~ X1
  Y1 ~ 1",
  data = testData
) |> summary()

# for one dv two uncorrelated iv

lm(
  Y1 ~ X1 + X2, 
  data = testData
) |> summary()

lavaan::sem(
  "Y1 ~ X1 + X2
  X1 ~~ 0*X2
  Y1 ~ 1",
  data = testData
) |> summary()

# for one dv two correlated iv

lm(
  Y1 ~ X1 + X2, 
  data = testData
) |> summary()

lavaan::sem(
  "Y1 ~ X1 + X2
  X1 ~~ X2
  Y1 ~ 1",
  data = testData
) |> summary()

# for two uncorrelated dv two correlated iv

lm(
  Y1 ~ X1 + X2, 
  data = testData
) |> summary()

lm(
  Y2 ~ X1 + X2, 
  data = testData
) |> summary()

lavaan::sem(
  "Y1 + Y2 ~ X1 + X2
  Y1 ~~ 0*Y2
  X1 ~~ X2
  Y1 ~ 1",
  data = testData
) |> summary()
