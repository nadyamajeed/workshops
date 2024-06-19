##### START OF CODE #####

# R version 4.4.0
library(psych)   # version 2.4.3
library(ggplot2) # version 3.5.1
library(lavaan)  # version 0.6-17

# read in data
dataPath = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/sem/v2-2024/dataFiles/data1.csv")

##### DATA CHECKS #####

# look at descriptives
# check the sd and skew
dataPath |>
  psych::describe()

# visually examine normality
ggplot2::ggplot(dataPath, aes(x = X1)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataPath, aes(x = X2)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataPath, aes(x = M1)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataPath, aes(x = M2)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataPath, aes(x = Y1)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataPath, aes(x = Y2)) +
  ggplot2::geom_histogram(binwidth = 1)

##### PATH ANALYSIS #####

# X1 and X2 predict Y1
# with X1 and X2 correlated
lavaan::sem(
  "Y1 ~ X1 + X2
  X1 ~~ X2",
  data = dataPath
) |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)

# X1 predicts Y1 and Y2
# with Y1 and Y2 residually correlated
lavaan::sem(
  "Y1 + Y2 ~ X1
  Y1 ~~ Y2",
  data = dataPath
) |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)

# indirect X1 --> M1 --> Y1
# with residual direct X1 --> Y1
# note that SE of indirect is biased
# SE/pval should be obtained using bootstrap or Monte-Carlo instead
# (not shown)
lavaan::sem(
  "# structure
  M1 ~ a*X1
  Y1 ~ b*M1 + c*X1
  # labels
  indirect := a*b
    direct := c
     total := c + a*b
  ",
  data = dataPath
) |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)

# X1 --> M1, M2 (parallel) --> Y1
# with resid corr between M1 and M2
lavaan::sem(
  "M1 + M2 ~ X1
  Y1 ~ M2 + M1 + X1
  M1 ~~ M2
  ",
  data = dataPath
) |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)

# X1 --> M1 --> M2 --> Y1
lavaan::sem(
  "M1 ~ X1
  M2 ~ M1 + X1
  Y1 ~ M2 + M1 + X1
  ",
  data = dataPath
) |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)

# X1, X2 --> M1 --> M2 --> Y1
# with corr between X1 and X2
lavaan::sem(
  "M1 ~ X1 + X2
  M2 ~ M1 + X1 + X2
  Y1 ~ M2 + M1 + X1 + X2
  X1 ~~ X2
  ",
  data = dataPath
) |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)

##### END OF CODE #####
