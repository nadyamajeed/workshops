##### START OF CODE #####

# R version 4.5.0
library(psych)   # version 2.5.3
library(ggplot2) # version 3.5.2
library(lavaan)  # version 0.6-19
library(semPlot) # version 0.5-7

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
mediation1 = lavaan::sem(
  "# structure
  M1 ~ a*X1
  Y1 ~ b*M1 + c*X1
  # labels
  indirect := a*b
    direct := c
     total := c + a*b
  ",
  data = dataPath
) 
mediation1 |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)
mediation1 |>
  semTools::monteCarloCI()

# X1 --> M1, M2 (parallel) --> Y1
# with resid corr between M1 and M2
# with residual direct X1 --> Y1
# note that SE of indirect is biased
# SE/pval should be obtained using bootstrap or Monte-Carlo instead
mediation2 = lavaan::sem(
  "# structure
  M2 ~ a2*X1
  M1 ~ a1*X1
  Y1 ~ b2*M2 + b1*M1 + X1
  M1 ~~ M2
  # labels
  indirect2 := a2*b2
  indirect1 := a1*b1
  ",
  data = dataPath
)
mediation2 |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)
mediation2 |>
  semTools::monteCarloCI()

# X1 --> M1 --> M2 --> Y1
# (serial mediation)
# with all other paths also (saturated)
# note that SE of indirect is biased
# SE/pval should be obtained using bootstrap or Monte-Carlo instead
mediation3 = lavaan::sem(
  "# structure
  M1 ~ m1x1*X1
  M2 ~ m2m1*M1 + X1
  Y1 ~ y1m2*M2 + M1 + X1
  # labels
  indirect := y1m2*m2m1*m1x1
  ",
  data = dataPath
)
mediation3 |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)
mediation3 |>
  semTools::monteCarloCI()

# X1, X2 --> M1 --> M2 --> Y1
# (serial mediation, more complex)
# with corr between X1 and X2
# with all other paths also (saturated)
# note that SE of indirect is biased
# SE/pval should be obtained using bootstrap or Monte-Carlo instead
mediation4 = lavaan::sem(
  "# structure
  M1 ~ m1x1*X1 + m1x2*X2
  M2 ~ m2m1*M1 + X1 + X2
  Y1 ~ y1m2*M2 + M1 + X1 + X2
  X1 ~~ X2
  # labels
  indirect1 := y1m2*m2m1*m1x1
  indirect2 := y1m2*m2m1*m1x2
  ",
  data = dataPath
)
mediation4 |>
  lavaan::summary(
    standardized = TRUE, 
    fit.measures = TRUE)
mediation4 |>
  semTools::monteCarloCI()

##### END OF CODE #####
