##### START OF CODE #####

# R version 4.4.0
library(psych)   # version 2.4.3
library(ggplot2) # version 3.5.1
library(lavaan)  # version 0.6-17

# settings
options(scipen = 9999)

# read in data
dataFact = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/sem/v2-2024/dataFiles/data2.csv")

##### DATA CHECKS #####

# look at descriptives
# check the sd and skew
dataFact |>
  psych::describe()

# visually examine normality
ggplot2::ggplot(dataFact, aes(x = i1)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i2)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i3)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i4)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i5)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i6)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i7)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFact, aes(x = i8)) +
  ggplot2::geom_histogram(binwidth = 1)

##### FACTOR ANALYSIS #####

# unidimensional
fit1 = lavaan::sem(
  "eta1 =~ i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8",
  data = dataFact
)

# two-dimensional
fit2 = lavaan::sem(
  "eta1 =~ i1 + i2 + i3 + i4
   eta2 =~ i5 + i6 + i7 + i8
   eta1 ~~ eta2",
  data = dataFact
)

# compare fits
# fit indices: max CFI, max TLI, min RMSEA, min SRMR
# comparison indices: min AIC, min BIC
sapply(
  X = c(fit1, fit2),
  FUN = function(X) lavaan::fitMeasures(X, c(
    "chisq", "df", "pvalue", 
    "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
) |> round(3)

##### END OF CODE #####
