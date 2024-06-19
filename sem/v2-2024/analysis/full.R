##### START OF CODE #####

# R version 4.4.0
library(psych)   # version 2.4.3
library(ggplot2) # version 3.5.1
library(lavaan)  # version 0.6-17

# settings
options(scipen = 9999)

# read in data
dataFull = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/sem/v2-2024/dataFiles/data3.csv")

##### DATA CHECKS #####

# look at descriptives
# check the sd and skew
dataFull |>
  psych::describe()

# visually examine normality
ggplot2::ggplot(dataFull, aes(x = i1)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i2)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i3)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i4)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i5)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i6)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i7)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i8)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i9)) +
  ggplot2::geom_histogram(binwidth = 1)

##### FULL STRUCTURAL & MEASUREMENT MODEL #####

lavaan::sem(
  "# measurement
  x =~ i1 + i2 + i3
  m =~ i4 + i5 + i6
  y =~ i7 + i8 + i9
  # structural
  y ~ m + x
  m ~ x",
  data = dataFull
) |>
  lavaan::summary(
    standardized = TRUE,
    fit.measures = TRUE
  )

##### END OF CODE #####
