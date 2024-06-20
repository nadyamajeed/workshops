##### START OF CODE #####

# R version 4.4.0
library(psych)      # version 2.4.3
library(ggplot2)    # version 3.5.1
library(lavaan)     # version 0.6-17
library(lavaanPlot) # version 0.8.1

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
ggplot2::ggplot(dataFull, aes(x = i01)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i02)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i03)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i04)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i05)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i06)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i07)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i08)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i09)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i10)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i11)) +
  ggplot2::geom_histogram(binwidth = 1)
ggplot2::ggplot(dataFull, aes(x = i12)) +
  ggplot2::geom_histogram(binwidth = 1)

##### FULL STRUCTURAL & MEASUREMENT MODEL #####

fit = lavaan::sem(
  "# measurement
   x =~ i01 + i02 + i03
   m =~ i04 + i05 + i06
  y1 =~ i07 + i08 + i09
  y2 =~ i10 + i11 + i12
  # structural
  y1 ~~ y2
  y1 + y2 ~ m + x
  m ~ x",
  data = dataFull
) 

fit |>
  lavaan::summary(
    standardized = TRUE,
    fit.measures = TRUE
  )

##### VISUALISE #####

fit |> 
  lavaanPlot::lavaanPlot2(include = "covs")

##### END OF CODE #####
