##### START OF CODE #####

# R version 4.4.1
library(dplyr)        # version 1.1.4
library(psych)        # version 2.5.3
library(effectsize)   # version 1.0.1
library(interactions) # version 1.2.0
library(ggplot2)      # version 3.5.2

# display options
options(scipen = 99999)

# import and clean data
d = 
  # import
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/regression/2025/data/dataSimPasta.csv") %>%
  # centre predictors
  dplyr::mutate(across(
    .cols = c(pastasPerDay, pastaLiking),
    .fns = ~ as.numeric(scale(.x, center = TRUE, scale = FALSE)),
    .names = "{.col}.c"
  ))

# look at descriptives
d %>% 
  psych::describe() %>%
  as.data.frame() %>%
  dplyr::select(n, mean, sd, skew, min, max, range)

##### SIMPLE LINEAR REGRESSION (X1 -> Y1) #####

# look at scatterplot
ggplot(d, aes(x = pastasPerDay, y = happinessScore)) +
  geom_jitter(size = 1)

# run simple linear regression
results1 = lm(
  happinessScore ~ 1 + pastasPerDay,
  data = d)

# look at results
results1 %>% summary()

# get std coeffs
results1 %>% effectsize::standardize_parameters()

##### LINEAR REGRESSION WITH TWO PREDICTORS (X1 + X2 -> Y1) #####

# run linear regression
results2 = lm(
  happinessScore ~ 1 + pastasPerDay + pastaLiking,
  data = d)

# look at results
results2 %>% summary()

# get std coeffs
results2 %>% effectsize::standardize_parameters()

##### LINEAR REGRESSION WITH INTERACTION (X1 * X2 -> Y1) #####

# run linear regression
results3 = lm(
  happinessScore ~ 1 + pastasPerDay.c * pastaLiking.c, # a * b = a + b + a:b
  data = d)

# look at results
results3 %>% summary()

# get std coeffs
results3 %>% effectsize::standardize_parameters()

# if the interaction was sig
# we would want to probe
# using simple slopes & johnson-neyman analysis

# simple slopes
interactions::probe_interaction(
  results3,
  pred = "pastasPerDay.c",
  modx = "pastaLiking.c")

# johnson-neyman analysis
interactions::johnson_neyman(
  results3,
  pred = "pastasPerDay.c",
  modx = "pastaLiking.c")

##### END OF CODE #####
