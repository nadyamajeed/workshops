##### START OF CODE #####
# Nadyanna M. Majeed
# DawnLab Linear Regression Workshop 2023-05 v1

# set working directory
setwd("...") # change ... to whatever your working directory is

# R version 3.6.3
library(haven)        # version 2.4.3
library(dplyr)        # version 1.0.10
library(psych)        # verison 2.2.9
library(Hmisc)        # version 4.6-0
library(effectsize)   # version 0.6.0.1
library(interactions) # version 1.1.5

##### data cleaning -----

d =
  # import data
  haven::read_sav("https://github.com/nadyamajeed/workshops/raw/main/regression/dawnreg-2023-05.sav") %>%
  # clean data
  dplyr::mutate(
    # reverse age
    D_SEX = 1 - D_SEX,
    # reverse opt items
    LOT_P1 = 6 - LOT_P1,
    LOT_P2 = 6 - LOT_P2,
    LOT_P3 = 6 - LOT_P3,
    # get opt composite
    LOToverall = rowSums(across(starts_with("LOT_"))),
    # mean-centre age and income
    D_AGE.c = scale(D_AGE, center = TRUE, scale = FALSE) %>% as.numeric(),
    D_INCOME.c = scale(D_INCOME, center = TRUE, scale = FALSE) %>% as.numeric())

##### checking -----

# checking reliability of the LOT scale using cronbach's alpha
d %>%
  dplyr::select(starts_with("LOT_")) %>%
  psych::alpha()

# descriptives
d %>%
  dplyr::select(
    D_AGE,     # age
    D_INCOME,  # income
    D_LAD_1_1, # subjective social standing
    LOToverall # overall optimism
  ) %>%
  psych::describe()

# zero order correlations
d %>%
  dplyr::select(
    D_AGE, D_INCOME, D_LAD_1_1, LOToverall
  ) %>%
  as.matrix() %>%
  Hmisc::rcorr()

##### linear regression -----

#> QN1: is optimism predicted by age? ----

results1 = lm(
  LOToverall ~ 1 + D_AGE,
  data = d
)

results1 %>% summary()
results1 %>% effectsize::standardize_parameters()

#> QN2: is optimism predicted by age and income? ----

results2 = lm(
  LOToverall ~ 1 + D_AGE + D_INCOME,
  data = d
)

results2 %>% summary()
results2 %>% effectsize::standardize_parameters()

#> QN3: does income affect the effect of age on optimism? ----

results3 = lm(
  LOToverall ~ 1 + D_AGE.c * D_INCOME.c,
  # D_AGE.c * D_INCOME.c auto expands to D_AGE.c + D_INCOME.c + D_AGE.c:D_INCOME.c
  data = d
)

results3 %>% summary()
# for person of income = 0, effect of age on optimism is 0.13 (nonsig)
# ***IF*** IT WAS SIG => every time age increases by 1 year, optimism will increase by 0.13 units
# for person of age = 0, effect of income on optimism is 0.67 (sig)
# => every time income increases by 1 unit, optimism will increase by 0.67 units
# interaction between age and income is -0.03 (nonsig)
# ***IF*** IT WAS SIG, WE WOULD INTERPRET AS:
# for person of low income (1 unit below mean):
# effect of age on optimism is 0.13+(-1)*-0.03 = 0.16
# for person of high income (1 unit above mean):
# effect of age on optimsim is 0.13+(1)*-0.03 = 0.10
# conclusion: stronger positive assoc between age & optimism for lower income people
results3 %>% effectsize::standardize_parameters()
# => every time income increases by 1 SD, optimism will increase by 0.19 SD
# etc.

# IF THE INTERACTION WAS SIG, WE NEED TO PROBE
# 1. simple slopes
# 2. johnson-neyman analyses
# can do both at once using interactions::probe_interaction()
interactions::probe_interaction(results3, pred = "D_AGE.c", modx = "D_INCOME.c")

#> QN4: does age affect the effect of income on optimism? ----

# mathematically the same as QN3

# interaction between age and income is -0.03 (nonsig)
# ***IF*** IT WAS SIG, WE WOULD INTERPRET AS:
# for younger people (1 unit below mean):
# effect of income on optimism is 0.67+(-1)*-0.03 = 0.70
# for older people (1 unit above mean):
# effect of income on optimsim is 0.67+(1)*-0.03 = 0.64
# conclusion: stronger positive assoc between income & optimism for younger people

interactions::probe_interaction(results3, pred = "D_INCOME.c", modx = "D_AGE.c")
# "When D_AGE.c is INSIDE the interval [-3.16, 1.33], the slope of D_INCOME.c is p < .05."
# => for people from age = -3.16 to age = 1.33 (with reference to the mean age),
# => there is some relationship between age and optimism
# => for people younger than -3.16 or older than 1.33,
# => there is no sig relationship between age and optimism

##### END OF CODE #####
