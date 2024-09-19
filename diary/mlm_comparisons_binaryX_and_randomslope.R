##### START OF CODE #####
# version 0.1 (240919)

# R version 4.4.1
library(dplyr)       # version 1.1.4
library(lme4)        # version 1.1-35.5
library(lmerTest)    # version 3.1-3
library(nlme)        # version 3.1-166
library(lavaan)      # version 0.6-18
library(effectsize)  # version 0.8.9
library(broom.mixed) # version 0.2.9.5
library(ggplot2)     # version 3.5.1

##### SIMULATE DATA #####

simData = 
  data.frame(
    ID = 1:250,
    temp = sample(c(0, 1), size = 250, replace = TRUE)
  ) %>%
  dplyr::bind_rows(., ., ., ., ., ., ., ., ., .) %>%
  dplyr::mutate(
    Xdi = rnorm(m = temp, sd = 0.5, n = 250*10)
  )

simData = data.frame(); for(p in 1:500) {
  simData = dplyr::bind_rows(
    simData,
    data.frame(
      ID = p,
      random_intercept = rnorm(1),
      W_trait = rnorm(1),
      X = sample(c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), size = 7, replace = TRUE)
    ) %>% dplyr::mutate(X_mean = mean(X))
  )
}; simData = simData %>%
  dplyr::mutate(
    Y = random_intercept + 
      0.5*X + 0.5*X_mean + 
      -0.25*W_trait +
      -0.25*X*W_trait + 
      rnorm(n = 250, mean = 0, sd = 0.125)); rm(p)

##### DO LME4, NLME, AND LAVAAN GIVE THE SAME RESULTS FOR THE ABOVE MULTILEVEL MODEL? #####

#> THROUGH LME4 ----

results_lme4 = lmerTest::lmer(
  Y ~ 1 + X + X_mean + W_trait + X*W_trait + (1 | ID),
  data = simData,
  REML = FALSE
) %>% broom.mixed::tidy()

#> THROUGH NLME ----

results_nlme = nlme::lme(
  fixed = Y ~ 1 + X + X_mean + W_trait + X*W_trait,
  random = ~ 1 | ID,
  data = simData,
  method = "ML"
) %>% broom.mixed::tidy()

#> THROUGH LAVAAN ----

# nope lavaan cant do this yet

results_lavaan = lavaan::sem(
  "
  level: 1
  Y ~ slope*X
  level: 2
  Y ~ X_mean + W_trait
  slope ~ W_trait
  ",
  cluster = "ID",
  data = simData
) %>% broom.mixed::tidy()

##### END OF CODE #####
