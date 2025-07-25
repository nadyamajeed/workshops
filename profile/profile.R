########## START OF CODE ##########

#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("tibble")
#install.packages("ggplot2")
#install.packages("mclust")
#devtools::install_github("Ringomed/ggvanced")

# load libraries

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(mclust)
library(ggvanced)

########## DATA ##########

# make artificial data
# with 3 profiles
dataSim = 
  dplyr::bind_rows(
    # profile 1
    data.frame(
      person = 1:100,
      depression = rnorm(100, m = 5, sd = 0.5),
      anxiety = rnorm(100, m = 1, sd = 0.5),
      stress = rnorm(100, m = 1, sd = 0.5),
      gpa = sample(seq(0, 2, 0.1), size = 100, replace = TRUE)
    ),
    # profile 2
    data.frame(
      person = 101:200,
      depression = rnorm(100, m = 1, sd = 0.5),
      anxiety = rnorm(100, m = 5, sd = 0.5),
      stress = rnorm(100, m = 1, sd = 0.5),
      gpa = sample(seq(0, 4, 0.1), size = 100, replace = TRUE)
    ),
    # profile 3
    data.frame(
      person = 201:300,
      depression = rnorm(100, m = 1, sd = 0.5),
      anxiety = rnorm(100, m = 1, sd = 0.5),
      stress = rnorm(100, m = 5, sd = 0.5),
      gpa = sample(seq(0, 2, 0.1), size = 100, replace = TRUE)
    )
  )

# take only cols that will be used for profile
dataSim_onlyprofilecols = dataSim %>%
  dplyr::select(depression, anxiety, stress)

########## CREATE PROFILES ##########

# find out how many profiles to create
profileOutput = mclust::mclustBIC(data = dataSim_onlyprofilecols)
profileOutput %>% summary()
profileOutput %>% plot()

# based on prev results,
# create 3 profiles
model_3profiles = mclust::Mclust(
  data = dataSim_onlyprofilecols, 
  G = 3,
  modelNames = "EEI",
  x = profileOutput)
model_3profiles %>% summary()

# add profile labels back to original data
# ensure profile labels are an ordered factor
dataSim_withprofiles =
  dplyr::bind_cols(
    dataSim,
    profile = model_3profiles$classification
  ) %>%
  dplyr::mutate(
    profile = factor(profile, levels = c(1, 2, 3))
  )

########## PLOT - SPAGHETTI ##########

model_3profiles$parameters$mean %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  tidyr::pivot_longer(
    cols = c(V1, V2, V3),
    names_to = "Profile",
    values_to = "Mean"
  ) %>%
  ggplot(aes(
    x = variable, 
    y = Mean, 
    group = Profile, 
    colour = Profile)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

########## PLOT - SPIDER ##########

model_3profiles$parameters$mean %>% 
  t() %>% as.data.frame() %>% 
  dplyr::mutate(profile = c(1, 2, 3)) %>%
  dplyr::select(profile, everything()) %>%
  ggspider()

########## USE PROFILES TO PREDICT OTHER VAR ##########

lm(gpa ~ profile, data = dataSim_withprofiles) %>%
  summary()

########## END OF CODE ##########
