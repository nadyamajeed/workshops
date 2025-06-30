##### START OF CODE #####

# R 4.5.0
library(dplyr)    # 1.1.4
library(tidyr)    # 1.3.1
library(psych)    # 2.5.3
library(merTools) # 0.6.2

dataSim = 
  data.frame(
    id = 1:200
  ) %>%
  dplyr::mutate(
    latent = rnorm(n = nrow(.), mean = 0, sd = 1),
    time1 = latent + rnorm(n = nrow(.), mean = 0, sd = 1),
    time2 = latent + rnorm(n = nrow(.), mean = 0, sd = 1),
    time1 = round(time1),
    time2 = round(time2),
    latent = NULL
  )

# look at spearman's correlation
with(dataSim, cor(time1, time2, method = "spearman"))

# look at pearson's correlation
with(dataSim, cor(time1, time2, method = "pearson"))

# look at icc (via psych)
psych::ICC(
  # data must be in wide format
  # with extra cols removed
  x = dataSim %>%
    dplyr::select(time1, time2),
  # other settings
  lmer = TRUE,
  check.keys = TRUE
)

# look at icc (via merTools)
merTools::ICC(
  # data must be in long format
  # with ID col included
  data = dataSim %>%
    tidyr::pivot_longer(
      cols = c("time1", "time2"),
      names_to = "time",
      names_prefix = "time",
      values_to = "rating"
    ),
  # indicate col with the ratings
  outcome = "rating",
  # indicate col with the ID
  group = "id"
)

##### END OF CODE #####
