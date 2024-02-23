##### START OF CODE #####

library(dplyr)

# create artificial data
simData = 
  dplyr::bind_rows(
    data.frame(ID = 1:100) %>%
      dplyr::mutate(
        profile = "A",
        x1 = sample(c(0, 0, 0, 1), nrow(.), replace = TRUE),
        x2 = sample(c(0, 0, 0, 1), nrow(.), replace = TRUE),
        x3 = sample(c(0, 0, 0, 1), nrow(.), replace = TRUE),
        x4 = sample(c(0, 0, 0, 1), nrow(.), replace = TRUE),
        x5 = sample(c(0, 0, 0, 1), nrow(.), replace = TRUE)),
    data.frame(ID = 101:200) %>%
      dplyr::mutate(
        profile = "B",
        x1 = sample(c(0, 0, 1), nrow(.), replace = TRUE),
        x2 = sample(c(0, 0, 1), nrow(.), replace = TRUE),
        x3 = sample(c(0, 0, 1), nrow(.), replace = TRUE),
        x4 = sample(c(0, 1), nrow(.), replace = TRUE),
        x5 = sample(c(0, 1), nrow(.), replace = TRUE)),
    data.frame(ID = 201:300) %>%
      dplyr::mutate(
        profile = "C",
        x1 = sample(c(0, 1), nrow(.), replace = TRUE),
        x2 = sample(c(0, 1), nrow(.), replace = TRUE),
        x3 = sample(c(0, 1), nrow(.), replace = TRUE),
        x4 = sample(c(0, 0, 1), nrow(.), replace = TRUE),
        x5 = sample(c(0, 0, 1), nrow(.), replace = TRUE))
  )

# look at incidence of each x indicator in each profile
with(simData, table(profile, x1))
with(simData, table(profile, x2))
with(simData, table(profile, x3))
with(simData, table(profile, x4))
with(simData, table(profile, x5))

# predict profile using the five x indicators
# https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
nnet::multinom(
  profile ~ x1 + x2 + x3 + x4 + x5, 
  data = simData)

##### END OF CODE #####
