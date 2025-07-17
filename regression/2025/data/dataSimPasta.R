library(dplyr)

data.frame(
  id = 1:5000
) %>%
  dplyr::mutate(
    pastasPerDay = sample(
      seq(0, 6, 0.5),
      size = nrow(.),
      replace = TRUE),
    pastaLiking = sample(
      seq(-10, 10, 0.01),
      size = nrow(.),
      replace = TRUE),
    happinessScore = 
      rnorm(nrow(.)) + 
      0.01*pastasPerDay +
      0.001*pastaLiking +
      0.008*pastasPerDay*pastaLiking
  ) %>%
  write.csv("dataSimPasta.csv", row.names = FALSE)
