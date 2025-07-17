library(dplyr)

data.frame(
  id = 1:500
) %>%
  dplyr::mutate(
    pastasPerDay = sample(
      seq(0, 6, 1),
      size = nrow(.),
      replace = TRUE),
    pastaLiking = sample(
      seq(-3, 3, 0.01),
      size = nrow(.),
      replace = TRUE),
    happinessScore = 
      rnorm(nrow(.)) + 
      0.3*pastasPerDay +
      0.1*pastaLiking +
      0.5*pastasPerDay*pastaLiking
  ) %>%
  write.csv("dataSimPasta.csv", row.names = FALSE)
