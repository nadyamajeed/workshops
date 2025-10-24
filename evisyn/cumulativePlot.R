library(dplyr)
library(ggplot2)

# generate artificial data
simdata = 
  data.frame(
    age = rnorm(1000)
  ) %>%
  dplyr::mutate(
    dv = age + rnorm(nrow(.)),
    dv = scale(dv, center = FALSE, scale = TRUE),
    dv = as.numeric(dv),
    age = age - min(age),
    age = age / max(age),
    age = age * (95-18) + 18,
    age = round(age, 2)
  ) %>%
  dplyr::rename(
    sampleMeanAge = age,
    yi = dv
  )

# plot 1: scatterplot + linear best fit of raw data
ggplot(
  simdata,
  aes(x = sampleMeanAge, y = yi)) +
  geom_smooth(
    method = "lm" # linear best fit line
  ) +
  geom_point(
    size = 0.5
  ) +
  scale_x_continuous(
    "Sample Mean Age",
    limits = c(18, 95),
    breaks = seq(15, 100, 5)
  ) +
  scale_y_continuous(
    "Effect size",
    breaks = seq(-3, 3, 0.5)
  ) +
  theme(
    panel.grid.minor.x = element_blank()
  )

# plot 2: scatterplot of raw data + cumulative line
ggplot(
  simdata %>%
    dplyr::arrange(sampleMeanAge) %>%
    dplyr::mutate(yi_cumulative = cummean(yi)),
  aes(x = sampleMeanAge)) +
  geom_line(
    aes(y = yi_cumulative) # spaghetti line
  ) +
  geom_point(
    aes(y = yi),
    size = 0.5
  ) +
  scale_x_continuous(
    "Sample Mean Age",
    limits = c(18, 95),
    breaks = seq(15, 100, 5)
  ) +
  scale_y_continuous(
    "Effect size",
    breaks = seq(-3, 3, 0.5)
  ) +
  theme(
    panel.grid.minor.x = element_blank()
  )
