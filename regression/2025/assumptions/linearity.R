##### START OF CODE #####

library(dplyr)
library(ggplot2)

# generate artificial data
# with non-normality of residuals
dataSimAss1 = 
  data.frame(id = 1:500) |>
  mutate(
    iv = rnorm(500),
    dv = rnorm(500) + 2.5*(iv) + 1.25*(iv^2)
  )

# run linear regression
model = lm(dv ~ iv, data = dataSimAss1)

# plot raw points against overall linreg line
ggplot(
  dataSimAss1, 
  aes(x = iv, y = dv)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") +
  theme_classic()

##### END OF CODE #####
