##### START OF CODE #####

library(dplyr)
library(ggplot2)
library(ggpubr)

# generate artificial data
# with non-normality of residuals
dataSimAss3 = 
  bind_rows(
    # normal people
    # residuals are normally distributed for them
    data.frame(id = 1:500) |>
      mutate(
        studyHours = sample(
          seq(0, 24, 0.5),
          size = 500,
          replace = TRUE),
        examScore = 
          0.75 * studyHours + rnorm(500)),
    # test-anxious people
    # residuals are large and negative for them
    # (despite same study hours, 
    # they always score worse than main sample)
    data.frame(id = 501:600) |>
      mutate(
        studyHours = sample(
          seq(0, 24, 0.5),
          size = 100,
          replace = TRUE),
        examScore = 
          0.75 * studyHours + rnorm(100) - 5)
  ) |>
  # fix examScores so theyre in a realistic range (bounded 0-100)
  mutate(
    examScore = (examScore - min(examScore)) / (max(examScore) - min(examScore)),
    examScore = round(examScore * 100)
  )

# run linear regression
model = lm(examScore ~ studyHours, data = dataSimAss3)

# plot raw points against overall linreg line
p1 = ggplot(
  dataSimAss3, 
  aes(x = studyHours, y = examScore)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") +
  theme_classic()

# view
p1

# get residuals
residuals = residuals(model)

# check normality of residuals
p2 = ggplot(
  as.data.frame(residuals),
  aes(x = residuals)) +
  geom_histogram(
    aes(y = after_stat(density)), bins = 30,
    fill = "lightgray", color = "black") +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(residuals), sd = sd(residuals)),
    color = "blue", linewidth = 1) +
  theme_classic()

# can also see that homoscedasticity is not violated
# no fan-like pattern observed
p3 = ggplot(
  cbind(fittedvalues = fitted(model), residuals),
  aes(x = fittedvalues, y = residuals)) +
  geom_hline(yintercept = 0, colour = "blue", linewidth = 2) +
  geom_point(size = 1) +
  theme_classic()

# view
p3

# plot all side by side
ggarrange(
  p1,
  p3,
  p2, 
  nrow = 1
)

##### END OF CODE #####
