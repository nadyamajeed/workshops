##### START OF CODE #####

library(dplyr)
library(ggplot2)
library(ggpubr)

# generate artificial data
# with heteroscedasticity
dataSimAss2 = 
  bind_rows(
    # lowest income ppl
    # need to spend most (85-95%) of their income
    data.frame(id = 1:250) |>
      mutate(
        income = sample(
          seq(100, 1000, 1),
          size = 250,
          replace = TRUE),
        spending = income * sample(seq(0.85, 0.95, 0.01), size = 250, replace = TRUE) + rnorm(250)
      ),
    # low-mid income ppl
    # need to spend most (75-90%) of their income
    data.frame(id = 251:500) |>
      mutate(
        income = sample(
          seq(1001, 2500, 1),
          size = 250,
          replace = TRUE),
        spending = income * sample(seq(0.75, 0.9, 0.01), size = 250, replace = TRUE) + rnorm(250)
      ),
    # mid income ppl
    # spend a lower amt w higher variance (65-85%) of their income
    data.frame(id = 501:750) |>
      mutate(
        income = sample(
          seq(2501, 5000, 1),
          size = 250,
          replace = TRUE),
        spending = income * sample(seq(0.65, 0.85, 0.01), size = 250, replace = TRUE) + rnorm(250)
      ),
    # high income ppl
    # highest variance in spending of income (55-80%)
    data.frame(id = 751:1000) |>
      mutate(
        income = sample(
          seq(5001, 10000, 1),
          size = 250,
          replace = TRUE),
        spending = income * sample(seq(0.55, 0.8, 0.01), size = 250, replace = TRUE) + rnorm(250)
      ),
    # super high income ppl
    # highest variance in spending of income (45-75%)
    data.frame(id = 1001:1250) |>
      mutate(
        income = sample(
          seq(10001, 15000, 1),
          size = 250,
          replace = TRUE),
        spending = income * sample(seq(0.45, 0.75, 0.01), size = 250, replace = TRUE) + rnorm(250)
      )
  )

# run linear regression
model = lm(spending ~ income, data = dataSimAss2)

# plot raw points against overall linreg line
p1 = ggplot(
  dataSimAss2, 
  aes(x = income, y = spending)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

# view
p1

# get fitted values (predicted spending based on income) and residuals
fittedvalues = fitted(model)
residuals = residuals(model)

# plot fitted values against residuals
# (this is checking for homoscedasticity)
p2 = ggplot(
  cbind(fittedvalues, residuals),
  aes(x = fittedvalues, y = residuals)) +
  geom_hline(yintercept = 0, colour = "blue") +
  geom_point() +
  theme_classic()

# view
# (observe the fan-like pattern
# which indicates heteroscedasticity,
# i.e., violation of assumption)
p2
  
# plot both side by side
ggarrange(
  p1,
  p2, 
  nrow = 1
)

##### END OF CODE #####
