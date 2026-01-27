library(MASS)
library(lavaan)
library(dplyr)
library(broom)
library(purrr)

# set population parameters
beta_mx = 1
beta_y1x = 0.5
beta_y2x = 0.5
beta_y1m = 0.25
beta_y2m = 0.5
residcor_y1y2 = 0.25

# set sample parameters
N = 100

# set repetition number
reps = 100

# function to generate data
give_me_my_data = function(N) {
  y_residuals = 
    MASS::mvrnorm(
      n = N, 
      mu = c(0, 0), 
      Sigma = matrix(c(1, residcor_y1y2, 
                       residcor_y1y2, 1), 
                     nrow = 2))
  data_sim = 
    data.frame(x = rnorm(N)) %>%
    dplyr::mutate(
      m = beta_mx*x + rnorm(N),
      y1 = beta_y1m*m + beta_y1x*x + y_residuals[, 1],
      y2 = beta_y2m*m + beta_y2x*x + y_residuals[, 2]
    )
  return(data_sim)
}

# conduct repetitions
out = purrr::map_dfr(1:reps, ~{
  lavaan::sem(
    "m ~ a*x
  y1 ~ b1*m + c1*x
  y2 ~ b2*m + c2*x
  y1 ~~ y2",
    data = give_me_my_data(N)
  ) %>% 
    broom::tidy()
}, .id = "replication")

# examine power (of path b1)
out %>%
  dplyr::filter(label == "b1") %>%
  dplyr::summarise(power = mean(p.value < .05)) 
