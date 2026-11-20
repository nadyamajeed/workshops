# START OF CODE #

# R version 4.5.0
library(dplyr) # version 1.2.1
library(pwr)   # version 1.3-0

# "study 1" -----

# raw data
data.raw = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/regression/2025/data/dataSim20.csv")

# clean data
# take only relevant cols (x, m, y) & std them
data.clean = data.raw %>%
  dplyr::mutate(
    across(
      .cols = c(extraversion, optimism, gratitude),
      .fns = ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))
    ),
    .keep = "none"
  )

# check std
# should have M=0 and SD=1
data.clean %>%
  dplyr::summarise(
    across(
      .cols = c(extraversion, optimism, gratitude),
      .fns = ~ round(mean(.x), 2),
      .names = "{.col}_mn"
    ),
    across(
      .cols = c(extraversion, optimism, gratitude),
      .fns = ~ round(sd(.x), 2),
      .names = "{.col}_sd"
    )
  )

# find corr between x & m
# and note the coeff
# LATER, PUT INTO rho_xz
data.clean %>%
  with(cor(gratitude, optimism))

# run lm (linear regression)
# and note the reg coeffs
# LATER, PUT INTO beta1 beta2 beta3
# where beta3 is the interaction
lm(
  extraversion ~ gratitude * optimism,
  data = data.clean
) %>%
  summary()

# sample size determination for "study 2" -----

# CHANGE INPUTS HERE
beta1  = -0.47642
beta2  = -0.08922
beta3  =  0.06072
rho_xz = -0.3615911

# DO NOT TOUCH
delta_r2 = beta3^2 * (1 + rho_xz^2)
r2_full  = beta1^2 + beta2^2 + 2 * rho_xz * beta1 * beta2 + delta_r2
f2_int   = delta_r2 / (1 - r2_full)

# DO NOT TOUCH
# "Exactly one of the parameters 'u','v','f2','power' and 'sig.level' must be passed as NULL, and that parameter is determined from the others."
res = pwr::pwr.f2.test(
  u = 1,    # u = df for numerator (1 tested interaction term)
  v = NULL, # v = df for denominator = N - k - 1 (where k = total predictors = 3)
  f2 = f2_int, 
  sig.level = 0.05, 
  power = 0.80
)

# DO NOT TOUCH
n_required = ceiling(res$v + 3 + 1) # v + k + 1
cat(sprintf("f2_int = %.4f | Required N = %d\n", f2_int, n_required))

# END OF CODE #
