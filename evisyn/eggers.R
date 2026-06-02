##### START OF CODE #####

# R version 4.5.0

# uncomment and run the following lines if packages are not already installed:
# install.packages("metafor")  # version 5.0-1
# install.packages("lmerTest") # version 3.2-1

##### READ IN AND CLEAN DATA #####

# create SMDs (hedge's g)
data_g_simple = metafor::escalc(
  # read in data
  data = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/evisyn/data/data_simple_SMD.csv"),
  # compute effect size and variance
  measure = "SMD", 
  n1i = n1, m1i = m1, sd1i = sd1,
  n2i = n2, m2i = m2, sd2i = sd2
)

# compute SE = sqrt(variance)
# following metafor documentation
data_g_simple$sei = sqrt(data_g_simple$vi)

##### CONDUCT EGGER'S REGRESSION TEST #####

# two equations
# will give the exact same results

# original (Egger et al., 1997; doi:10.1136/bmj.315.7109.629):
# g/SE = intercept + slope*1/SE

# rearranged (Rothstein et al., 2005; doi:10.1002/0470870168):
# g = slope*SE + intercept
# weighted by 1/SE^2

#> test via lm -----
# corresponds to original

lm(
  # g weighted by SE is predicted by intercept and inverse SE
  I(yi/sei) ~ 1 + I(1/sei),
  data = data_g_simple
) |> 
  # estimate of interest is the intercept
  summary()

#> test via metafor (1) -----

metafor::rma(
  # indicate model for normal overall effect size
  yi = yi, vi = vi,
  data = data_g_simple
) |>
  # put into regtest with model = "lm"
  # see "Test for Funnel Plot Asymmetry" line in output
  metafor::regtest(model = "lm")

#> test via metafor (2) -----
# corresponds to rearranged

metafor::rma(
  # indicate g and variance
  yi = yi, vi = vi,
  # indicate moderator which is SE
  mods = ~ sei,
  # indicate weight which is inverse SE^2
  weights = 1/sei^2,
  data = data_g_simple
) |>
  # estimate of interest is the slope
  # note that the estimate is the same as previous two methods
  # but the test statistic and hence p-value are not
  summary()

##### PUSTEJOVSKY-RODGERS ADJUSTMENT FOR SMD #####
# doi:10.1002/jrsm.1332

# using egger's test (unadjusted) on SMDs
# results in inflated type 1 error
# as SMD and SE are not independent

# hence, use corrected formula for SE
# SE corrected = sqrt((n1+n2)/(n1*n2))

data_g_simple$sei_corrected = with(data_g_simple, sqrt((n1+n2)/(n1*n2)))

#> test via lm -----

lm(
  # g weighted by SE is predicted by intercept and inverse SE
  I(yi/sei_corrected) ~ 1 + I(1/sei_corrected),
  data = data_g_simple
) |> 
  # estimate of interest is the intercept
  summary()

#> test via metafor -----

metafor::rma(
  # indicate g and variance
  yi = yi, vi = vi,
  # indicate moderator which is SE
  mods = ~ sei_corrected,
  # indicate weight which is inverse SE^2
  weights = 1/sei_corrected^2,
  data = data_g_simple
) |>
  # estimate of interest is the slope
  # note that the estimate is the same as previous method
  # but the test statistic and hence p-value are not
  summary()

##### EXTENSION TO THREE-LEVEL META-ANALYSIS #####

# create SMDs (hedge's g)
data_g_multi = metafor::escalc(
  # read in data
  data = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/evisyn/data/data_multi_SMD.csv"),
  # compute effect size and variance
  measure = "SMD", 
  n1i = n1, m1i = m1, sd1i = sd1,
  n2i = n2, m2i = m2, sd2i = sd2
)

# compute SE = sqrt(variance)
# following metafor documentation
data_g_multi$sei = sqrt(data_g_multi$vi)

# compute corrected SE
data_g_multi$sei_corrected = with(data_g_multi, sqrt((n1+n2)/(n1*n2)))

#> test via lmer -----

lmerTest::lmer(
  # g weighted by SE is predicted by intercept and inverse SE
  # with random intercept by sample
  I(yi/sei_corrected) ~ 1 + I(1/sei_corrected) + (1 | sample),
  data = data_g_multi
) |> 
  # estimate of interest is the intercept
  summary(correlation = FALSE)

#> test via metafor -----

metafor::rma.mv(
  # indicate g and variance
  yi = yi, V = vi,
  # indicate moderator which is SE
  mods = ~ sei_corrected,
  # indicate weight which is inverse SE^2
  W = 1/sei_corrected^2,
  data = data_g_multi
) |>
  # estimate of interest is the slope
  # note that the estimate is the same as previous method
  # but the test statistic and hence p-value are not
  summary()

# metafor::rma.mv does not have a weights argument.
# metafor::regtest does not support rma.mv objects.
# thus for three (or more) level meta,
# lmerTest::lmer can/should be used instead.

##### END OF CODE #####
