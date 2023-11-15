##### START OF CODE #####

# R version 4.3.1

# uncomment and run the following lines if packages are not already installed:
# install.packages("metafor")
# install.packages("lmerTest")

##### GENERATE SOME DATA #####

# set up some changeable parameters
no_of_samples = 25
n_min = 25
n_max = 100
ndiff_max = 10

# create single simulated dataset
# of a meta-analysis of SMDs

# group 1
n1 = sample(n_min:n_max, size = no_of_samples, replace = TRUE)
m1 = rnorm(n = no_of_samples, mean = 5, sd = 1) |> round(2)
sd1 = 
  (sample((0.5*n_max):(1.25*n_max), size = no_of_samples, replace = TRUE) /
     n1 ) |> 
  round(2)

# group 2
n2 = n1 + sample(-ndiff_max:ndiff_max, size = no_of_samples, replace = TRUE)
m2 = m1 - rnorm(n = no_of_samples, mean = 4, sd = 0.5) |> round(2)
sd2 = 
  (sample((0.5*n_max):(1.25*n_max), size = no_of_samples, replace = TRUE) /
     n2 ) |> 
  round(2)

# combine
data_recorded_A = data.frame(
  sample = 1:no_of_samples,
  n1, m1, sd1,
  n2, m2, sd2
)

# create SMDs (hedge's g)
data_g_A = metafor::escalc(
  "SMD", 
  n1i = n1, m1i = m1, sd1i = sd1,
  n2i = n2, m2i = m2, sd2i = sd2,
  data = data_recorded_A
)

##### CONDUCT EGGER'S REGRESSION TEST #####

# two options
# will give the exact same results

# original (Egger et al., 1997; doi:10.1136/bmj.315.7109.629):
# g/SE = intercept + slope*1/SE

# rearranged (Rothstein et al., 2005; doi:10.1002/0470870168):
# g = slope*SE + intercept
# weighted by 1/v

#> test via lm -----
# corresponds to original

lm(
  # g weighted by SE is predicted by intercept and inverse SE
  I(yi/sqrt(vi)) ~ 1 + I(1/sqrt(vi)),
  data = data_g_A
) |> 
  # estimate of interest is the intercept
  summary()

#> test via metafor -----
# corresponds to rearranged

metafor::rma(
  # indicate g and variance
  yi = yi, vi = vi,
  # indicate moderator which is SE
  mods = ~ I(sqrt(vi)),
  # indicate weight which is inverse SE^2
  weights = 1/vi,
  data = data_g_A
) |>
  # estimate of interest is the slope
  summary()

##### PUSTEJOVSKY-RODGERS ADJUSTMENT FOR SMD #####
# doi:10.1002/jrsm.1332

# using egger's test (unadjusted) on SMDs
# results to inflated type 1 error
# as SMD and SE are not independent

# hence, use corrected formula for SE
# SE_corrected = sqrt((n1+n2)/(n1*n2))

data_g_A$SE_corrected = with(data_g_A, sqrt((n1+n2)/(n1*n2)))

#> test via lm -----

lm(
  # g weighted by SE is predicted by intercept and inverse SE
  I(yi/SE_corrected) ~ 1 + I(1/SE_corrected),
  data = data_g_A
) |> 
  # estimate of interest is the intercept
  summary()

#> test via metafor -----

metafor::rma(
  # indicate g and variance
  yi = yi, vi = vi,
  # indicate moderator which is SE
  mods = ~ I(SE_corrected),
  # indicate weight which is inverse SE^2
  weights = 1/SE_corrected^2,
  data = data_g_A
) |>
  # estimate of interest is the slope
  summary()

##### EXTENSION TO THREE-LEVEL META-ANALYSIS #####

# create second dataset
# which is based on (i.e., correlated with) the first
# this will be our second effect size per sample.
# for simplicity purposes, let us assume
# only the means change, and the SDs stay the same.

# duplicate the data
data_recorded_B = data_recorded_A

# change the means slightly
data_recorded_B$m1 = (data_recorded_B$m1 + rnorm(no_of_samples)) |> round(2)
data_recorded_B$m2 = (data_recorded_B$m2 + rnorm(no_of_samples)) |> round(2)

# recompute g and v and corrected SE
# note that corrected SE will be the same
# as it is only determined by sample size
# which is the same as it's drawn from the same sample
data_g_B = metafor::escalc(
  "SMD", 
  n1i = n1, m1i = m1, sd1i = sd1,
  n2i = n2, m2i = m2, sd2i = sd2,
  data = data_recorded_B
)
data_g_B$SE_corrected = with(data_g_B, sqrt((n1+n2)/(n1*n2)))

# combine the data
data_g_all = rbind(data_g_A, data_g_B)

#> test via lmer -----

lmerTest::lmer(
  # g weighted by SE is predicted by intercept and inverse SE
  # with random intercept by sample
  I(yi/SE_corrected) ~ 1 + I(1/SE_corrected) + (1 | sample),
  data = data_g_all
) |> 
  # estimate of interest is the intercept
  summary(correlation = FALSE)

# metafor::rma.mv does not have a weights argument.
# thus for three (or more) level meta,
# lmerTest::lmer can/should be used instead

##### END OF CODE #####
