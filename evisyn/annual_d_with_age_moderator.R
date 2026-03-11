library(dplyr)
library(metafor)
library(psych)
library(ggplot2)

# some fake data that would be extracted from primary studies
fakeStudies = 
  data.frame(study_id = 1:100) %>%
  dplyr::mutate(
    # sample size
    n = sample(
      50:500,
      size = nrow(.),
      replace = TRUE
    ),
    # sample mean age at time 1
    age_t1 = sample(
      18:95,
      size = nrow(.),
      replace = TRUE
    ),
    # length of time interval (random 1-10)
    t_interval = sample(
      seq(from = 1, to = 10, by = 0.5), 
      size = nrow(.), 
      replace = TRUE),
    # correlation between time 1 and time 2
    age_temp = (age_t1 - min(age_t1)) / (max(age_t1) - min(age_t1)),
    r_t1_t2 = 0.99 * exp(-(0.3 * (1.1 - age_temp)) * (t_interval - 1)) + rnorm(n(), m = 0, sd = 0.05), # exponential decay of r as interval increases
    r_t1_t2 = case_when(
      r_t1_t2 < 0 ~ 0.01,
      r_t1_t2 > .99 ~ .99,
      .default = r_t1_t2
    ),
    age_temp = NULL,
    # mean & sd at time 1
    m_t1 = rnorm(n = nrow(.), m = 5, sd = 1),
    sd_t1 = abs(rnorm(n = nrow(.), m = 2.5, sd = 0.5)),
    # mean at time 2
    m_t2 = (r_t1_t2 * m_t1) + sqrt(1 - r_t1_t2^2) * rnorm(n = nrow(.)),
    # randomly make 20% of r values missing (more realistic)
    r_t1_t2 = if_else(runif(n()) < 0.20, NA_real_, r_t1_t2)
  )

##### method following description given in Orth et al 2024 10.1037/bul0000436 #####

# before we can process the data,
# we need to obtain the meta-analytic r_t1_t2
# so that we can use that as imputed value for 
# the studies which did not report r_t1_t2
meta_z_t1_t2 = metafor::rma(
  yi = yi,
  vi = vi,
  data = fakeStudies %>%
    metafor::escalc(
      data = .,
      measure = "ZCOR",
      ri = r_t1_t2,
      ni = n),
  method = "REML" # random effects
)$b[1, 1]
meta_r_t1_t2 = psych::fisherz2r(meta_z_t1_t2)

# now we can process the data
fakeStudies_processed =
  fakeStudies %>%
  dplyr::mutate(
    # smd
    d = (m_t2 - m_t1) / sd_t1,
    d.annual = d / t_interval,
    # variance
    r_temp = ifelse(
      !is.na(r_t1_t2),
      r_t1_t2,
      meta_r_t1_t2
    ),
    v = ((2  * (1 - r_temp)) / n) + (d^2 / (2 * n)),
    v.annual = v / (t_interval^2),
    r_temp = NULL
  )

# overall meta-analytic model for 1-year change in mean level
results1 = metafor::rma(
  yi = d.annual,
  vi = v.annual,
  data = fakeStudies_processed,
  method = "REML" # random effects
)

# check if 1-year change in mean level is different depending on age
results2 = metafor::rma(
  yi = d.annual,
  vi = v.annual,
  mods = age_t1,
  data = fakeStudies_processed,
  method = "REML" # random effects
)

# plot effect against age
plothelp_agerange = seq(
  min(fakeStudies_processed$age_t1), 
  max(fakeStudies_processed$age_t1), 
  length.out = 100)
plothelp_preds = predict(results2, newmods = plothelp_agerange)
plothelp_data = data.frame(
  age_t1 = plothelp_agerange,
  d.annual_pred = plothelp_preds$pred)
ggplot() +
  # Bubble plot: points sized by inverse variance (weight)
  geom_point(data = fakeStudies_processed, 
             aes(x = age_t1, y = d.annual, size = n), 
             alpha = 0.4, color = "steelblue") +
  # Regression Line
  geom_line(data = plothelp_data, 
            aes(x = age_t1, y = d.annual_pred), 
            color = "firebrick", linewidth = 1) +
  # Add a horizontal line at 0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Meta-regression: Age as a Moderator of Annual Change",
    x = "Mean Age at Time 1",
    y = "Annual Change (d)"
  ) +
  theme_minimal()
