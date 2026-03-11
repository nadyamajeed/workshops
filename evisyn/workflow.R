##### START OF CODE #####

# R version 4.5.0
library(dplyr)   # version 1.2.0
library(metafor) # version 4.8-0
library(psych)   # version 2.6.1
library(ggplot2) # version 4.0.2

##### IMPORT META-ANALYTIC DATA AND COMPUTE EFFECT SIZES #####
# in actual case you would probably only have one import
# here we have three imports for demonstration purposes

# meta-analytic dataset 1 - traditional, SMD
d_SMD_SIMPLE = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_simple_SMD.csv") %>%
  metafor::escalc(
    measure = "SMD",
    n1i = n1, n2i = n2,
    m1i = m1, m2i = m2,
    sd1i = sd1, sd2i = sd2,
    data = .)

# meta-analytic dataset 2 - multilevel, SMD
d_SMD_MULTI =
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_multi_SMD.csv") %>%
  metafor::escalc(
    measure = "SMD",
    n1i = n1, n2i = n2,
    m1i = m1, m2i = m2,
    sd1i = sd1, sd2i = sd2,
    data = .) %>%
  dplyr::mutate(unique_id = row.names(.))

# meta-analytic dataset 3 - multilevel, COR
d_COR_MULTI = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_multi_COR.csv") %>%
  metafor::escalc(
    measure = "ZCOR",
    ni = sample_size, ri = corr_nfcwb,
    data = .) %>%
  dplyr::mutate(
    effect_id = rownames(.),
    year = as.numeric(substr(year, 1, 4)))

##### META-ANALYTIC WORKFLOW (DEMONSTRATION WITH SIMPLE SMD DATA) #####

# obtain overall effect size
ovr1 = metafor::rma(
  yi = yi, vi = vi,
  data = d_SMD_SIMPLE,
  method = "REML"
)

# look at overall results
ovr1 %>% summary()

# overall forest plot
ovr1 %>% metafor::forest(order = "obs", header = TRUE, ilab = country)

# examine moderator (country) through meta-regression
# note that Qm stat is asking 'are any of the non-ref categories different from ref category'
# NOT typical anova omnibus asking 'are any of the pairs (incl non-ref vs non-ref) different'
metafor::rma(
  yi = yi, vi = vi,
  mods = ~ country,
  data = d_SMD_SIMPLE,
  method = "REML"
)
# test of moderators is nonsig, Q(2)=0.73, p=.695
# but if it was sig, we would get the country-specific effect sizes, eg:
metafor::rma(
  yi = yi, vi = vi,
  mods = ~ 0 + country, # <- remove intercept so that there's no reference category. note that this changes the Qm test to ask 'are any of the categories different from 0' (not 'are any of the categories different from reference')
  data = d_SMD_SIMPLE,
  method = "REML"
)

# examine sensitivity/bias
metafor::funnel(ovr1, legend = TRUE)
metafor::ranktest(ovr1)
metafor::fsn(ovr1)
# see https://github.com/nadyamajeed/workshops/blob/main/evisyn/eggers.R
# for eggers test correction for SMD effect sizes

##### META-ANALYTIC WORKFLOW (DEMONSTRATION WITH MULTILEVEL CORRELATION DATA) #####

# obtain overall effect size
# and indicate sample ID as label
# for later plotting purposes
ovr3 = metafor::rma.mv(
  yi = yi, V = vi,
  random = ~ 1 | sample_id/effect_id, # <- effects are nested within samples
  data = d_COR_MULTI,
  method = "REML",
  slab = paste0("Sample ", sample_id)
)

# look at overall results
# also convert from Fisher's z to Pearson's r
ovr3 %>% summary()
ovr3$b %>% psych::fisherz2r()

# overall forest plot
# not very useful as there's too many rows
ovr3 %>% metafor::forest(order = "obs", header = TRUE) 

# see https://github.com/nadyamajeed/workshops/blob/main/evisyn/eggers.R
# for eggers test for multilevel meta-analysis

# also see Hu et al 2026 https://doi.org/10.31234/osf.io/fmw7t_v4
# for discussion of sensitivity/bias tests in multilevel meta-analysis

# examine moderator (year) through meta-regression
# centre year on 2000 so that intercept is more interpretable
# (does not change significance of moderator)
ovr3_year = metafor::rma.mv(
  yi = yi, V = vi,
  random = ~ 1 | sample_id/effect_id, 
  mods = ~ I(year - 2000),
  data = d_COR_MULTI,
  method = "REML"
)
ovr3_year %>% summary()
# test of moderator is sig, Q(1)=4.45, p=.035
# plot it using ggplot2
ggplot() +
  # Plot the raw data points
  # BUT if sample has multiple effect sizes, just avg them for plotting purposes only so that plot is not too cluttered/confusing
  geom_point(
    data = d_COR_MULTI %>%
      dplyr::group_by(sample_id) %>%
      dplyr::mutate(
        sample_size = mean(sample_size),
        yi = mean(yi),
        corr_nfcwb = NULL,
        vi = NULL) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(sample_id, .keep_all = TRUE), 
    aes(
      x = year, 
      y = yi, 
      size = sample_size), 
    alpha = 0.3) +
  # Add the meta-analytic regression line
  geom_line(
    data = data.frame(
      years = seq(
        min(d_COR_MULTI$year, na.rm = TRUE), 
        max(d_COR_MULTI$year, na.rm = TRUE), 
        length.out = 100)) %>%
      dplyr::mutate(
        yi.pred = predict(ovr3_year, newmods = years - 2000)$pred), 
    aes(
      x = years, 
      y = yi.pred), 
    size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    size = "Study Sample Size",
    x = "Year",
    y = "Predicted Meta-Analytic Effect Size")

##### END OF CODE #####
