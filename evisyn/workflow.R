##### START OF CODE #####

# R version 4.4.0
library(dplyr)   # version 1.1.4
library(metafor) # version 4.6-0
library(psych)   # version 2.4.3

##### IMPORT META-ANALYTIC DATA AND COMPUTE EFFECT SIZES #####
# in actual case you would probably only have one import
# here we have three imports for demonstration purposes

# meta-analytic dataset 1 - simple SMD
d_SMD_SIMPLE = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_simple_SMD.csv") %>%
  metafor::escalc(
    measure = "SMD",
    n1i = n1, n2i = n2,
    m1i = m1, m2i = m2,
    sd1i = sd1, sd2i = sd2,
    data = .)

# meta-analytic dataset 2 - multi SMD
d_SMD_MULTI =
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_multi_SMD.csv") %>%
  metafor::escalc(
    measure = "SMD",
    n1i = n1, n2i = n2,
    m1i = m1, m2i = m2,
    sd1i = sd1, sd2i = sd2,
    data = .) %>%
  dplyr::mutate(unique_id = row.names(.))

# meta-analytic dataset 3 - simple COR
d_COR_SIMPLE = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_simple_COR.csv") %>%
  metafor::escalc(
    measure = "ZCOR",
    ni = sample_size, ri = corr_nfcwb,
    data = .)

##### META-ANALYTIC WORKFLOW (DEMONSTRATION WITH SIMPLE SMD DATA) #####

# obtain overall effect size
ovr1 = metafor::rma(
  yi = yi, vi = vi,
  data = d_SMD_SIMPLE
)

# look at overall results
ovr1 %>% summary()
ovr1 %>% metafor::forest(order = "obs", header = TRUE, ilab = country)

# examine moderator (country) through meta-regression
metafor::rma(
  yi = yi, vi = vi,
  mods = ~ country,
  data = d_SMD_SIMPLE
)
# test of moderators is nonsig, Q(2)=0.73, p=.695
# but if it was sig, we would probe, eg:
lapply(
  X = c("A", "B", "C"),
  FUN = function(X) metafor::rma(
    yi = yi, vi = vi,
    data = d_SMD_SIMPLE,
    subset = (country == X)
  )
)

# examine sensitivity/bias
metafor::funnel(ovr1, legend = TRUE)
metafor::ranktest(ovr1)
metafor::regtest(ovr1, model = "lm")
metafor::fsn(ovr1)

##### META-ANALYTIC WORKFLOW (DEMONSTRATION WITH SIMPLE CORRELATION DATA) #####

# obtain overall effect size
# and indicate sample ID as label
# for later plotting purposes
ovr3 = metafor::rma(
  yi = yi, vi = vi,
  data = d_COR_SIMPLE,
  slab = paste0("Sample ", sample_id)
)

# look at overall results
# also convert from Fisher's z to Pearson's r
ovr3 %>% summary()
ovr3$b %>% psych::fisherz2r()
ovr3 %>% metafor::forest(order = "obs", header = TRUE)

# examine sensitivity/bias
metafor::funnel(ovr3, legend = TRUE, label = TRUE)
metafor::ranktest(ovr3)
metafor::regtest(ovr3, model = "lm")
metafor::fsn(ovr3)

##### END OF CODE #####
