# NADYANNA M. MAJEED (https://github.com/nadyamajeed/workshops)
# VERSION: 22/06/22 3:13PM



########## set up ##########

# load libraries
# indicate versions for reproducibility
library(dplyr)      # version 1.0.8
library(psych)      # version 2.2.5
library(lavaan)     # version 0.6-11
library(lavaanPlot) # version 0.6.2

# disable scientific notation
options(scipen = 9999)



########## preparation ##########

# read in data
data_sem = read.csv("https://github.com/nadyamajeed/workshops/raw/main/sem/sem.csv")

# briefly inspect the data
data_sem %>% dplyr::glimpse()

# get descriptives
data_sem %>% psych::describe()



########## confirm measurement model (CFA) ##########

# set up formulae
ef.formula = '
  ic =~ ef_ic_af + ef_ic_cf + ef_ic_ef
  ts =~ ef_ts_cs + ef_ts_mp + ef_ts_al
  wm =~ ef_wm_os + ef_wm_rs + ef_wm_ss

  ic ~~ ts + wm
  ts ~~ wm
'

# run CFA
ef.results =
  ef.formula %>%
  lavaan::cfa(
    data = data_sem,
    missing = "fiml",
    mimic = "Mplus")

# see results
ef.results %>%
  summary(
    standardized = TRUE,
    fit.measures = TRUE)

# show plot
ef.results %>%
  lavaanPlot::lavaanPlot(
    model = .,
    node_options = list(shape = "box", fontname = "Helvetica"),
    edge_options = list(color = "grey"),
    coefs = TRUE, covs = TRUE, stand = TRUE, stars = TRUE)



########## test structural model (SEM) ##########

##> UNADJUSTED MODEL -----

# set up formulae
model1.formula = '
  ic ~ OMSI_COMPUTED
  ts ~ OMSI_COMPUTED
  wm ~ OMSI_COMPUTED
  ' %>% paste(ef.formula)

# run SEM
model1.results =
  model1.formula %>%
  lavaan::sem(
    data = data_sem,
    missing = "fiml",
    mimic = "Mplus")

# see results
model1.results %>%
  summary(
    standardized = TRUE,
    fit.measures = TRUE)

# show plot
model1.results %>%
  lavaanPlot::lavaanPlot(
    model = .,
    node_options = list(shape = "box", fontname = "Helvetica"),
    edge_options = list(color = "grey"),
    coefs = TRUE, covs = TRUE, stand = TRUE, stars = TRUE)

##> ADJUSTED MODEL -----

# set up formulae
model2.formula = '
  ic ~ OMSI_COMPUTED + age + female + income + ladder
  ts ~ OMSI_COMPUTED + age + female + income + ladder
  wm ~ OMSI_COMPUTED + age + female + income + ladder
  ' %>% paste(ef.formula)

# run SEM
model2.results =
  model2.formula %>%
  lavaan::sem(
    data = data_sem,
    missing = "fiml",
    mimic = "Mplus")

# see results
model2.results %>%
  summary(
    standardized = TRUE,
    fit.measures = TRUE)

# show plot
model2.results %>%
  lavaanPlot::lavaanPlot(
    model = .,
    node_options = list(shape = "box", fontname = "Helvetica"),
    edge_options = list(color = "grey"),
    coefs = TRUE, covs = TRUE, stand = TRUE, stars = TRUE)



########## end of code ##########
