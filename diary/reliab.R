##### START OF CODE #####
# version 0 (240621-2030)

# R version 4.4.0
library(dplyr)    # version 1.1.4
library(tidyr)    # version 1.3.1
library(psych)    # version 2.4.3
library(lavaan)   # version 0.6-17
library(semTools) # version 0.5-6
library(lme4)     # version 1.1-35.3

# set working directory to that of script's current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### READ IN DATA #####

dataFull = read.csv("Diss-240621.csv")

# keep just rumination data for now

dataRumi = dataFull %>%
  dplyr::select(PID, DAY, RUM1:RUM3)

##### COMPUTE CRONBACH'S ALPHA #####

#> 1. Treat all data as independent and calculate "overall" reliability -----

dataRumi %>%
  dplyr::select(RUM1:RUM3) %>%
  psych::alpha()

#> 2. Aggregate data to remove dependence and calculate (between-cluster) reliability -----

dataRumi %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(
    RUM1 = mean(RUM1, na.rm = TRUE),
    RUM2 = mean(RUM2, na.rm = TRUE),
    RUM3 = mean(RUM3, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(PID, RUM1, RUM2, RUM3) %>%
  dplyr::select(RUM1:RUM3) %>%
  psych::alpha()

#> 3. Split the data into independent subsets and calculate reliability per subset -----

dataRumi %>%
  split(.$DAY) %>%
  lapply(
    X = .,
    FUN = function(X) X %>%
      dplyr::select(RUM1:RUM3) %>%
      psych::alpha())

#> 4. Use semTools's SEM framework to obtain separate between-cluster and within-cluster reliabilities -----

dataRumi %>%
  lavaan::cfa(
    data = .,
    model = "
    level: 1
    RUM =~ RUM1 + RUM2 + RUM3
    level: 2
    RUM =~ RUM1 + RUM2 + RUM3",
    cluster = "PID") %>%
  semTools::compRelSEM()

#> 5. Use Nezlek's (2017) multilevel approach to obtain separate between-cluster and within-cluster reliabilities -----

temp5 = dataRumi %>%
  tidyr::pivot_longer(
    cols = c(RUM1, RUM2, RUM3)
  ) %>%
  dplyr::rename(ITEM = name, RESP = value) %>%
  lme4::lmer(
    RESP ~ 1 + (1 | PID/DAY),
    data = .
  )

variances = temp5 %>%
  lme4::VarCorr() %>%
  as.data.frame() %>%
  dplyr::filter(is.na(var2)) %>%
  dplyr::mutate(
    variable = grp,
    var = sdcor * sdcor,
    .keep = "none"
  )
n_items = 3
avg_days = lme4::ngrps(temp5)[1] / lme4::ngrps(temp5)[2]
var_item        = variances$var[variances$variable == "Residual"]
var_day         = variances$var[variances$variable == "DAY:PID"]
var_participant = variances$var[variances$variable == "PID"]
alpha_within  = var_day / (var_day + var_item/n_items)
alpha_between = var_participant / (var_participant + var_day/avg_days + var_item/(n_items*avg_days))
alphas = c(alpha_within, alpha_between); names(alphas) = c("w", "b"); rm(alpha_within); rm(alpha_between)

##### END OF CODE #####
