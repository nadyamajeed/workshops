##### START OF CODE #####
# version 2.0 (241017)

# R version 4.4.1
library(dplyr)       # version 1.1.4
library(tidyr)       # version 1.3.1
library(lme4)        # version 1.1-35.5
library(lavaan)      # version 0.6-19
library(broom.mixed) # version 0.2.9.6

##### CUSTOM HELPER FUNCTIONS #####

mlm_unstd_and_std = function(lmer_output) {
  unstd = lmer_output %>% 
    broom.mixed::tidy()
  std = lmer_output %>% 
    effectsize::standardize_parameters(method = "pseudo") %>%
    as.data.frame() %>%
    dplyr::rename(
      term = Parameter,
      std.estimate = Std_Coefficient,
      std.cilo = CI_low,
      std.cihi = CI_high) %>%
    dplyr::select(-CI)
  return(merge(unstd, std, all = TRUE))
}

##### READ IN DATA #####

dataFull = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/diary/ME/Diss-240708.csv") %>%
  dplyr::filter(complete.cases(.))

# count N

dataFull %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(
    ndays = n(),
    all7 = ifelse(ndays == 7, 100, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(PID, ndays, all7) %>%
  psych::describe()

##### TRADITIONAL MLM #####

appr1_nomis = lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = dataFull %>%
    dplyr::mutate(
      LON = rowMeans(across(starts_with("LON"))),
      ANX = rowMeans(across(starts_with("ANX"))),
      RUM = rowMeans(across(starts_with("RUM")))
    ) %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup()
) %>% mlm_unstd_and_std()

##### TWO-STAGE MLM WITH MEASUREMENT EXTENSION (BASED OFF NEZLEK) #####

temp1 = lme4::lmer(
  value ~ 1 + (1 | PID/DAY),
  data = dataFull %>%
    dplyr::select(PID, DAY, starts_with("LON")) %>%
    tidyr::pivot_longer(starts_with("LON")) %>%
    dplyr::rename(item = name)
) %>% coef(); temp1 = temp1$`DAY:PID`; temp1 = temp1 %>%
  dplyr::mutate(
    temp = rownames(.),
    PID = substr(temp, nchar(temp) - 3, nchar(temp)),
    DAY = substr(temp, 1, 1),
    temp = NULL) %>%
  dplyr::rename(LON = `(Intercept)`)

temp2 = lme4::lmer(
  value ~ 1 + (1 | PID/DAY),
  data = dataFull %>%
    dplyr::select(PID, DAY, starts_with("ANX")) %>%
    tidyr::pivot_longer(starts_with("ANX")) %>%
    dplyr::rename(item = name)
) %>% coef(); temp2 = temp2$`DAY:PID`; temp2 = temp2 %>%
  dplyr::mutate(
    temp = rownames(.),
    PID = substr(temp, nchar(temp) - 3, nchar(temp)),
    DAY = substr(temp, 1, 1),
    temp = NULL) %>%
  dplyr::rename(ANX = `(Intercept)`)

temp3 = lme4::lmer(
  value ~ 1 + (1 | PID/DAY),
  data = dataFull %>%
    dplyr::select(PID, DAY, starts_with("RUM")) %>%
    tidyr::pivot_longer(starts_with("RUM")) %>%
    dplyr::rename(item = name)
) %>% coef(); temp3 = temp3$`DAY:PID`; temp3 = temp3 %>%
  dplyr::mutate(
    temp = rownames(.),
    PID = substr(temp, nchar(temp) - 3, nchar(temp)),
    DAY = substr(temp, 1, 1),
    temp = NULL) %>%
  dplyr::rename(RUM = `(Intercept)`)

appr2_nomis = lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = merge(merge(temp1, temp2), temp3) %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup()
) %>% mlm_unstd_and_std()

##### SAM-L #####

appr3_nomis = lavaan::sam(
  "
  level: 1
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + RUM2 + RUM3
  LON ~ ANX + RUM
  level: 2
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + RUM2 + RUM3
  LON ~ ANX + RUM
  ",
  mm.list = list(LON = "LON", ANX = "ANX", RUM = "RUM"),
  sam.method = "local",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy()

##### SAM-G #####

appr4_nomis = lavaan::sam(
  "
  level: 1
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + RUM2 + RUM3
  LON ~ ANX + RUM
  level: 2
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + RUM2 + RUM3
  LON ~ ANX + RUM
  ",
  mm.list = list(LON = "LON", ANX = "ANX", RUM = "RUM"),
  sam.method = "global",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy()

##### FULL SEM #####

appr5_nomis = lavaan::sem(
  "
  level: 1
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + RUM2 + RUM3
  LON ~ ANX + RUM
  level: 2
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + RUM2 + RUM3
  LON ~ ANX + RUM
  ",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy()

##### END OF CODE #####

##### DUMP FOR MISSPECIFICATION #####

# RUM misspecified?

appr1_mis = lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = dataFull %>%
    dplyr::mutate(
      LON = rowMeans(across(starts_with("LON"))),
      ANX = rowMeans(across(starts_with("ANX"))),
      RUM = rowMeans(across(c(RUM1, ENV1, RUM3)))
    ) %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup()
) %>% mlm_unstd_and_std()

# RUM misspecified?

temp3 = lme4::lmer(
  value ~ 1 + (1 | PID/DAY),
  data = dataFull %>%
    dplyr::select(PID, DAY, RUM1, ENV1, RUM3) %>%
    tidyr::pivot_longer(c(RUM1, ENV1, RUM3)) %>%
    dplyr::rename(item = name)
) %>% coef(); temp3 = temp3$`DAY:PID`; temp3 = temp3 %>%
  dplyr::mutate(
    temp = rownames(.),
    PID = substr(temp, nchar(temp) - 3, nchar(temp)),
    DAY = substr(temp, 1, 1),
    temp = NULL) %>%
  dplyr::rename(RUM = `(Intercept)`)

appr2_mis = lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = merge(merge(temp1, temp2), temp3) %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup()
) %>% broom.mixed::tidy()

# RUM misspecified?

appr3_mis = lavaan::sam(
  "
  level: 1
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + ENV1 + RUM3
  LON ~ ANX + RUM
  level: 2
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + ENV1 + RUM3
  LON ~ ANX + RUM
  ",
  mm.list = list(LON = "LON", ANX = "ANX", RUM = "RUM"),
  sam.method = "local",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy()

# RUM misspecified?

appr4_mis = lavaan::sam(
  "
  level: 1
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + ENV1 + RUM3
  LON ~ ANX + RUM
  level: 2
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + ENV1 + RUM3
  LON ~ ANX + RUM
  ",
  mm.list = list(LON = "LON", ANX = "ANX", RUM = "RUM"),
  sam.method = "global",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy()

# RUM misspecified?

appr5_mis = lavaan::sem(
  "
  level: 1
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + ENV1 + RUM3
  LON ~ ANX + RUM
  level: 2
  LON =~ LON1 + LON2 + LON3 + LON4 + LON5 + LON6
  ANX =~ ANX1 + ANX2 + ANX3
  RUM =~ RUM1 + ENV1 + RUM3
  LON ~ ANX + RUM
  ",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy()

##### END OF DUMP #####
