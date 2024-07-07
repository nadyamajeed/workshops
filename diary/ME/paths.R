##### START OF CODE #####
# version 0 (240708)

# R version 4.4.0
library(dplyr)  # version 1.1.4
library(tidyr)  # version 1.3.1
library(lme4)   # version 1.1-35.4 # UPDATE THIS
library(mirt)   # version 1.41
library(lavaan) # version 0.6-17

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

lme4::lmer(
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
) %>% summary(correlation = FALSE)

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

lme4::lmer(
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
) %>% summary(correlation = FALSE)

##### 2SPA #####

# to be added

##### SAM-L #####

lavaan::sam(
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
) %>% summary()

##### SAM-G #####

lavaan::sam(
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
) %>% summary()

##### FULL SEM #####

lavaan::sem(
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
) %>% summary()

##### END OF CODE #####
