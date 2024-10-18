##### START OF CODE #####
# version 1.0 (241018)

# R version 4.4.1
library(dplyr)       # version 1.1.4
library(tidyr)       # version 1.3.1
library(lme4)        # version 1.1-35.5
library(lavaan)      # version 0.6-19
library(broom.mixed) # version 0.2.9.6

##### READ IN DATA #####

dataFull = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/diary/ME/Diss-240708.csv") %>%
  dplyr::filter(complete.cases(.)) %>%
  # score
  dplyr::mutate(
    LON = rowMeans(across(starts_with("LON"))),
    ANX = rowMeans(across(starts_with("ANX"))),
    RUM = rowMeans(across(starts_with("RUM")))
  ) %>%
  # CWC
  dplyr::group_by(PID) %>%
  dplyr::mutate(
    ANXb = mean(ANX),
    ANXw = ANX - ANXb,
    RUMb = mean(RUM),
    RUMw = RUM - RUMb,
    LONb = mean(LON),
    LONw = LON - LONb
  ) %>%
  dplyr::ungroup()

##### TRY #####

# through effectsize pseudo
lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = dataFull 
) %>% effectsize::standardize_parameters(method = "pseudo") 

# through CWC then separate STD at L2 and L1
lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = merge(
    dataFull %>% 
      dplyr::distinct(PID, ANXb, RUMb, LONb, .keep_all = FALSE) %>%
      dplyr::mutate(across(c(ANXb, RUMb, LONb), ~as.numeric(scale(.x)))),
    dataFull %>% 
      dplyr::select(PID, DAY, ANXw, RUMw, LONw) %>%
      dplyr::group_by(PID) %>%
      dplyr::mutate(
        across(c(ANXw, RUMw, LONw), ~as.numeric(scale(.x))),
        across(c(ANXw, RUMw, LONw), ~replace(.x, is.nan(.x), 0))) %>%
      dplyr::ungroup(),
    by = "PID"
  ) %>% dplyr::mutate(LON = LONb + LONw)
) %>% broom.mixed::tidy() %>%
  dplyr::filter(effect == "fixed")

# through raw STD then CWC
lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = dataFull %>%
    dplyr::mutate(across(c(LON, ANX, RUM), ~as.numeric(scale(.x)))) %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup()
) %>% broom.mixed::tidy() %>%
  dplyr::filter(effect == "fixed")

##### END OF CODE #####
