##### START OF CODE #####
# version 0.1 (240822)

# R version 4.4.0
library(dplyr)       # version 1.1.4
library(lme4)        # version 1.1-35.5
library(nlme)        # version 3.1-166
library(lavaan)      # version 0.6-18
library(effectsize)  # version 0.8.9
library(broom.mixed) # version 0.2.9.5
library(ggplot2)     # version 3.5.1

##### READ IN DATA #####

dataFull = 
  # read in raw data in long format
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/diary/ME/Diss-240708.csv") %>%
  # filter to remove days with missing data
  dplyr::filter(complete.cases(.)) %>%
  # create mean scores for each construct
  dplyr::mutate(
    LON = rowMeans(across(starts_with("LON"))),
    ANX = rowMeans(across(starts_with("ANX"))),
    RUM = rowMeans(across(starts_with("RUM")))
  ) %>%
  # throw out item-level data; not needed for this purpose
  dplyr::select(PID, DAY, LON, ANX, RUM)

##### DO LME4, NLME, AND LAVAAN GIVE THE SAME RESULTS FOR A SIMPLE MULTILEVEL MODEL? #####

#> THROUGH LME4 ----

results_lme4_temp_unstd = lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = dataFull %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup(),
  REML = FALSE
) 

results_lme4_temp_std = results_lme4_temp_unstd %>% 
  effectsize::standardize_parameters(method = "pseudo") %>%
  as.data.frame() %>%
  dplyr::rename(
    term = Parameter,
    std.estimate = Std_Coefficient,
    std.cilo = CI_low,
    std.cihi = CI_high) %>%
  dplyr::select(-CI)

results_lme4_clean = merge(
  results_lme4_temp_unstd %>% broom.mixed::tidy(), 
  results_lme4_temp_std, 
  all = TRUE)

#> THROUGH NLME ----

results_nlme_temp_unstd = nlme::lme(
  fixed = LON ~ 1 + ANXb + ANXw + RUMb + RUMw,
  random = ~ 1 | PID,
  data = dataFull %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(
      ANXb = mean(ANX),
      ANXw = ANX - ANXb,
      RUMb = mean(RUM),
      RUMw = RUM - RUMb
    ) %>%
    dplyr::ungroup(),
  method = "ML"
) 

# note to self: how to get std output?

results_nlme_clean = 
  results_nlme_temp_unstd %>% broom.mixed::tidy()

#> THROUGH LAVAAN ----

results_lavaan_temp = lavaan::sem(
  "
  level: 1
  LON ~ ANX + RUM
  level: 2
  LON ~ ANX + RUM
  ",
  cluster = "PID",
  data = dataFull %>%
    dplyr::mutate(
      LON = rowMeans(across(starts_with("LON"))),
      ANX = rowMeans(across(starts_with("ANX"))),
      RUM = rowMeans(across(starts_with("RUM")))
    )
) 

results_lavaan_clean = 
  results_lavaan_temp %>% broom.mixed::tidy()

#> COMPARISON ----

all = dplyr::bind_rows(
  results_lme4_clean %>% 
    dplyr::filter(term %in% c("ANXb", "ANXw", "RUMb", "RUMw")) %>%
    dplyr::select(term, est = estimate, se = std.error, stdest = std.estimate) %>%
    dplyr::mutate(source = "lme4 (std by effectsize)") %>%
    dplyr::arrange(term),
  results_nlme_clean %>%
    dplyr::filter(term %in% c("ANXb", "ANXw", "RUMb", "RUMw")) %>%
    dplyr::select(term, est = estimate, se = std.error) %>%
    dplyr::mutate(source = "nlme") %>%
    dplyr::arrange(term) %>%
    as.data.frame(),
  results_lavaan_clean %>%
    dplyr::filter(term %in% c("LON ~ ANX", "LON ~ RUM")) %>%
    dplyr::mutate(term = case_when(
      term == "LON ~ ANX" & level == 1 ~ "ANXw",
      term == "LON ~ ANX" & level == 2 ~ "ANXb",
      term == "LON ~ RUM" & level == 1 ~ "RUMw",
      term == "LON ~ RUM" & level == 2 ~ "RUMb"
    )) %>%
    dplyr::select(term, est = estimate, se = std.error, stdest = std.all) %>%
    dplyr::mutate(source = "lavaan") %>%
    dplyr::arrange(term) %>%
    as.data.frame()
) %>%
  dplyr::arrange(term)

all %>% print()

#> VISUALISATION ----

ggplot(
  dplyr::bind_rows(
    all %>% dplyr::mutate(value = est, type = "Estimate (unstandardised)"), 
    all %>% dplyr::mutate(value = se, type = "Standard error"), 
    all %>% dplyr::mutate(value = stdest, type = "Estimate (standardised)")
  ),
  aes(x = value, y = source)) +
  geom_point() +
  facet_wrap(~ term + type, nrow = 4) +
  theme_bw()

##### LOOK AT MANUALLY STANDARDISING? #####

# prepare data

dataL2 = 
  dataFull %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(
    LONb = mean(LON),
    ANXb = mean(ANX), 
    RUMb = mean(RUM)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(PID, .keep_all = TRUE) %>%
  dplyr::mutate(
    LONb.z = as.numeric(scale(LONb)),
    ANXb.z = as.numeric(scale(ANXb)),
    RUMb.z = as.numeric(scale(RUMb))) %>%
  dplyr::select(PID, LONb, LONb.z, ANXb, ANXb.z, RUMb, RUMb.z)

dataFullStd = 
  merge(dataFull, dataL2) %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(
    LONw = LON - LONb,
    LONw.z = as.numeric(scale(LONw)),
    ANXw = ANX - ANXb,
    ANXw.z = as.numeric(scale(ANXw)),
    RUMw = RUM - RUMb,
    RUMw.z = as.numeric(scale(RUMw))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    across(
      .cols = c(LONw.z, ANXw.z, RUMw.z),
      .fns = ~ replace(., is.nan(.), 0)),
    LON.z = LONb.z + LONw.z)

# run analyses

lme4::lmer(
  LON.z ~ 0 + ANXb.z + ANXw.z + RUMb.z + RUMw.z + (1 | PID),
  data = dataFullStd,
  REML = FALSE
) %>% broom.mixed::tidy() %>%
  as.data.frame() %>%
  dplyr::select(term, estimate, std.error) %>%
  dplyr::filter(grepl(".z", term)) 

nlme::lme(
  fixed = LON.z ~ 0 + ANXb.z + ANXw.z + RUMb.z + RUMw.z,
  random = ~ 1 | PID,
  data = dataFullStd,
  method = "ML"
) %>% broom.mixed::tidy() %>%
  as.data.frame() %>%
  dplyr::select(term, estimate, std.error) %>%
  dplyr::filter(grepl(".z", term)) 

lavaan::sem(
  "
  level: 1
  LON.z ~ ANXw.z + RUMw.z
  level: 2
  LON.z ~ ANXb.z + RUMb.z
  ",
  cluster = "PID",
  data = dataFullStd
) %>% broom.mixed::tidy() %>%
  dplyr::filter(op == "~") %>%
  as.data.frame() %>%
  dplyr::select(term, estimate, std.error, std.lv, std.all)

##### END OF CODE #####
