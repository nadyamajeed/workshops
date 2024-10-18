##### START OF CODE #####
# version 1.1 (241018)

# R version 4.4.1
library(dplyr)       # version 1.1.4
library(tidyr)       # version 1.3.1
library(lme4)        # version 1.1-35.5
library(lavaan)      # version 0.6-19
library(broom.mixed) # version 0.2.9.6
library(ggplot2)

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

##### TRY LME4 #####

# through effectsize pseudo
a = lme4::lmer(
  LON ~ 1 + ANXb + ANXw + RUMb + RUMw + (1 | PID),
  data = dataFull 
) %>% effectsize::standardize_parameters(method = "pseudo") 

# through CWC then separate STD at L2 and L1
b = lme4::lmer(
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
c = lme4::lmer(
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

##### TRY LAVAAN #####

# normal
d = lavaan::sem(
  "level: 1
  LON ~ ANX + RUM
  level: 2
  LON ~ ANX + RUM",
  cluster = "PID",
  data = dataFull
) %>% broom.mixed::tidy() %>%
  dplyr::filter(term %in% c("LON ~ ANX", "LON ~ RUM")) %>%
  dplyr::mutate(term = case_when(
    term == "LON ~ ANX" & level == 1 ~ "ANXw",
    term == "LON ~ ANX" & level == 2 ~ "ANXb",
    term == "LON ~ RUM" & level == 1 ~ "RUMw",
    term == "LON ~ RUM" & level == 2 ~ "RUMb"
  )) %>%
  dplyr::select(term, estimate, std.error, std.lv, std.all)

##### COMPARISON #####

all = dplyr::bind_rows(
  a %>% 
    as.data.frame() %>%
    dplyr::filter(Parameter %in% c("ANXb", "ANXw", "RUMb", "RUMw")) %>%
    dplyr::select(term = Parameter, est = Std_Coefficient) %>%
    dplyr::mutate(source = "lme4 > effectsize > pseudo") %>%
    dplyr::arrange(term),
  b %>%
    dplyr::filter(term %in% c("ANXb", "ANXw", "RUMb", "RUMw")) %>%
    dplyr::select(term, est = estimate, se = std.error) %>%
    dplyr::mutate(source = "lme4 > manual > CWC then STD") %>%
    dplyr::arrange(term),
  c %>%
    dplyr::filter(term %in% c("ANXb", "ANXw", "RUMb", "RUMw")) %>%
    dplyr::select(term, est = estimate, se = std.error) %>%
    dplyr::mutate(source = "lme4 > manual > STD then CWC") %>%
    dplyr::arrange(term),
  d %>%
    dplyr::select(term, est = estimate, se = std.error) %>%
    dplyr::mutate(source = "lavaan est") %>%
    dplyr::arrange(term),
  d %>%
    dplyr::select(term, est = std.lv) %>%
    dplyr::mutate(source = "lavaan std.lv") %>%
    dplyr::arrange(term),
  d %>%
    dplyr::select(term, est = std.all) %>%
    dplyr::mutate(source = "lavaan std.all") %>%
    dplyr::arrange(term)
) %>%
  dplyr::arrange(term)

ggplot(
  dplyr::bind_rows(
    all %>% dplyr::mutate(value = est, type = "Estimate ('standardised')"), 
    all %>% dplyr::mutate(value = se, type = "Standard error")
  ),
  aes(x = value, y = source)) +
  geom_point() +
  facet_wrap(~ term + type, nrow = 4) +
  theme_bw()

##### END OF CODE #####
