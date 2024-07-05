##### START OF CODE #####
# adapted from https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/

# R version 4.4.0
library(dplyr) # version 1.1.4
library(irr)   # version 0.84.1

# read in and clean data
trial = 
  # read
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/interrater/Training%20Set%20(24600%20-%2024755).csv") %>%
  # rater 'E' did not record responses in a clean format (extra whitespaces)
  dplyr::mutate(across(
    .cols = ends_with("_E"),
    .fns = trimws
  )) %>%
  # change character to integer to represent ordinal
  dplyr::mutate(across(
    .cols = contains("_"),
    .fns = ~ case_when(
      .x == "N" ~ 0,
      .x == "UPON REQUEST" ~ 1,
      .x == "Y" ~ 2
    )
  )
  )

# briefly inspect
trial %>% dplyr::glimpse()

# calculate ICC for 6 raters who completed (almost all) records

# criterion: duplicate
# note that ICC is undefined as agreement in this case was 100%
# so there is no variance
trial %>%
  dplyr::select(starts_with("Duplicate")) %>%
  irr::icc(
    model = "twoway", 
    type = "agreement", 
    unit = "single"
  )

# criterion: access
trial %>%
  dplyr::select(starts_with("Access")) %>%
  irr::icc(
    model = "twoway", 
    type = "agreement", 
    unit = "single"
  )

# criterion: valid
trial %>%
  dplyr::select(starts_with("Valid")) %>%
  irr::icc(
    model = "twoway", 
    type = "agreement", 
    unit = "single"
  )

# criterion: data check
trial %>%
  dplyr::select(starts_with("DataCheck")) %>%
  irr::icc(
    model = "twoway", 
    type = "agreement", 
    unit = "single"
  )

##### END OF CODE #####
