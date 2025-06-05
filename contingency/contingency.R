############## START ##############

# R version 4.4.1
library(haven)       # version 2.5.4
library(dplyr)       # version 1.1.4
library(tidyr)       # version 1.3.1
library(psych)       # version 2.4.3
library(ggplot2)     # version 3.5.1
library(ggpubr)      # version XXXXXXXXX
library(ggstatsplot) # version 0.3.1
library(rlang)       # version XXXXXXXXX

# display settings
options(scipen = 9999, digits = 4)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######################### read in and clean data ##################
data_long =
  dplyr::bind_rows(
    # MIDUS CORE 3
    merge(
      # baseline
      haven::read_sav("midus 3/baseline/ICPSR_36346spss/DS0001/36346-0001-Data.sav") %>%
        haven::zap_labels() %>%
        dplyr::select(
          id = M2ID,
          sex = C1PRSEX,
          age = C1PRAGE,
          race = C1PF7A,
          marital_status = C1PB19,
          education = C1PB1,
          ladder_rank = C1SE3,
          household_income = C1STINC
        ),
      # daily
      haven::read_sav("midus 3/midus 3/38529-0001-Data.sav") %>%
        haven::zap_labels() %>%
        dplyr::select(
          id = M2ID,
          day = C2DDAY,
          i_errand = C2DX1A,
          i_medication = C2DX2A,
          i_finish = C2DX3A,
          i_appointment = C2DX4A,
          i_room = C2DX5A,
          i_name = C2DX6A,
          i_something = C2DX7A,
          i_word = C2DX8A,
          i_important = C2DX9A
        ),
      # merge settings
      all = FALSE
    ),
    # MIDUS REFRESHER 1
    merge(
      # baseline
      haven::read_sav("midus refresher/baseline/spss_ICPSR_36532/DS0001/36532-0001-Data.sav") %>%
        haven::zap_labels() %>%
        dplyr::select(
          id = MRID,
          sex = RA1PRSEX,
          age = RA1PRAGE,
          race = RA1PF7A,
          marital_status = RA1PB19,
          education = RA1PB1,
          ladder_rank = RA1SF3,
          household_income = RA1STINC
        ),
      # daily
      haven::read_sav("midus refresher/midus refresher/37083-0001-Data.sav") %>%
        haven::zap_labels() %>%
        dplyr::select(
          id = MRID,
          day = RA2DDAY,
          i_errand = RA2DX1A,
          i_medication = RA2DX2A,
          i_finish = RA2DX3A,
          i_appointment = RA2DX4A,
          i_room = RA2DX5A,
          i_name = RA2DX6A,
          i_something = RA2DX7A,
          i_word = RA2DX8A,
          i_important = RA2DX9A,
        ),
      # merge settings
      all = FALSE
    )) %>%
  # CLEAN COMBINED DATA
  # filter to only rows w/ non-missing
  dplyr::filter(complete.cases(.)) %>%
  # compute cogfail numbers
  dplyr::mutate(
    # recode cogfail
    # original 1=yes, 2=no
    # new 0=no, 1=yes
    across(
      .cols = starts_with("i_"),
      .fns = ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0)
    ),
    # for each person, for that day,
    # count how many cogfail categories they endorsed in one person in one day
    todayCogFailSum = rowSums(across(starts_with("i_"))),
    # convert the prev col into a FALSE (no cogfail) / TRUE (have at least 1)
    # if cogfail categories endorsed in one day > 0 then true, if 0 then false
    # for that person for that day
    isTodayCogFail = todayCogFailSum!=0,
  ) %>%
  # for each person, for the WHOLE study period
  # how many days of cogfail (as % of study days)
  # does each person have
  dplyr::group_by(id) %>%
  dplyr::mutate(
    proportionOfCogFail = mean(isTodayCogFail)
  ) %>%
  dplyr::ungroup()

View(data_long)

# get just L2 data
data_L2 = data_long %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::select(-starts_with("i_"), -day, -contains("today")) %>%
  # convert categorical variables into how we want to split them
  # (1) 0 : no cogfail at all during the week
  # (2) 0 < x < 0.25 : occasional cogfail during the week i.e., low cogfail
  # (3) 0.25 <= x < 0.50 : recurrent cogfail during the week i.e., med cogfail
  # (4) x >= 0.50 : persistent cogfail during the week i.e., high cogfail
  # >= is ≥
  # <= is ≤
  dplyr::mutate(
    cogfailGroup = case_when(
      # (1) 0 : no cogfail at all during the week
      proportionOfCogFail == 0 ~ "None",
      # (2) 0 < x < 0.25 : occasional cogfail during the week
      proportionOfCogFail > 0 & proportionOfCogFail < 0.25 ~ "Low",
      # (3) 0.25 <= x < 0.5: recurrent cogfail during the week
      proportionOfCogFail >= 0.25 & proportionOfCogFail < 0.50 ~ "Med",
      # (4) x >= 0.5 : persistent cogfail during the week
      proportionOfCogFail >= 0.50 ~ "High"
    ),
    cogfailGroup = factor(
      cogfailGroup,
      levels = c("None", "Low", "Med", "High")),
    # age groups
    age_group = dplyr::case_when(
      age < 40 ~ "25-39",
      age < 50 ~ "40-49",
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      TRUE ~ "80-95"
    ),
    # sex
    sex = dplyr::case_when(
      sex == 1 ~ "M",
      sex == 2 ~ "F"
    ),
    # race groups
    race = dplyr::case_when(
      race == 1 ~ "White",
      race != 1 ~ "non-White",
    ),
    #marital status
    marital_status = dplyr::case_when(
      marital_status == 1 ~ "Married",
      marital_status %in% 2:3 ~ "Separated",
      marital_status == 4 ~ "Widowed",
      marital_status == 5 ~ "Never Married",
      TRUE ~ "Others"
    ),
    #education
    education_group = dplyr::case_when(
      education %in% 1:5 ~ "High School",
      education %in% 6:9 ~ "College",
      education %in% 10:12 ~ "Grad School",
    ),
    education_group = factor(
      education_group,
      levels = c("High School", "College", "Grad School")),
    #reverse ladder_rank
    #1=highest, 10=lowest --> reverse to #1=lowest, 10=highest
    ladder_rank = 11 - ladder_rank,
    #income - continuous
    household_income = dplyr::ntile(household_income, 10),
  ) %>%
  as.data.frame()
View(data_L2)

######################### descriptives #########################
data_L2 %>%
  psych::describe()

# age distribution
ggpubr::ggarrange(
  # density
  ggplot(data_L2, aes(x = age)) +
    scale_x_continuous(breaks = seq(20, 100, 10)) +
    geom_vline(xintercept = c(40, 50, 60, 70, 80), colour = "grey") +
    geom_density(),
  # histogram
  ggplot(data_L2, aes(x = age_group)) +
    geom_histogram(stat = "count"),
  # display settings
  ncol = 1
)

# edu distribution
ggpubr::ggarrange(
  # density
  ggplot(data_L2, aes(x = education)) +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    geom_histogram(binwidth = 1),
  # histogram
  ggplot(data_L2, aes(x = education_group)) +
    geom_histogram(stat = "count"),
  # display settings
  ncol = 1
)

######################### subgroup prevalences #########################

getAllOutputs = function(current_variable, simulate.p.value = FALSE) {
  # select columns of interest
  data_current = data_L2 %>%
    dplyr::select(cogfailGroup, all_of(current_variable))
  # create contingency table
  contingency_table = table(data_current)
  # fisher's test
  fishers_test = fisher.test(contingency_table, simulate.p.value = simulate.p.value)
  pval = fishers_test$p.value
  # combine plot and statistical test with ggbarstats
  ggplot = ggstatsplot::ggbarstats(
    # pass in data
    data_current,
    # pass in variables
    cogfailGroup, !!rlang::sym(current_variable),
    # display settings
    results.subtitle = FALSE,
    subtitle = paste0(
      "Fisher's exact test, p-value",
      ifelse(
        pval < 0.001,
        " < 0.001",
        paste0(" = ", round(pval, 3)))
    )
  ) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    ) +
    guides(
      fill = guide_legend(
        title = "Cognitive Failure Rate",
        reverse = FALSE)) +
    labs(
      x = paste(sapply(strsplit(gsub("_", " ", current_variable), " ")[[1]], function(word) {
        paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
      }), collapse = " "),
      y = "Proportion of Subgroup"
    )
  # return all objects
  return(list(
    raw_data = data_current,
    contingency_table = contingency_table,
    fishers_test = fishers_test,
    ggplot = ggplot
  ))
}

#> sex -----
getAllOutputs("sex")

#> race -----
getAllOutputs("race")

#> age -----
getAllOutputs("age_group", simulate.p.value = TRUE)

#> marital -----
getAllOutputs("marital_status", simulate.p.value = TRUE)

#> education -----
getAllOutputs("education_group", simulate.p.value = TRUE)

#> ladder rank -----
getAllOutputs("ladder_rank", simulate.p.value = TRUE)

#> income -----
getAllOutputs("household_income", simulate.p.value = TRUE)

##################### END OF CODE #########################
