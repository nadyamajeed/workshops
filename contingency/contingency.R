############## START ##############

# R version 4.4.1
library(haven)    # version 2.5.4
library(dplyr)    # version 1.1.4
library(tidyr)    # version 1.3.1
library(psych)    # version 2.4.3
library(ggplot2)  # version 3.5.1
library(ggstatsplot) # version 0.3.1

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
          ladder = C1SE3,
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
          ladder = RA1SF3,
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
  # how many days of cogfail does each person have
  dplyr::group_by(id) %>%
    dplyr::mutate(
      #numCogFailDays = sum(isTodayCogFail)
      proportionOfCogFail = mean (isTodayCogFail)
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
    education = dplyr::case_when(
      education %in% 1:5 ~ "High School",
      education %in% 6:9 ~ "Bachelor",
      education %in% 10:12 ~ "Grad School",
    ),
    #reverse ladder
    #1=highest, 10=lowest --> reverse to #1=lowest, 10=highest
    ladder = 11 - ladder,
    #income - continuous
    household_income = dplyr::ntile(household_income, 10), 
  ) %>%
  as.data.frame() 
View(data_L2)

######################### descriptives #########################
data_L2 %>%
  psych::describe()

ggplot(data_L2, aes(x = age)) +
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  geom_vline(xintercept = c(40, 50, 60, 70, 80), colour = "grey") +
  geom_density() 

######################### subgroup prevalences #########################
##################### SEX #########################
# select columns of interest & create a contingency table
contingency_table_sex = table(data_L2$sex, data_L2$cogfailGroup) %>% t()
print(contingency_table_sex)

# create mosaic plot
mosaicplot(contingency_table_sex,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_sex = fisher.test(contingency_table_sex)
test_sex
test_sex$p.value

# contingency table
#fisher.test(table(df_sex$M, df_sex$F))

# create dataframe from contingency table
# row = "None", "Low", "Med", "High"
# col = male, female
sex <- c()
for (row in rownames(contingency_table_sex)) (
  for (col in colnames(contingency_table_sex)) (
    sex <- rbind(sex, matrix(rep(c(row, col), contingency_table_sex[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_sex_2 <- as.data.frame(sex)
colnames(df_sex_2) = c("CogFail_Days", "Sex")
df_sex_2

# fisher's exact test with raw data
test_sex_2 <- fisher.test(table(df_sex_2))

#assign factor and level 
df_sex_2 = df_sex_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_sex_2 = df_sex_2 %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

#check factor and level 
levels(df_sex_2$CogFail_Days)
levels(df_sex_2$Sex)
is.factor(df_sex_2$CogFail_Days)  
is.factor(df_sex_2$Sex)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_sex_2, CogFail_Days, Sex,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_sex_2$p.value < 0.001, "< 0.001", round(test_sex_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Sex",
    y = "Proportion"
  )

##################### RACE #########################
# select columns of interest & create a contingency table
contingency_table_race = table(data_L2$race, data_L2$cogfailGroup) %>% t()
print(contingency_table_race)

# create mosaic plot
mosaicplot(df_race,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_race = fisher.test(df_race)
test_race$p.value

# contingency table
fisher.test(table(df_race$`non-White`, df_race$White))

# create dataframe from contingency table
# row = "None", "Low", "Med", "High"
# col = "non-white", "white"
race <- c()
for (row in rownames(df_race)) (
  for (col in colnames(df_race)) (
    race <- rbind(race, matrix(rep(c(row, col), df_race[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_race_2 <- as.data.frame(race)
colnames(df_race_2) <- c("CogFail_Days", "Race")
df_race_2

# fisher's exact test with raw data
test_race_2 <- fisher.test(table(df_race_2))

#assign factor and level 
df_race_2 = df_race_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_race_2 = df_race_2 %>%
  mutate(Race = factor(Race, levels = c("non-White", "White")))

#check factor and level 
levels(df_race_2$CogFail_Days)
levels(df_race_2$Race)
is.factor(df_race_2$CogFail_Days)
is.factor(df_race_2$Race)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_race_2, CogFail_Days, Race,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_race_2$p.value < 0.001, "< 0.001", round(test_race_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Race",
    y = "Proportion"
  )

##################### AGE #########################
# select columns of interest & create a contingency table
# 1 - "25-39", 2 - "40-49", 3 - "50-59", 4 - "60-69", 5 - "70-79", 6 - "80-95"
contingency_table_age = table(data_L2$age_group, data_L2$cogfailGroup)
print(contingency_table_age)

# create a dataframe
df_age <- data.frame(
  "25-40" = c(25, 29, 65, 90),
  "41-50" = c(38, 53, 81, 136),
  "51-60" = c(75, 70, 135, 196),
  "61-70" = c(72, 71, 106, 181),
  "71-80" = c(53, 32, 63, 98),
  "81-95" = c(8, 2, 15, 36),
  row.names = c("None", "Low", "Med", "High"),
  stringsAsFactors = FALSE
)

colnames(df_age) <- c("25-40", "41-50", "51-60", "61-70", "71-80", "81-95")
df_age

# create mosaic plot
mosaicplot(df_age,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_age = fisher.test(df_age, simulate.p.value=TRUE)

# create dataframe from contingency table
# row = "None", "Low", "Med", "High"
# col = "25-40", "41-50", "51-60", "61-70", "71-80", "81-95"
age <- c()
for (row in rownames(df_age)) (
  for (col in colnames(df_age)) (
    age <- rbind(age, matrix(rep(c(row, col), df_age[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_age_2 <- as.data.frame(age)
colnames(df_age_2) <- c("CogFail_Days", "Age")
df_age_2

# fisher's exact test with raw data
test_age_2 = fisher.test(table(df_age_2), simulate.p.value = TRUE)

#assign factor and level 
df_age_2 = df_age_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_age_2 = df_age_2 %>%
  mutate(Age = factor(Age, levels = c("25-40", "41-50", "51-60", "61-70", "71-80", "81-95")))

#check factor and level 
levels(df_age_2$CogFail_Days)
levels(df_age_2$Age)
is.factor(df_age_2$CogFail_Days)
is.factor(df_age_2$Age)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_age_2, CogFail_Days, Age,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_age_2$p.value < 0.001, "< 0.001", round(test_age_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Age",
    y = "Proportion"
  )

##################### MARITAL STATUS #########################
# 1 = Married, 2-3 = "Separated", 4 = "Widowed", 5 = "Never Married"
# select columns of interest & create a contingency table
contingency_table_marital = table(data_L2$marital_status, data_L2$cogfailGroup)
print(contingency_table_marital)

# create a dataframe
df_marital <- data.frame(
  "Married" = c(182, 163, 323, 491),
  "Separated" = c(38, 42, 69, 106),
  "Widowed" = c(23, 13, 33, 63),
  "Never Married" = c(28, 39, 40, 77),
  row.names = c("None", "Low", "Med", "High"),
  stringsAsFactors = FALSE
)

colnames(df_marital) <- c("Married", "Separated", "Widowed", "Never Married")
df_marital

# create mosaic plot
mosaicplot(df_marital,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_marital = fisher.test(df_marital, simulate.p.value=TRUE)

# create dataframe from contingency table
# row = "None", "Low", "Med", "High"
# col = "Married", "Separated", "Widowed", "Never Married"
marital <- c()
for (row in rownames(df_marital)) (
  for (col in colnames(df_marital)) (
    marital <- rbind(marital, matrix(rep(c(row, col), df_marital[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_marital_2 <- as.data.frame(marital)
colnames(df_marital_2) <- c("CogFail_Days", "Marital_Status")
df_marital_2

# fisher's exact test with raw data
test_marital_2 = fisher.test(table(df_marital_2), simulate.p.value = TRUE)

#assign factor and level 
df_marital_2 = df_marital_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_marital_2 = df_marital_2 %>%
  mutate(Marital_Status = factor(Marital_Status, levels = c("Married", "Separated", "Widowed", "Never Married")))

#check factor and level 
levels(df_marital_2$CogFail_Days)
levels(df_marital_2$Marital_Status)
is.factor(df_marital_2$CogFail_Days)
is.factor(df_marital_2$Marital_Status)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_marital_2, CogFail_Days, Marital_Status,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_marital_2$p.value < 0.001, "< 0.001", round(test_marital_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Marital Status",
    y = "Proportion"
  )

##################### EDUCATION #########################
# select columns of interest & create a contingency table
contingency_table_education = table(data_L2$education, data_L2$cogfailGroup)
print(contingency_table_education)

# create a dataframe
df_education <- data.frame(
  "High School" = c(86, 50, 88, 141),
  "Bachelor" = c(140, 150, 265, 398),
  "Grad School" = c(45, 57, 112, 198),
  row.names = c("None", "Low", "Med", "High"),
  stringsAsFactors = FALSE
)

colnames(df_education) <- c("High School", "Bachelor", "Grad School")
df_education

# create mosaic plot
mosaicplot(df_education,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_education = fisher.test(df_education, simulate.p.value=TRUE)

# create dataframe from contingency table
# row = "None", "Low", "Med", "High"
# col = "High School", "Bachelor", "Grad School"
education <- c()
for (row in rownames(df_education)) (
  for (col in colnames(df_education)) (
    education <- rbind(education, matrix(rep(c(row, col), df_education[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_education_2 <- as.data.frame(education)
colnames(df_education_2) <- c("CogFail_Days", "Education")
df_education_2

# fisher's exact test with raw data
test_education_2 = fisher.test(table(df_education_2), simulate.p.value = TRUE)

#assign factor and level 
df_education_2 = df_education_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_education_2 = df_education_2 %>%
  mutate(Education = factor(Education, levels = c("High School", "Bachelor", "Grad School")))

#check factor and level 
levels(df_education_2$CogFail_Days)
levels(df_education_2$Education)
is.factor(df_education_2$CogFail_Days)
is.factor(df_education_2$Education)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_education_2, CogFail_Days, Education,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_education_2$p.value < 0.001, "< 0.001", round(test_education_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Education",
    y = "Proportion"
  )


##################### LADDER ########################
# from 1 to 10; 1 lowest and 10 highest 
# select columns of interest & create a contingency table
contingency_table_ladder = table(data_L2$ladder, data_L2$cogfailGroup)
print(contingency_table_ladder)

# create a dataframe
df_ladder <- data.frame(
  "1" = c(4, 3, 5, 12),
  "2" = c(4, 6, 10, 15),
  "3" = c(14, 10, 16, 36),
  "4" = c(19, 17, 26, 58),
  "5" = c(45, 20, 58, 78),
  "6" = c(52, 54, 84, 140),
  "7" = c(57, 67, 112, 170),
  "8" = c(48, 53, 118, 170),
  "9" = c(19, 20, 29, 46),
  "10" = c(9, 7, 7, 12),
  row.names = c("None", "Low", "Med", "High"),
  stringsAsFactors = FALSE
)

colnames(df_ladder) <- c("1", "2", "3", "4", "5","6", "7", "8", "9", "10")
df_ladder

# create mosaic plot
mosaicplot(df_ladder,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_ladder = fisher.test(df_ladder, simulate.p.value=TRUE)

# create dataframe from contingency table
# row = "None", "Low", "Med", "High"
# col = "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
ladder <- c()
for (row in rownames(df_ladder)) (
  for (col in colnames(df_ladder)) (
    ladder <- rbind(ladder, matrix(rep(c(row, col), df_ladder[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_ladder_2 <- as.data.frame(ladder)
colnames(df_ladder_2) <- c("CogFail_Days", "Ladder")
df_ladder_2

# Fisher's exact test with raw data
test_ladder_2 = fisher.test(table(df_ladder_2), simulate.p.value = TRUE)

#assign factor and level 
df_ladder_2 = df_ladder_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_ladder_2 = df_ladder_2 %>%
  mutate(Ladder = factor(Ladder, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))

#check factor and level 
levels(df_ladder_2$CogFail_Days)
levels(df_ladder_2$Ladder)
is.factor(df_ladder_2$CogFail_Days)
is.factor(df_ladder_2$Ladder)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_ladder_2, CogFail_Days, Ladder,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_ladder_2$p.value < 0.001, "< 0.001", round(test_ladder_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Ladder",
    y = "Proportion"
  )

##################### INCOME #########################
# from 1 to 10; 1 lowest and 10 highest 
# select columns of interest & create a contingency table
contingency_table_income = table(data_L2$household_income, data_L2$cogfailGroup)
print(contingency_table_income)

# create a dataframe
df_income <- data.frame(
  "1" = c(38, 26, 41, 68),
  "2" = c(29, 29, 37, 78),
  "3" = c(37, 23, 46, 67),
  "4" = c(25, 31, 51, 66),
  "5" = c(27, 24, 61, 61),
  "6" = c(32, 24, 47, 70),
  "7" = c(24, 27, 44, 78),
  "8" = c(21, 20, 48, 84),
  "9" = c(25, 24, 46, 78),
  "10" = c(13, 29, 44, 87),
  row.names = c("None", "Low", "Med", "High"),
  stringsAsFactors = FALSE
)

colnames(df_income) <- c("1", "2", "3", "4", "5","6", "7", "8", "9", "10")

df_income

# create mosaic plot
mosaicplot(df_income,
           main = "Mosaic plot",
           color = TRUE
)

# fisher's test
test_income = fisher.test(df_income, simulate.p.value=TRUE)

# create dataframe from contingency table
# row = "None", "Low", "Med", "High
# col = "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
income <- c()
for (row in rownames(df_income)) (
  for (col in colnames(df_income)) (
    income <- rbind(income, matrix(rep(c(row, col), df_income[row, col]), ncol = 2, byrow = TRUE))
  )
)

df_income_2 <- as.data.frame(income)
colnames(df_income_2) <- c("CogFail_Days", "Income")
df_income_2

# Fisher's exact test with raw data
test_income_2 = fisher.test(table(df_income_2), simulate.p.value = TRUE)

#assign factor and level 
df_income_2 = df_income_2 %>%
  mutate(CogFail_Days = factor(CogFail_Days, levels = c("None", "Low", "Med", "High")))

df_income_2 = df_income_2 %>%
  mutate(Income = factor(Income, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))

#check factor and level 
levels(df_income_2$CogFail_Days)
levels(df_income_2$Income)
is.factor(df_income_2$CogFail_Days)
is.factor(df_income_2$Income)

# combine plot and statistical test with ggbarstats
ggbarstats(
  df_income_2, CogFail_Days, Income,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test_income_2$p.value < 0.001, "< 0.001", round(test_income_2$p.value, 3))
  )
) + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = "Cognitive Failure Days", reverse = TRUE)) +  
  labs(
    x = "Income",
    y = "Proportion"
  )

##################### END OF CODE #########################
