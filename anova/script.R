# load libraries
# indicate versions for reproducibility
library(dplyr)      # version 1.0.7
library(car)        # version 3.0-9
library(effectsize) # version 0.4.5
library(DescTools)  # version 0.99.43

# disable scientific notation
options(scipen = 999)



########## between-subjects anova ##########

#####> import and inspect data #####

# read in data
data_alertness = read.csv("https://github.com/nadyamajeed/workshops/raw/main/anova/anova-between.csv")

# briefly inspect the data
data_alertness %>% glimpse()

#####> get descriptives of alertness per cell #####

data_alertness %>%
  group_by(condition, sex) %>%
  summarise(
    n = n(),
    m = mean(alertness),
    sd = sd(alertness),
    min = min(alertness),
    max = max(alertness)
  )

#####> conduct one-way between-subjects anova #####
# collapse across sex
# since it was not experimentally manipulated

# check HoVA
leveneTest(alertness ~ condition, data = data_alertness)

# conduct anova
alertness_by_condition = aov(alertness ~ condition, data = data_alertness)

# look at results
alertness_by_condition %>% summary()

# calculate eta-squared
alertness_by_condition %>% eta_squared()

# conduct posthoc pairwise comparisons
alertness_by_condition %>% PostHocTest(method = "bonferroni")

#####> conduct two-way between-subjects anova #####
# 3 (condition) x 2 (sex)

# check HoVA
leveneTest(alertness ~ condition * sex, data = data_alertness)

# conduct anova
alertness_by_condition_and_sex = aov(alertness ~ condition * sex, data = data_alertness)

# look at results
alertness_by_condition_and_sex %>% summary()

# calculate partial eta-squared
alertness_by_condition_and_sex %>% eta_squared()

# conduct posthoc pairwise comparisons
alertness_by_condition_and_sex %>% PostHocTest(method = "bonferroni")



########## within-subject anova ##########

# tbc
