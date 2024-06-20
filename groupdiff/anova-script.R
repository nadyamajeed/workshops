# NADYANNA M. MAJEED (https://github.com/nadyamajeed/workshops)
# VERSION: 06/12/21 21:06PM



########## set up ##########

# load libraries
# indicate versions for reproducibility
library(dplyr)      # version 1.0.7
library(car)        # version 3.0-9
library(effectsize) # version 0.4.5
library(DescTools)  # version 0.99.43
library(rstatix)    # version 0.6.0
library(ggpubr)     # version 0.4.0

# disable scientific notation
options(scipen = 9999)



########## between-subjects anova ##########

#####> import and inspect data #####

# read in data
data_alertness = read.csv("https://github.com/nadyamajeed/workshops/raw/main/groupdiff/anova-between.csv")

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
alertness_by_condition %>% effectsize::eta_squared()

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
alertness_by_condition_and_sex %>% effectsize::eta_squared()

# conduct posthoc pairwise comparisons
alertness_by_condition_and_sex %>% PostHocTest(method = "bonferroni")



########## within-subject anova ##########

#####> import and inspect data #####

# read in data
data_stress = read.csv("https://github.com/nadyamajeed/workshops/raw/main/groupdiff/anova-within.csv")

# briefly inspect the data
data_stress %>% glimpse()

#####> get descriptives of stress per cell #####

data_stress %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    m = mean(stress),
    sd = sd(stress),
    min = min(stress),
    max = max(stress)
  )

#####> conduct one-way within-subjects anova #####
# more info: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

# conduct anova (will automatically also check for sphericity assumption)
stress_by_condition = anova_test(dv = stress, wid = id, within = condition, data = data_stress)

# look at results without correction for violated assumptions
# output includes generalised eta squared (ges)
stress_by_condition %>% get_anova_table(correction = "none")

# look at results with auto correction for violated assumptions
# output includes generalised eta squared (ges)
stress_by_condition %>% get_anova_table(correction = "auto")

# conduct posthoc pairwise comparisons
stress_pairwise = data_stress %>% pairwise_t_test(stress ~ condition, paired = TRUE, p.adjust.method = "bonferroni")

# look at results of pairwise comparisons
stress_pairwise

#####> visualise one-way within-subjects anova #####
# more info: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

data_stress %>%
  # mutate data to capitalise conditions for visuals/aesthetics
  dplyr::mutate(condition = case_when(
    condition == "happy" ~ "Happy",
    condition == "sad"   ~ "Sad",
    condition == "pink"  ~ "Pink"
  )) %>%
  # generate detailed boxplot
  ggboxplot(x = "condition", y = "stress", order = c("Happy", "Sad", "Pink")) %>%
  ggpar(ylab = "Stress", xlab = "Condition") +
  # add significance bars
  stat_pvalue_manual(add_xy_position(stress_pairwise, x = "condition", step.increase = 1.5)) +
  # add annotations
  labs(
    subtitle = get_test_label(stress_by_condition, detailed = TRUE),
    caption = get_pwc_label(stress_pairwise)
  )



########## end of code ##########
