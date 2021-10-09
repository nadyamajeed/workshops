# load libraries
# indicate versions for reproducibility
library(dplyr) # version 1.0.7
library(haven) # version 2.4.3
library(psych) # version 2.1.6

##### import and inspect data #####

# read in data
data.raw = read_sav("https://github.com/nadyamajeed/workshops/raw/main/data-cleaning/data-cleaning.sav")

# briefly inspect the data
data.raw %>% glimpse()
data.raw %>% describe()

##### basic cleaning of invalid data #####

data.fix = data.raw %>%
  mutate(
    # remove negative values in teaparty_per_week
    teaparty_per_week = ifelse(teaparty_per_week < 0, NA_real_, teaparty_per_week),
    # remove values > 4.3 in gpa
    gpa = ifelse(gpa > 4.3, NA_real_, gpa)
  )

##### score psychometric scales and get reliability #####

# score the agreeableness measure
data.scored = data.fix %>%
  mutate(
    # reverse code agr2, agr4, agr6
    agr2 = 6 - agr2,
    agr4 = 6 - agr4,
    agr6 = 6 - agr6,
    # aggregate
    agree_avg = rowMeans(across(agr1:agr6))
  )

# get cronbach's alpha for agreeableness
data.scored %>%
  select(agr1:agr6) %>%
  alpha()

##### get descriptives from final data #####

# for everything
data.scored %>%
  describe()

# for only some things we care about
data.scored %>%
  select(teaparty_per_week, gpa, agree_avg) %>%
  describe()

##### write our final data (in two formats) for future use #####

write.csv(data.scored, file = "final.csv", row.names = FALSE)
write_sav(data.scored, file = "final.sav")