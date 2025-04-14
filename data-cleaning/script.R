##### START OF CODE #####

# R version 4.4.1

# load libraries
# indicate versions for reproducibility
library(dplyr) # version 1.1.4
library(haven) # version 2.5.4
library(psych) # version 2.5.3

##### import and inspect data #####

# read in data
data.raw = haven::read_sav("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/data-cleaning/data-cleaning.sav")

# briefly inspect the data
data.raw %>% dplyr::glimpse()
data.raw %>% psych::describe()

##### basic cleaning of invalid data #####

data.fix = data.raw %>%
  dplyr::mutate(
    # remove negative values in teaparty_per_week
    teaparty_per_week = ifelse(teaparty_per_week < 0, NA_real_, teaparty_per_week),
    # remove values > 4.3 in gpa
    gpa = ifelse(gpa > 4.3, NA_real_, gpa)
  )

# briefly inspect the data
data.fix %>% dplyr::glimpse()
data.fix %>% psych::describe()

##### score psychometric scales and get reliability #####

# score the agreeableness measure
data.scored = data.fix %>%
  dplyr::mutate(
    # reverse code agr2, agr4, agr6
    dplyr::across(
      .cols = c(agr2, agr4, agr6),
      .fns = ~ 6 - .x
    ),
    # aggregate
    agree_avg = rowMeans(across(agr1:agr6))
  )

# get cronbach's alpha for agreeableness
agr.alpha = data.scored %>%
  dplyr::select(agr1:agr6) %>%
  psych::alpha()

# cronbach's alpha - full output
print(agr.alpha)

# cronbach's alpha - just alpha & 95% CI (feldt)
print(agr.alpha$feldt)

##### get descriptives from final data #####

# for everything
data.scored %>%
  psych::describe()

# for only some things we care about
data.scored %>%
  dplyr::select(teaparty_per_week, gpa, agree_avg) %>%
  psych::describe()

##### write our final data (in two formats) for future use #####

write.csv(data.scored, file = "final.csv", row.names = FALSE)
write_sav(data.scored, file = "final.sav")

##### END OF CODE #####
