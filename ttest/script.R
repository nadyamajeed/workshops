# load libraries
# indicate versions for reproducibility
library(dplyr)      # version 1.0.7
library(car)        # version 3.0-9
library(effectsize) # version 0.4.5



########## independent samples t-test ##########

#####> import and inspect data #####

# read in data
data_between = read.csv("https://github.com/nadyamajeed/workshops/raw/main/ttest/ttest-between.csv")

# briefly inspect the data
data_between %>% glimpse()

#####> get descriptives of anxiety per condition #####

# threat condition
data_between %>%
  filter(condition == "threat") %>%
  select(taskAnxiety) %>%
  describe()

# control condition
data_between %>%
  filter(condition == "control") %>%
  select(taskAnxiety) %>%
  describe()

#####> conduct independent samples t-test #####

# check HoVA
leveneTest(taskAnxiety ~ condition, data = data_between)

# conduct t-test
# set paired = FALSE as it's independent samples
# set var.equal = TRUE since HoVA was not violated
t_between = t.test(taskAnxiety ~ condition, data = data_between, paired = FALSE, var.equal = TRUE)

# look at results of t-test
t_between

# calculate cohen's d
# note that this differs from manuscript value
# as d in manuscript and d from cohens_d
# are calculated using different formulae
# (there are many ways to calculate cohen's d)
# (choose wisely and report transparently)
cohens_d(t_between)



########## dependent (aka paired or correlated) samples t-test ##########

#####> import and inspect data #####

# read in data
data_within = read.csv("https://github.com/nadyamajeed/workshops/raw/main/ttest/ttest-within.csv")

# briefly inspect the data
data_within %>% glimpse()

#####> get descriptives of stress per condition #####

data_within %>%
  select(stress_ctrl, stress_exp) %>%
  describe()

#####> conduct dependent samples t-test #####

# conduct t-test
# set paired = TRUE as it's dependent samples
t_within = t.test(data_within$stress_ctrl, data_within$stress_exp, paired = TRUE)

# look at results of t-test
t_within

# calculate cohen's d
# (there are many ways to calculate cohen's d)
# (choose wisely and report transparently)
cohens_d(t_within)
