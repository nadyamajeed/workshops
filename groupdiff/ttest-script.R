# NADYANNA M. MAJEED (https://github.com/nadyamajeed/workshops)
# VERSION: 06/12/21 19:24PM



########## set up ##########

# load libraries
# indicate versions for reproducibility
library(dplyr)      # version 1.0.7
library(tidyr)      # version 1.1.4
library(psych)      # version 2.1.6
library(car)        # version 3.0-9
library(effectsize) # version 0.4.5
library(ggplot2)    # version 3.3.5



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

#####> visualise independent samples t-test #####

# box plot with scatterplot
# more info: http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data_between, aes(x = condition, y = taskAnxiety)) +
  # plot the boxes
  geom_boxplot() +
  # plot the points
  geom_jitter(width = 0.1, size = 0.25) +
  # fix the axis titles and tick labels
  ylab("Task anxiety") +
  xlab("Condition") +
  scale_x_discrete(breaks = c("control", "threat"), labels = c("Control", "Threat"))

# violin plot
ggplot(data_between, aes(x = condition, y = taskAnxiety)) +
  # plot the violins
  geom_violin() +
  # fix the axis titles and tick labels
  ylab("Task anxiety") +
  xlab("Condition") +
  scale_x_discrete(breaks = c("control", "threat"), labels = c("Control", "Threat"))

# box and violin plot, with colours
ggplot(data_between, aes(x = condition, y = taskAnxiety)) +
  # plot the violins and boxes
  geom_violin(aes(fill = condition)) +
  geom_boxplot(width = 0.1) + 
  # fix the axis titles and tick labels
  ylab("Task anxiety") +
  xlab("Condition") +
  scale_x_discrete(breaks = c("control", "threat"), labels = c("Control", "Threat")) +
  # remove legend to reduce unnecessary clutter
  theme(legend.position = "none")



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

#####> visualise dependent samples t-test #####

# box and violin plot, with colours
# note that data must be in long format
# currently, it is in wide format
# we can reshape the data using pivot_longer() from the tidyr package
# and then pipe the reshaped data into ggplot()
data_within %>%
  pivot_longer(cols = c(stress_ctrl, stress_exp)) %>%
  ggplot(aes(x = name, y = value)) +
  # plot the violins and boxes
  geom_violin(aes(fill = name)) +
  geom_boxplot(width = 0.1) + 
  # fix the axis titles and tick labels
  ylab("Stress") +
  xlab("Condition") +
  scale_x_discrete(breaks = c("stress_ctrl", "stress_exp"), labels = c("Control", "Mindfulness")) +
  # remove legend to reduce unnecessary clutter
  theme(legend.position = "none")
