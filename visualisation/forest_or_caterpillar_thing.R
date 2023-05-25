##### START #####

# R version 3.6.3
library(dplyr)   # version 1.1.0
library(tidyr)   # version 1.1.4
library(ggplot2) # version 3.3.5

##### IMPORT & CLEAN DATA #####

# data comes from a parallel mediation scenario; 
#> X --> D --> Y and X --> A --> Y
#> where Y has 7 components which are entered as 7 separate outcomes

# the dataset has four columns;
#> "path" refers to which path in the mediation model it is estimating
#>> last digit refers to which Y component is involved
#>> text prefix refers to which type of path in the mediation 
#>> "A" paths refer to mediator A while "D" paths refer to mediator D
#> "b" refers to the estimate (i.e., unstandardised path coefficient)
#> "ci.lower" and "ci.upper" refer to the lower and upper 95% confidence intervals of b

resultsSummary = 
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/visualisation/resultsSummary.csv") %>%
  # break "path" into two columns indicating type of path and Y component
  tidyr::separate(path, into = c("path", "component"), sep = "(?<=[A-Za-z])(?=\\d)") %>%
  # label the 7 components of Y
  # as well as the 4 types of paths (reverse order for plotting purposes)
  # and add an indicator for statistical significance
  dplyr::mutate(
    component = case_when(
      component == 1 ~ "1\n(Quality)",
      component == 2 ~ "2\n(Latency)",
      component == 3 ~ "3\n(Duration)",
      component == 4 ~ "4\n(Efficiency)",
      component == 5 ~ "5\n(Disturbance)",
      component == 6 ~ "6\n(Medication)",
      component == 7 ~ "7\n(Dysfunction)"
    ),
    path = case_when(
      path == "indirectD" ~ "Indirect 1 (Depression)",
      path == "indirectA" ~ "Indirect 2 (Anxiety)",
      path == "direct"    ~ "Direct",
      path == "total"     ~ "Total") %>% reorder(., desc(.)),
    sig = (ci.lower * ci.upper > 0)
  )

##### PLOT FOREST/CATERPILLAR THING #####

ggplot(
  # tell ggplot where to get the data from
  data = resultsSummary,
  # tell ggplot what to use for x axis, y axis, and colour
  mapping = aes(x = b, y = path, colour = sig)) +
  # add in point estimates and 95% CIs
  geom_point() + geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper)) +
  # add a dashed vertical line indicating 0
  geom_vline(xintercept = 0, lty = "dashed") +
  # fix plot title and axis titles
  ylab("") + xlab("b [95% CI]") + ggtitle("Consistency Across Sleep Components") +
  # fix x axis breaks and tick labels
  scale_x_continuous(labels = ~ sprintf("%.2f", .), breaks = (-10:10)/10) +
  # tell ggplot exactly what colours to use
  scale_colour_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
  # misc cleanup
  theme(legend.position = "none", axis.ticks.y = element_blank(), strip.text.y = element_text(size = 5)) +
  # divide plot by components so that it's neater
  facet_wrap(~ component, ncol = 1, strip.position = "right")

# write to file
ggsave("plot.png")

##### END #####
