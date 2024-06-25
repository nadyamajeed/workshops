##### SET UP #####

# R version 4.3.1

library(dplyr)   # version 1.1.2
library(ggplot2) # version 3.4.2

##### CREATE SOME ARTIFICIAL DATA #####
# this is just for demonstration purposes
# import your own data if you already have it

set.seed(0)

myDataImport = data.frame(
  AuthorYear = c(
    "Meow et al. (2020)", "Woof et al. (2019)", "Neigh (2018)", "Oink et al. (2004)", "Quack & Moo (2005)",
    "Chirp (2012)", "Baa et al. (2003)", "Tweet et al. (2004)", "Ribbit (2014)", "Hoo et al. (2013)")) %>%
  dplyr::mutate(
    N = sample(x = 50:750, size = nrow(.)),
    N1_offset = sample(x = 1:10, size = nrow(.)),
    N2_offset = sample(x = 1:10, size = nrow(.)),
    # group 1
    N1 = N + N1_offset,
    est1 = rnorm(n = nrow(.), mean = 0.3, sd = 0.2),
    SE1 = rnorm(n = nrow(.), mean = N1*-1), SE1 = 0.1 * SE1 / min(SE1),
    CI1.lower = est1 - 1.96*SE1,
    CI1.upper = est1 + 1.96*SE1,
    # group 2
    N2 = N + N2_offset,
    est2 = rnorm(n = nrow(.), mean = 0.7, sd = 0.2),
    SE2 = rnorm(n = nrow(.), mean = N2*-1), SE2 = 0.1 * SE2 / min(SE2),
    CI2.lower = est2 - 1.96*SE2,
    CI2.upper = est2 + 1.96*SE2,
    # remove cols that wouldnt be in extracted data
    N = NULL, N1_offset = NULL, N2_offset = NULL,
    SE1 = NULL, SE2 = NULL
  )

##### CLEAN DATA #####

myDataClean = 
  # rearrange to long format instead of wide
  # and add group labels
  rbind(
    myDataImport %>% 
      dplyr::select(AuthorYear, N = N1, est = est1, CI.lower = CI1.lower, CI.upper = CI1.upper) %>%
      dplyr::mutate(group = "Female"), 
    myDataImport %>% 
      dplyr::select(AuthorYear, N = N2, est = est2, CI.lower = CI2.lower, CI.upper = CI2.upper) %>%
      dplyr::mutate(group = "Male")
  ) %>%
  # add label called est_with_ci to hold estimates with 95% CIs
  dplyr::mutate(
    est_with_ci = paste0(round(est, 2), " [", round(CI.lower, 2), ", ", round(CI.upper, 2), "]")
  ) %>%
  # rearrange to reverse alphabetical
  dplyr::arrange(desc(AuthorYear), group)

##### PLOT #####

AuthorYearLabels = unique(myDataClean$AuthorYear)
AuthorYearSpacer = sapply(1:length(AuthorYearLabels), function(X) paste(rep(" ", X), collapse = ""))

ggplot(myDataClean, aes(y = AuthorYear, colour = group)) +
  # fix y axis to add spacers 
  scale_y_discrete(limits = as.vector(rbind(AuthorYearLabels, AuthorYearSpacer))) +
  # fix x axis ticks
  scale_x_continuous(breaks = (-100:100)/5) +
  # fix axis titles
  ylab("") + xlab("Effect size [95% CI]") + 
  # plot points
  geom_point(aes(x = est, size = N), position = position_dodge(width = -0.5)) + 
  scale_size(range = c(0.5, 2.5)) +
  # plot CIs
  geom_errorbar(aes(xmin = CI.lower, xmax = CI.upper), width = 0.3, position = position_dodge(width = -0.5)) +
  # add annotations
  geom_text(aes(x = est, label = est_with_ci), size = 2, position = position_dodge(width = -1.5)) +
  # clean up misc visuals
  theme_classic() +
  # hide y axis tick marks
  theme(axis.ticks.y = element_blank()) +
  # fix legend
  theme(legend.position = "bottom") + labs(colour = "Sex") 

##### SAVE #####

ggsave("myPlot.pdf", height = 8, width = 6)

##### END OF CODE #####
