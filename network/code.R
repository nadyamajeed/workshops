############################## set up ##############################

# set working directory to that of script's current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R version 4.3.1
library(dplyr)         # version 1.1.2
library(psych)         # version 2.3.6
library(reshape2)      # version 1.4.4
library(ggplot2)       # version 3.4.2
library(psychonetrics) # version 0.11
library(qgraph)        # version 1.9.5

# read in data
dataReactFull = read.csv("reactivity234_230811.csv") %>%
  dplyr::mutate(
    across(.cols = c(opt4, opt5, opt6),    .fns = ~ 6 - .x),
    across(.cols = c(grat3, grat6),        .fns = ~ 8 - .x),
    across(.cols = c(kind3, kind4),        .fns = ~ 6 - .x),
    across(.cols = c(forg2, forg4, forg6), .fns = ~ 8 - .x)
  )

############################## descriptives ##############################

dataReactFull %>%
  psych::describe() %>%
  as.data.frame() %>%
  dplyr::select(n, mean, sd, min, max) %>%
  dplyr::mutate_if(is.numeric, ~ round(.x, 4))

############################## prepare data (remove unused cols) ##############################

dataReactUse = dataReactFull %>% dplyr::select(-sample, -demAge, -demSexF)

############################## correlation matrix and heatmap ##############################

dataReactUse %>%
  cor() %>%
  round(2) %>%
  reshape2::melt() %>%
  ggplot2::ggplot(aes(x = Var1, y = Var2, fill = value)) +
  ggplot2::geom_tile() +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green",
    limit = c(-1,1), midpoint = 0, space = "Lab", name = "Pearson r") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggplot2::coord_fixed() +
  ggplot2::geom_text(aes(label = value), color = "black", size = 1)

############################## exploratory network analysis ##############################

# establish partial correlation matrix
modelSaturated = psychonetrics::ggm(
  data = dataReactUse,
  omega = "full"
)

# prune spurious correlations (defined via alpha = .01)
modelPruned = modelSaturated %>%
  psychonetrics::prune(alpha = 0.01, recursive = TRUE)

# optimise via modification index (defined via bic)
modelOptimised = modelPruned %>%
  psychonetrics::stepup(alpha = 0.05, criterion = "bic")

# examine model fit
modelOptimised %>% fit()

# plot
modelOptimised %>%
  psychonetrics::getmatrix("omega") %>%
  qgraph::qgraph(
    layout = "spring",
    theme = "colorblind",
    labels = colnames(dataReactUse),
    filetype = "pdf",
    filename = "psychonetrics_GGM")

############################## end of code ##############################
