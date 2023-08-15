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

############################## prepare data ##############################

# read in data and reverse items
dataReactFull = read.csv("reactivity234_230811.csv") %>%
  dplyr::mutate(
    across(.cols = c(opt4, opt5, opt6),    .fns = ~ 6 - .x),
    across(.cols = c(grat3, grat6),        .fns = ~ 8 - .x),
    across(.cols = c(kind3, kind4),        .fns = ~ 6 - .x),
    across(.cols = c(forg2, forg4, forg6), .fns = ~ 8 - .x)
  )

# prepare data.frame without demographics for later use
dataReactUse = dataReactFull %>% dplyr::select(-sample, -demAge, -demSexF)

############################## descriptives ##############################

dataReactFull %>%
  psych::describe() %>%
  as.data.frame() %>%
  dplyr::select(n, mean, sd, min, max, skew) %>%
  dplyr::mutate_if(is.numeric, ~ round(.x, 2)) %>%
  dplyr::mutate(skewFlag = ifelse(abs(skew) > 1, "FLAG", "")) %>%
  write.csv("descriptives.csv", row.names = TRUE)

############################## distributions ##############################

dataReactUse %>%
  # rearrange order of variables
  dplyr::select(-starts_with("react"), everything(), starts_with("react")) %>%
  # get long data
  reshape2::melt() %>%
  # add skew to item labels
  merge(
    dataReactUse %>%
      psych::describe() %>%
      as.data.frame() %>%
      dplyr::select(skew) %>%
      dplyr::mutate(variable = rownames(.), skew = round(skew, 2))
  ) %>%
  dplyr::mutate(variableWithSkew = paste0(variable, " (skew = ", skew, ")")) %>%
  # plot histograms by item
  ggplot2::ggplot(aes(x = value)) +
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::facet_wrap(
    ~ variableWithSkew,
    scales = "free",
    ncol = 6) +
  ggplot2::scale_y_continuous(lim = c(0, nrow(dataReactUse)), breaks = c(0, 250, 500, 750)) +
  ggplot2::scale_x_continuous(breaks = 1:7) +
  ggplot2::theme_bw(); ggplot2::ggsave(filename = "distributions.pdf", height = 9, width = 11)

############################## correlation matrices and heatmap ##############################

dplyr::bind_rows(
  # pearson correlation matrix
  dataReactUse %>%
    cor(method = "pearson") %>%
    round(2) %>%
    reshape2::melt() %>%
    dplyr::mutate(type = "pearson"),
  # spearman correlation matrix
  dataReactUse %>%
    cor(method = "spearman") %>%
    round(2) %>%
    reshape2::melt() %>%
    dplyr::mutate(type = "spearman")) %>%
  # plot heatmap
  ggplot2::ggplot(aes(x = Var1, y = Var2, fill = value)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(aes(label = value), color = "black", size = 1.5) +
  ggplot2::scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    limit = c(-1,1), midpoint = 0, space = "Lab", name = "correlation") +
  ggplot2::coord_fixed() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_blank()) +
  ggplot2::facet_wrap(~ type); ggplot2::ggsave(filename = "heatmap.pdf", width = 11, height = 9)

############################## exploratory network analysis ##############################

# establish partial correlation matrix
modelSaturated = psychonetrics::ggm(
  covs = dataReactUse %>% cov(method = "spearman"),
  covtype = "UB",
  nobs = dataReactUse %>% nrow(),
  corinput = FALSE,
  omega = "full"
)

# prune spurious correlations (defined via alpha = .01)
# optimise via modification index (defined via bic)
modelOptimised = modelSaturated %>%
  psychonetrics::prune(alpha = 0.01, recursive = TRUE) %>%
  #psychonetrics::setoptimizer(optimizer = "ucminf")
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
