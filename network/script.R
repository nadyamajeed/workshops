############################## set up ##############################

# set working directory to that of script's current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R version 4.3.1
library(dplyr)         # version 1.1.2
library(psych)         # version 2.3.6
library(reshape2)      # version 1.4.4
library(ggplot2)       # version 3.4.2
library(bootnet)       # version 1.5.4
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

# prepare labels
labelsAll = data.frame(
  constructs = c(
    rep("Reactivity", 3),
    rep("Optimism", 6),
    rep("Gratitude", 6),
    rep("Self-kindness", 6),
    rep("Self-forgiveness", 6)),
  items = c(
    "My mood often goes up and down.",
    "[...] tension and turmoil as I think of the day's events.",
    "Minor setbacks sometimes irritate me too much.",
    "In uncertain times, I usually expect the best.",
    "I'm always optimistic about my future.",
    "I expect more good things to happen to me than bad.",
    "If something can go wrong for me, it will. [R]",
    "I hardly ever expect things to go my way. [R]",
    "I rarely count on good things happening to me. [R]",
    "I have so much in life to be thankful for.",
    "[...] grateful for, it would be a very long list.",
    "When I look at the world, I don't see much to be grateful for. [R]",
    "I am grateful to a wide variety of people.",
    "[...] more able to appreciate [...] my life history.",
    "Long amounts of time can go by before I feel grateful [...] [R]",
    "[...] patient towards those aspects of my personality I don't like.",
    "[...] give myself the caring and tenderness I need.",
    "[...] judgmental about my own flaws and inadequacies. [R]",
    "[...] impatient towards those aspects of my personality I don't like. [R]",
    "I try to see my failings as part of the human condition.",
    "[...] feelings of inadequacy are shared by most people.",
    "[...] over time I can give myself some slack.",
    "I hold grudges against myself for negative things I've done. [R]",
    "Learning from bad things that I've done helps me get over them.",
    "It is really hard for me to accept myself once I've messed up. [R]",
    "With time I am understanding of myself for mistakes I've made.",
    "I don't stop criticizing myself [...] [R]"
  )
)

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

############################## network analysis ##############################

# > ESTIMATE OVERALL NETWORK -----

# estimate network
# notes:
# Estimates an unregularized GGM using the glasso algorithm and stepwise model selection,
# using the 'ggmModSelect' function from the qgraph package.
# default tuning is 0 for ggmModSelect
# default stepwise = TRUE
result = bootnet::estimateNetwork(dataReactUse, default = "ggmModSelect", corMethod = "spearman")

# plot
pdf(file = "network_spring.pdf", width = 20, height = 12.5); result %>% plot(
  negDashed = FALSE,
  layout = "spring", # can be spring, circle, or groups
  label.cex = 0.7,
  label.prop = 0.9,
  legend.cex = 0.5,
  legend.mode = "style2",
  groups = labelsAll$constructs,
  nodeNames = labelsAll$items); dev.off()
pdf(file = "network_circle.pdf", width = 20, height = 12.5); result %>% plot(
  negDashed = FALSE,
  layout = "circle", # can be spring, circle, or groups
  label.cex = 0.7,
  label.prop = 0.9,
  legend.cex = 0.5,
  legend.mode = "style2",
  groups = labelsAll$constructs,
  nodeNames = labelsAll$items); dev.off()
pdf(file = "network_groups.pdf", width = 20, height = 12.5); result %>% plot(
  negDashed = FALSE,
  layout = "groups", # can be spring, circle, or groups
  label.cex = 0.7,
  label.prop = 0.9,
  legend.cex = 0.5,
  legend.mode = "style2",
  groups = labelsAll$constructs,
  nodeNames = labelsAll$items); dev.off()

# view some info about the fitted model
result

# get density based on ^
74 / 351

# bootstrap to get 95% CIs
# note to self: nBoots = 100, nCores = 3 takes 14m 13s
result_boot = bootnet::bootnet(
  result,
  type = "nonparametric",
  nBoots = 50, nCores = 4)

# look at width of variation (i.e., 95% CIs)
pdf("bootstrap_edges.pdf"); result_boot %>% plot(labels = FALSE, order = "sample"); dev.off()

# > ESTIMATE CENTRALITY OF NODES -----

# view centrality plot
# note to self: understand what each of the 4 are telling us
result %>% qgraph::centralityPlot(include = "all", orderBy = "ExpectedInfluence")

# bootstrap with case-dropping to get stability of estimates
# note to self: nBoots = 50, nCore = 4 takes 4m 53s
result_bootCase = bootnet::bootnet(
  result,
  type = "case",
  statistics = c("strength", "expectedInfluence", "betweenness", "closeness"),
  nBoots = 50, nCores = 4)

# plot
pdf("bootstrap_centrality.pdf"); result_bootCase %>% plot("all"); dev.off()

# get CS coefficient
# Epskamp et al. (2018) suggest that "CS-coefficient should not be below 0.25, and preferably above 0.5."
result_bootCase %>% bootnet::corStability()

############################## end of code ##############################

# to consider
# MORE IMPT TO RQ:
# differenceTest (bootnet)
# predictability? find out how to do in bootnet (or other package)
# EXPLORATORY:
# clustering, exploratory graph analysis, check number + stability of clusters
