############################## set up ##############################

# set working directory to that of script's current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R version 4.3.1
library(dplyr)         # version 1.1.2
library(psych)         # version 2.3.6
library(reshape2)      # version 1.4.4
library(ggplot2)       # version 3.4.2
library(psychonetrics) # version 0.11
library(GPArotation)   # version 2023.8-1
library(qgraph)        # version 1.9.5

############################## prepare data ##############################

# read in data and reverse items
dataReactFull = read.csv("reactivity234_230914b.csv") %>%
  dplyr::mutate(
    across(.cols = c(opti4, opti5, opti6),    .fns = ~ 6 - .x),
    across(.cols = c(grat3, grat6),        .fns = ~ 8 - .x),
    across(.cols = c(kind3, kind4),        .fns = ~ 6 - .x),
    across(.cols = c(fose2, fose4, fose6), .fns = ~ 8 - .x),
    across(.cols = c(fosi1, fosi3, fosi5), .fns = ~ 8 - .x)
  )

# prepare data.frame without demographics for later use
dataReactUse = dataReactFull %>% dplyr::select(-sample, -demAge, -demSexF)

# prepare labels
labelsAll = data.frame(
  constructs = c(
    rep("Reactivity", 3),
    rep("Forgiveness (self)", 6),
    rep("Forgiveness (situation)", 6),
    rep("Gratitude", 6),
    rep("Kindness to self", 6),
    rep("Mindfulness", 6),
    rep("Optimism", 6)
  ),
  items = c(
    # reactivity
    "My mood often goes up and down.",
    "[...] tension and turmoil as I think of the day's events.",
    "Minor setbacks sometimes irritate me too much.",
    # forgiveness (self)
    "[...] over time I can give myself some slack.",
    "I hold grudges against myself for negative things I've done. [R]",
    "Learning from bad things that I've done helps me get over them.",
    "It is really hard for me to accept myself once I've messed up. [R]",
    "With time I am understanding of myself for mistakes I've made.",
    "I don't stop criticizing myself [...] [R]",
    # forgiveness (situation),
    "When things go wrong for reasons that can't be controlled [...] [R]",
    "With time I can be understanding of bad circumstances in my life.",
    "If I am disappointed by uncontrollable circumstances [...] [R]",
    "I eventually make peace with bad situations in my life.",
    "It’s really hard for me to accept negative situations [...] [R]",
    "Eventually I let go of negative thoughts [...]",
    # gratitude
    "I have so much in life to be thankful for.",
    "[...] grateful for, it would be a very long list.",
    "When I look at the world, I don't see much to be grateful for. [R]",
    "I am grateful to a wide variety of people.",
    "[...] more able to appreciate [...] my life history.",
    "Long amounts of time can go by before I feel grateful [...] [R]",
    # kindness to self
    "[...] patient towards those aspects of my personality I don't like.",
    "[...] give myself the caring and tenderness I need.",
    "[...] judgmental about my own flaws and inadequacies. [R]",
    "[...] impatient towards those aspects of my personality I don't like. [R]",
    "I try to see my failings as part of the human condition.",
    "[...] feelings of inadequacy are shared by most people.",
    # mindfulness
    "[...] not be conscious of it until some time later. [R]",
    "I find it difficult to stay focused [...] [R]",
    "I tend not to notice feelings [...] [R]",
    "It seems I am 'running on automatic' [...] [R]",
    "[...] I lose touch with what I'm doing right now [...] [R]",
    "I find myself preoccupied with the future or the past. [R]",
    # optimism
    "In uncertain times, I usually expect the best.",
    "I'm always optimistic about my future.",
    "I expect more good things to happen to me than bad.",
    "If something can go wrong for me, it will. [R]",
    "I hardly ever expect things to go my way. [R]",
    "I rarely count on good things happening to me. [R]"
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

# establish partial correlation matrix
model_ggmSaturated = psychonetrics::ggm(
  covs = dataReactUse %>% cor(method = "spearman"),
  covtype = "UB",
  nobs = dataReactUse %>% nrow(),
  corinput = FALSE, # should be TRUE, but set to FALSE temporarily due to backend issue
  omega = "full"
)

# prune spurious correlations (defined via alpha = .01)
# optimise via modification index (defined via bic)
model_ggmOptimised = model_ggmSaturated %>%
  setverbose(TRUE) %>%
  psychonetrics::prune(alpha = 0.01, recursive = TRUE) %>%
  psychonetrics::stepup(alpha = 0.01, criterion = "bic")

# examine model fit
model_ggmOptimised %>% fit()

# plot
pdf(file = "network_spring.pdf", width = 20, height = 12.5); model_ggmOptimised %>%
  psychonetrics::getmatrix("omega") %>%
  qgraph::qgraph(
    layout = "spring",
    theme = "colorblind",
    label.cex = 0.7,
    label.prop = 0.9,
    legend.cex = 0.5,
    legend.mode = "style2",
    labels = colnames(dataReactUse),
    groups = labelsAll$constructs,
    nodeNames = labelsAll$items); dev.off()

############################## cfa/saturated lnm 1 ##############################
# theoretical model
# without any adjustment for negatively-worded items

# factor loading matrix
Lambda1 = matrix(0, 33, 6)
Lambda1[1:3,  1] = 1
Lambda1[4:9,  2] = 1
Lambda1[10:15,3] = 1
Lambda1[16:21,4] = 1
Lambda1[22:27,5] = 1
Lambda1[28:33,6] = 1

# create model
model1_lnm = psychonetrics::lnm(
  data = dataReactUse,
  lambda = Lambda1, identification = "loadings",
  vars = colnames(dataReactUse),
  latents = unique(labelsAll$constructs)) %>%
  psychonetrics::runmodel()

# inspect fit and parameters
model1_lnm %>% fit()
model1_lnm %>% parameters() %>% View()

############################## cfa/saturated lnm 2 ##############################
# theoretical model
# plus inclusion of latent factor to capture bias from negatively worded items

# factor loading matrix
Lambda2 = matrix(0, 33, 7)
Lambda2[1:3,  1] = 1
Lambda2[4:9,  2] = 1
Lambda2[10:15,3] = 1
Lambda2[16:21,4] = 1
Lambda2[22:27,5] = 1
Lambda2[28:33,6] = 1
Lambda2[c(5,7,9,12,15,18,19,22:27,31:33),7] = 1

# create model
model2_lnm = psychonetrics::lnm(
  data = dataReactUse,
  lambda = Lambda2, identification = "loadings",
  vars = colnames(dataReactUse),
  latents = c(unique(labelsAll$constructs), "Negative wording")) %>%
  psychonetrics::runmodel()

# inspect fit and parameters
model2_lnm %>% fit()
model2_lnm %>% parameters() %>% View()

############################## lrnm 1 ##############################

# create model
model2_lrnm = psychonetrics::lrnm(
  data = dataReactUse,
  lambda = Lambda, identification = "loadings",
  vars = colnames(dataReactUse),
  latents = unique(labelsAll$constructs)) %>%
  psychonetrics::runmodel() %>%
  psychonetrics::stepup(alpha = 0.01, criterion = "bic", verbose = TRUE)

# inspect fit and parameters
model2_lrnm %>% fit()
model2_lrnm %>% parameters() %>% View()

############################## lrnm 2 ##############################

# create model
model3_lrnm = model2_lrnm %>%
  psychonetrics::prune(alpha = 0.01, recursive = TRUE, matrices = c("omega_zeta", "omega_epsilon"), verbose = TRUE) %>%
  psychonetrics::stepup(alpha = 0.01, criterion = "bic", matrices = c("omega_zeta", "omega_epsilon"), verbose = TRUE)

# inspect fit and parameters
model3_lrnm %>% fit()
model3_lrnm %>% parameters() %>% View()

############################## plots ##############################

#> write function -----

plot_lnm = function(
    model, latentNames, observedNames,
    residSize = 0.3, fade = FALSE, cut = 0,
    theme = "colorblind", colors = qgraph:::colorblind(length(latentNames)),
    display = TRUE, filename = NULL, width = 10, height = 10) {

  # get matrices
  latent_correlations = getmatrix(model, "omega_zeta")
  factor_loadings = getmatrix(model, "lambda")
  resid_vcov = getmatrix(model, "sigma_epsilon")

  # get labels
  groups = lapply(1:ncol(factor_loadings), function(x) which(factor_loadings[,x] != 0))
  names(groups) = latentNames

  # prepare plot
  p = qgraph::qgraph.loadings(
    fact = factor_loadings,
    model = "reflective",
    resid = diag(resid_vcov),
    residSize = residSize,
    groups = groups,
    labels = observedNames,
    factorCors = latent_correlations,
    fade = fade, cut = cut,
    theme = theme, colors = colors)

  # fix edges
  p$Edgelist$bidirectional[p$Edgelist$to > nrow(factor_loadings)] = FALSE
  p$Edgelist$directed[p$Edgelist$to > nrow(factor_loadings)] = FALSE

  # show plot
  if(display) plot(p)

  # write plot to pdf
  if(!is.null(filename)) {
    pdf(filename, width = width, height = height)
    plot(p)
    dev.off()
  }

  # return qgraph object
  invisible(p)
}

#> create plots -----

# cfa/saturated lnm
plot_lnm(
  model = model1_lnm,
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  filename = "plot1_cfa.pdf")

plot_lnm(
  model = model2_lnm,
  latentNames = c(unique(labelsAll$constructs), "Negative wording"),
  observedNames = colnames(dataReactUse),
  filename = "plot2_cfa.pdf")

# lrnm (saturated latent network + stepup residual network)
plot_lnm(
  model = model2_lrnm,
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  filename = "plot2_lrnm.pdf")

# lrnm (prune + stepup)
plot_lnm(
  model = model3_lrnm,
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  filename = "plot3_lrnm.pdf")

############################## end of code ##############################















##### old code ignore #####



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

# to consider
# MORE IMPT TO RQ:
# differenceTest (bootnet)
# predictability? find out how to do in bootnet (or other package)
# EXPLORATORY:
# clustering, exploratory graph analysis, check number + stability of clusters
