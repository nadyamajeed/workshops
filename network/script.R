############################## set up ##############################

# set working directory to that of script's current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R version 4.3.1
library(dplyr)         # version 1.1.2
library(purrr)         # version 1.0.2
library(reshape2)      # version 1.4.4
library(psych)         # version 2.3.6
library(psychonetrics) # version 0.11
library(GPArotation)   # version 2023.8-1
library(ggplot2)       # version 3.4.2
library(qgraph)        # version 1.9.5

# custom function to make merges easier
merge4 = function(a, b, c, d) a %>% merge(b, all = TRUE) %>% merge(c, all = TRUE) %>% merge(d, all = TRUE)

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
labelsAll = read.csv("labels.csv")

############################## descriptives ##############################

dataReactFull %>%
  psych::describe() %>%
  as.data.frame() %>%
  dplyr::select(n, mean, sd, min, max, skew) %>%
  dplyr::mutate_if(is.numeric, ~ round(.x, 2)) %>%
  dplyr::mutate(skewFlag = ifelse(abs(skew) > 1, "FLAG", "")) %>%
  write.csv("output/descriptives.csv", row.names = TRUE)

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
  ggplot2::theme_bw(); ggplot2::ggsave(filename = "output/distributions.pdf", height = 9, width = 11)

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
  ggplot2::facet_wrap(~ type); ggplot2::ggsave(filename = "output/heatmap.pdf", width = 11, height = 9)

############################## lnm 1: cfa/saturated ##############################
# theoretical model
# without any adjustment for item wording

# factor loading matrix
Lambda1 = matrix(0, 39, 7)
Lambda1[1:3,  1] = 1
Lambda1[4:9,  2] = 1
Lambda1[10:15,3] = 1
Lambda1[16:21,4] = 1
Lambda1[22:27,5] = 1
Lambda1[28:33,6] = 1
Lambda1[34:39,7] = 1

# create model
model1_lnm = psychonetrics::lnm(
  data = dataReactUse,
  identification = "variance", standardize = "z",
  lambda = Lambda1,
  vars = colnames(dataReactUse),
  latents = unique(labelsAll$constructs)) %>%
  psychonetrics::runmodel()

############################## lnm 2: cfa/saturated with method factors ##############################
# theoretical model
# plus inclusion of two extra latent factors
# to capture bias from negatively & positively worded items

# factor loading matrix
Lambda2 = matrix(0, 39, 9)
Lambda2[1:3,  1] = 1
Lambda2[4:9,  2] = 1
Lambda2[10:15,3] = 1
Lambda2[16:21,4] = 1
Lambda2[22:27,5] = 1
Lambda2[28:33,6] = 1
Lambda2[34:39,7] = 1
Lambda2[labelsAll$wording=="neg",8] = 1
Lambda2[labelsAll$wording=="pos",9] = 1

# ensure method factors are not correlated with other factors
Omega = matrix(0,ncol(Lambda2),ncol(Lambda2))
Omega[1:7,1:7] = 1
diag(Omega) = 0

# create model
model2_lnm = psychonetrics::lnm(
  data = dataReactUse,
  identification = "variance", standardize = "z",
  lambda = Lambda2, omega_zeta = Omega,
  vars = colnames(dataReactUse),
  latents = c(unique(labelsAll$constructs), "Negative wording", "Positive wording")) %>%
  psychonetrics::runmodel()

############################## lnm 3: pruned ##############################
# theoretical model
# plus inclusion of two extra latent factors
# to capture bias from negatively & positively worded items
# plus model search via prune

# conduct model search starting from prev model via prune
model3_lnm = model2_lnm %>%
  psychonetrics::prune(alpha = 0.01, recursive = TRUE, matrices = c("omega_zeta"), verbose = TRUE)

############################## lnm 4: stepped up ##############################
# theoretical model
# plus inclusion of two extra latent factors
# to capture bias from negatively & positively worded items
# plus model search via prune
# plus model search via stepup

# ensure zeroes in omega (method factors orthogonal to measure factors) are retained in model search
model3_lnm@parameters$identified[
  model3_lnm@parameters$matrix == "omega_zeta" &
    (model3_lnm@parameters$var2_id %in% (8:9) | model3_lnm@parameters$var1_id %in% (8:9))
] = TRUE

# conduct model search starting from prev model via stepup
model4_lnm = model3_lnm %>%
  psychonetrics::stepup(alpha = 0.01, criterion = "bic", matrices = c("omega_zeta"), verbose = TRUE)

############################## bootstrap last model for stability check ##############################

bootLNM4 = function(runNumber = NULL, prop = 0.75) {
  # create model
  cat("Starting single bootstrap run ", runNumber, " at ", as.character(Sys.time()), "\n", sep = "")
  model2_lnm_boot = psychonetrics::lnm(
    data = dataReactUse %>% dplyr::slice_sample(prop = prop),
    identification = "variance", standardize = "z",
    lambda = Lambda2, omega_zeta = Omega,
    vars = colnames(dataReactUse),
    latents = c(unique(labelsAll$constructs), "Negative wording", "Positive wording")) %>%
    psychonetrics::runmodel()

  # ensure zeroes in omega (method factors orthogonal to measure factors) are retained in model search
  model2_lnm_boot@parameters$identified[
    model2_lnm_boot@parameters$matrix == "omega_zeta" &
      (model2_lnm_boot@parameters$var2_id %in% (8:9) | model2_lnm_boot@parameters$var1_id %in% (8:9))
  ] = TRUE

  # conduct model search starting from prev model via stepup
  model4_lnm_boot = model2_lnm_boot %>%
    psychonetrics::prune(alpha = 0.01, recursive = TRUE, matrices = c("omega_zeta"), verbose = FALSE) %>%
    psychonetrics::stepup(alpha = 0.01, criterion = "bic", matrices = c("omega_zeta"), verbose = FALSE)

  # extract edges from latent network
  omega_zeta_boot = model4_lnm_boot %>% psychonetrics::getmatrix("omega_zeta")
  cat("Done with single bootstrap run ", runNumber, " at ", as.character(Sys.time()), "\n", sep = "")
  invisible(omega_zeta_boot)
}

# run bootstrapping
Sys.time()
boots_estimates = parallel::mclapply(1:100, bootLNM4, mc.cores = parallel::detectCores()-1)
Sys.time()

# write bootstrapped output for future use
boots_estimates %>% saveRDS("output/boots_estimates.RDS")

# get proportion of times the edge is included
if(!exists("boots_estimates")) readRDS("output/boots_estimates.RDS")
boots_present = lapply(boots_estimates, function(x) ifelse(x == 0, 0, 1))
boots_proportions = purrr::reduce(boots_present, `+`) / length(boots_present)

############################## summarise all fits and estimates ##############################

#> fits -----

modelFits = merge4(
  model1_lnm %>% psychonetrics::fit() %>% dplyr::rename(LNM1 = Value),
  model2_lnm %>% psychonetrics::fit() %>% dplyr::rename(LNM2 = Value),
  model3_lnm %>% psychonetrics::fit() %>% dplyr::rename(LNM3 = Value),
  model4_lnm %>% psychonetrics::fit() %>% dplyr::rename(LNM4 = Value)) %>%
  dplyr::mutate_if(is.numeric, ~ round(.x, 3)) %>%
  dplyr::filter(Measure %in% c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "aic.ll", "bic"))

modelFits %>% write.csv("output/modelfits.csv", row.names = FALSE)
modelFits %>% print()

#> estimates (latent network and factor loadings) -----

getParameters = function(model, name) {
  out = model %>% psychonetrics::parameters() %>%
    dplyr::filter(matrix %in% c("omega_zeta", "lambda")) %>%
    dplyr::filter(!(matrix == "lambda" & est == 0)) %>%
    dplyr::select(matrix, var1:var2_id, est, se, p) %>%
    dplyr::mutate_if(is.numeric, ~round(.x, 4))
  colnames(out)[c(7:9)] = paste0(colnames(out)[c(7:9)], name)
  invisible(out)
}

modelParams = merge4(
  model1_lnm %>% getParameters("1"),
  model2_lnm %>% getParameters("2"),
  model3_lnm %>% getParameters("3"),
  model4_lnm %>% getParameters("4")) %>%
  dplyr::arrange(matrix, var2_id, var1_id) %>%
  dplyr::mutate(est4_inclusion = NA)

for(r in 1:7)
  for(c in 1:7)
    if(r != c)
      modelParams[
        modelParams$var1_id==r & modelParams$var2_id==c & modelParams$matrix=="omega_zeta",
        "est4_inclusion"] =
  boots_proportions[r, c]; rm(r); rm(c)

modelParams %>% write.csv("output/modelparameters.csv", row.names = FALSE)
modelParams %>% View()

#> look at calculated CI plot for model 2 -----

model2_lnm %>% psychonetrics::CIplot(); ggsave("output/plot2_ci.pdf")

############################## plots ##############################

#> write function -----

plotLNM = function(
    model,
    which_latents = NULL,
    latentNames, observedNames,
    residSize = 0.3, vsize = NULL, fade = FALSE, cut = 0,
    theme = "colorblind", colors = qgraph:::colorblind(length(latentNames)),
    mar = c(2, 2, 2, 2), title = NULL,
    display = TRUE, filename = NULL, width = 7.5, height = 7.5) {

  # get matrices
  latent_correlations = psychonetrics::getmatrix(model, "omega_zeta")
  factor_loadings     = psychonetrics::getmatrix(model, "lambda")
  resid_vcov          = psychonetrics::getmatrix(model, "sigma_epsilon")
  if(!is.null(which_latents)) {
    latent_correlations = latent_correlations[which_latents,which_latents]
    factor_loadings     = factor_loadings[,which_latents]
  }

  # get labels
  groups = lapply(1:ncol(factor_loadings), function(x) which(factor_loadings[,x] != 0))
  names(groups) = latentNames

  # prepare plot
  p = qgraph::qgraph.loadings(
    fact = factor_loadings,
    model = "reflective",
    resid = diag(resid_vcov),
    residSize = residSize,
    vsize = vsize,
    groups = groups,
    labels = observedNames,
    nodeNames = c(groups, observedNames),
    factorCors = latent_correlations,
    fade = fade, cut = cut,
    theme = theme, colors = colors, mar = mar,
    title = title,
    DoNotPlot = TRUE)

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

#> create main plots -----

# cfa/saturated lnm without method factors
plot1 = plotLNM(
  model = model1_lnm,
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  vsize = c(3, 10),
  title = "Saturated Latent Network Model",
  filename = "output/plot1_lnm.pdf")

# cfa/saturated lnm with method factors
plot2 = plotLNM(
  model = model2_lnm,
  which_latents = c(1:7),
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  vsize = c(3, 10),
  title = "Saturated Latent Network Model\n(adjusted for methods factors, not shown)",
  filename = "output/plot2_lnm.pdf")

# cfa/saturated lnm with method factors and prune
plot3 = plotLNM(
  model = model3_lnm,
  which_latents = c(1:7),
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  vsize = c(3, 10),
  title = "Latent Network Model\n(adjusted for methods factors, not shown)\nafter model search (prune only)",
  filename = "output/plot3_lnm.pdf")

# cfa/saturated lnm with method factors and prune and stepup
plot4 = plotLNM(
  model = model4_lnm,
  which_latents = c(1:7),
  latentNames = unique(labelsAll$constructs),
  observedNames = colnames(dataReactUse),
  vsize = c(3, 10),
  title = "Latent Network Model\n(adjusted for methods factors, not shown)\nafter model search (prune + stepup)",
  filename = "output/plot4_lnm.pdf")

# write one combined plot
pdf("output/plots.pdf", width = 7.5*2, height = 7.5*2)
par(mfrow=c(2, 2))
plot(plot1); plot(plot2); plot(plot3); plot(plot4)
dev.off()

#> create bootstrap comparison plots -----

pdf("output/plotsboot.pdf", width = 7.5*2, height = 7.5)
par(mfrow=c(1, 2))

# cfa/saturated lnm with method factors and prune and stepup BOOTSTRAPPED
psychonetrics::getmatrix(model4_lnm, "omega_zeta")[1:7,1:7] %>%
  qgraph::qgraph(
    layout = "circle",
    labels = unique(labelsAll$constructs),
    edge.color = "black",
    edge.labels = TRUE, edge.label.margin = 0.025,
    title = "Latent network structure from full data\n(Edge weights represent magnitudes)")

boots_proportions[1:7,1:7] %>%
  qgraph::qgraph(
    layout = "circle",
    labels = unique(labelsAll$constructs),
    edge.color = "black",
    edge.labels = TRUE,
    probabilityEdges = TRUE, edge.label.margin = 0.025,
    title = "Latent network structure from 100 bootstrapped samples\n(Edge weights represent proportions)")

dev.off()

############################## end of code ##############################
