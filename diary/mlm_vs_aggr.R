# start of code #

# R version 4.4.1
library(dplyr)
library(lme4)
library(broom)
library(broom.mixed)
library(ggplot2)

# function to simulate data
mySimulation = function(N_indivs, max_days, true_est) {
  
  # create L2 data
  myDataL2_A = 
    data.frame(
      id = paste0("A", 1:(N_indivs/2)),
      group = "A",
      # logit is normally distributed
      logit_i = rnorm(n = (N_indivs/2))
    )
  myDataL2_B = 
    data.frame(
      id = paste0("B", 1:(N_indivs/2)),
      group = "B",
      # logit is normally distributed
      logit_i = rnorm(n = (N_indivs/2)) + true_est*1
    )
  myDataL2 = rbind(myDataL2_A, myDataL2_B) |>
    dplyr::mutate(
      # probability of exposure can be derived from logit
      p_i = exp(logit_i) / (1 + exp(logit_i))
    )
  
  # create L1 data
  myDataL1 = data.frame()
  for(i in 1:N_indivs) {
    # get info for current person
    currentRow = myDataL2[i,]
    currentProb = currentRow$p_i
    # make daily outcomes
    currentData = 
      merge(currentRow,
            data.frame(
              day = 1:max_days,
              outcome = sample(
                c(0, 1), 
                size = max_days, 
                replace = TRUE, 
                prob = c(1 - currentProb, currentProb))
            )
      )
    # allow diff ppl to have only 80%, 90%, or 100% of days
    nonmissingness = sample(c(.8, .9, 1), size = 1)
    currentData_partial =
      currentData[sample(1:max_days, size = nonmissingness*max_days, replace = FALSE), ]
    # merge data
    myDataL1 = rbind(myDataL1, currentData_partial)
  }; rm(i); rm(currentProb); rm(nonmissingness); rm(currentData); rm(currentData_partial); rm(currentRow)
  
  # clean up data
  myDataL1 = myDataL1 |>
    dplyr::arrange(id, day)
  rownames(myDataL1) = NULL
  
  # get aggregate info
  myDataAggr = myDataL1 |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      totalcount = sum(outcome),
      totaldays = length(day)
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(id, group, totalcount, totaldays)
  
  # compare analytic methods - full mlm
  result_mlm = 
    lme4::glmer(
      outcome ~ 1 + group + (1 | id),
      family = binomial,
      data = myDataL1
    ) |>
    broom.mixed::tidy(exponentiate = FALSE)
  
  # compare analytic methods - single level
  result_single =
    glm(
      totalcount / totaldays ~ 1 + group,
      weights = totaldays,
      family = binomial,
      data = myDataAggr
    ) |>
    broom::tidy(exponentiate = FALSE)
  
  out = lapply(
    list(result_mlm, result_single),
    function(X) X |> dplyr::mutate(
      term,
      est = round(estimate, 5),
      se = round(std.error, 5),
      .keep = "none")
  )
  
  names(out) = c("glmer", "glm")
  return(out)
  
}

# set number of sims per cell
nSims = 500

# run - 1000 ppl x 10 days max
allRuns1000x10 = dplyr::bind_rows(lapply(
  1:nSims,
  function(X) dplyr::bind_rows(lapply(
    mySimulation(N_indivs = 1000, max_days = 10, true_est = 1),
    function(X) X |>
      dplyr::filter(term == "groupB")
  )) |>
    dplyr::mutate(
      run = X,
      method = c("Full data (glmer)", "Aggregated data (glm)")
    ) |>
    dplyr::select(run, method, est, se)
)); saveRDS(allRuns1000x10, file = "allRuns1000x10.RDS")

# run - 250 ppl x 10 days max
allRuns250x10 = dplyr::bind_rows(lapply(
  1:nSims,
  function(X) dplyr::bind_rows(lapply(
    mySimulation(N_indivs = 250, max_days = 10, true_est = 1),
    function(X) X |>
      dplyr::filter(term == "groupB")
  )) |>
    dplyr::mutate(
      run = X,
      method = c("Full data (glmer)", "Aggregated data (glm)")
    ) |>
    dplyr::select(run, method, est, se)
)); saveRDS(allRuns1000x10, file = "allRuns250x10.RDS")

# run - 100 ppl x 10 days max
allRuns100x10 = dplyr::bind_rows(lapply(
  1:nSims,
  function(X) dplyr::bind_rows(lapply(
    mySimulation(N_indivs = 100, max_days = 10, true_est = 1),
    function(X) X |>
      dplyr::filter(term == "groupB")
  )) |>
    dplyr::mutate(
      run = X,
      method = c("Full data (glmer)", "Aggregated data (glm)")
    ) |>
    dplyr::select(run, method, est, se)
)); saveRDS(allRuns1000x10, file = "allRuns100x10.RDS")

# compile
allRunsCompiled = rbind(
  allRuns1000x10 |> dplyr::mutate(N = "1000 individuals"),
  allRuns250x10 |> dplyr::mutate(N = "250 individuals"),
  allRuns100x10 |> dplyr::mutate(N = "100 individuals")
) |>
  dplyr::mutate(N = factor(N, levels = c(
    "100 individuals", "250 individuals", "1000 individuals"
  )))

# plot - est
ggplot(
  allRunsCompiled, 
  aes(y = method)) +
  facet_wrap(~ N, ncol = 1) +
  geom_rect(aes(
    xmin = 1*0.95, xmax = 1*1.05,
    ymin = -Inf, ymax = Inf),
    fill = "green") +
  geom_vline(aes(xintercept = 1), lty = "dashed") +
  geom_boxplot(
    aes(x = est), 
    width = 0.5,
    fill = "#FFFFFF00", 
    colour = "black") +
  coord_cartesian(xlim = c(0, 2)) +
  ylab("Analytic Method") +
  xlab("Estimate\n(True population value = 1.00)")

# plot - se
ggplot(
  allRunsCompiled |>
    dplyr::group_by(N) |>
    dplyr::mutate(empiricalSD = sd(est)) |>
    dplyr::ungroup(), 
  aes(y = method)) +
  facet_wrap(~ N, ncol = 1) +
  geom_rect(aes(
    xmin = empiricalSD*0.9, xmax = empiricalSD*1.1,
    ymin = -Inf, ymax = Inf),
    fill = "green") +
  geom_vline(aes(xintercept = empiricalSD), lty = "dashed") +
  geom_boxplot(
    aes(x = se), 
    width = 0.5,
    fill = "#FFFFFF00", 
    colour = "black") +
  ylab("Analytic Method") +
  xlab("Standard Error")

# end of code #
