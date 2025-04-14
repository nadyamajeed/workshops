##### START #####

reps = 100

# SIMULATION WITHOUT PARSIM

conditions = expand.grid(
  N = seq(50, 1000, by = 50),
  beta0 = c(0, 1),
  beta1 = c(0, 1)
)

conditions_repeated = conditions[rep(1:nrow(conditions), each = reps), ]

results1 = apply(
  conditions_repeated, MARGIN = 1,
  function(condition) {
    X = rnorm(n = condition["N"], mean = 0, sd = 1)
    Y = rnorm(n = condition["N"], mean = condition["beta0"], sd = 1) + 
      rnorm(n = condition["N"], mean = condition["beta1"], sd = 1)*X
    simData = data.frame(X = X, Y = Y)
    out = lm(Y ~ X, data = simData) |>
      broom::tidy()
    out$N = condition["N"]
    out$beta0 = condition["beta0"]
    out$beta1 = condition["beta1"]
    return(out)
  })

results1 = do.call(rbind, results1)

# SIMULATION WITH PARSIM

library(parSim)

results2 = parSim(
  # conditions
  N = seq(50, 1000, by = 50),
  beta0 = c(0, 1),
  beta1 = c(0, 1),
  # other settings
  reps = reps,
  nCores = 1,
  expression = {
    X = rnorm(n = N, mean = 0, sd = 1)
    Y = rnorm(n = N, mean = beta0, sd = 1) + 
      rnorm(n = N, mean = beta1, sd = 1)*X
    simData = data.frame(X = X, Y = Y)
    lm(Y ~ X, data = simData) |>
      broom::tidy()
  }
)

##### PLOT #####

library(dplyr)
library(ggplot2)

# TYPE 1 ERROR (FALSE POSITIVE) RATE OF SLOPE AGAINST SAMPLE SIZE 

results1 %>%
  dplyr::filter(term == "X" & beta1 == 0) %>%
  dplyr::mutate(t1e = ifelse(p.value < .05, "FALSE POSITIVE", "TRUE NEGATIVE")) %>%
  dplyr::group_by(N, beta0) %>%
  dplyr::summarise(T1Erate = mean(t1e == "FALSE POSITIVE")) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = N, y = T1Erate, colour = as.factor(beta0))) +
  geom_point() +
  geom_smooth(linewidth = 0.25, se = FALSE) +
  scale_y_continuous(limits = c(0, 0.5)) +
  geom_hline(yintercept = .05)

# TYPE 2 ERROR (FALSE NEGATIVE) RATE OF SLOPE AGAINST SAMPLE SIZE 

results2 %>%
  dplyr::filter(term == "X" & beta1 == 1) %>%
  dplyr::mutate(t2e = ifelse(p.value < .05, "TRUE POSITIVE", "FALSE NEGATIVE")) %>%
  dplyr::group_by(N, beta0) %>%
  dplyr::summarise(T2Erate = mean(t2e == "FALSE NEGATIVE")) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = N, y = T2Erate, colour = as.factor(beta0))) +
  geom_point() +
  geom_smooth(linewidth = 0.25, se = FALSE) +
  scale_y_continuous(limits = c(0, 0.5)) +
  geom_hline(yintercept = .05)

##### END #####
