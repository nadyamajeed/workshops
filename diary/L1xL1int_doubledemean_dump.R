# !!!!!!!!!!!!!!!!!!!!!!!!
# JUST SOME SCRATCH
# DO NOT USE
# !!!!!!!!!!!!!!!!!!!!!!!!

# read more???????
# Balli, H. O., & Sørensen, B. E. (2013). Interaction effects in econometrics. Empirical Economics, 45(1), 583–603.
# Giesselmann, M., & Schmidt-Catran, A. W. (2022). Interactions in fixed effects regression models. Sociological Methods & Research, 51(3), 1100–1127.

library(dplyr)
library(lme4)

# generate clustered data with L1 x L1 interaction

# how many clusters and indivs per cluster?

Nc = 100
Niperc = 100

# generate L2 data first where we have
# random intercept per cluster
# uncorrelated preds Aic_true(between) and Bic_true(between) 

Ic_true = rnorm(Nc)
Ac_true = rnorm(Nc)
Bc_true = rnorm(Nc)

# for each cluster
# generate L1 data where we have
# uncorrelated preds Aic_true(within) and Bic_true(within)
# random noise

data = lapply(
  1:Nc,
  function(c) {
    c = c
    Ic_true = Ic_true[c]
    Ac_true = Ac_true[c]
    Bc_true = Bc_true[c]
    i = 1:Niperc
    Aic_true = rnorm(Niperc, Ac_true)
    Bic_true = rnorm(Niperc, Bc_true)
    e = rnorm(Niperc)
    data.frame(c, Ic_true, Ac_true, Bc_true, i, Aic_true, Bic_true, e)
  }
) %>% 
  do.call(rbind, .) %>%
  as.data.frame()

# choose reg coeffs

BETA01 = 1 # between Aic_true -> Y
BETA02 = 1 # between Bic_true -> Y
BETA03 = 0 # between Aic_true * between Bic_true -> Y

BETA10 = 1 # within Aic_true -> Y
BETA20 = 1 # within Bic_true -> Y
BETA30 = 1 # within Aic_true * within Bic_true -> Y

# create and add Y
# note that L1 x L1 interaction itself has been de-meaned

data = data %>%
# ????????? START
  dplyr::group_by(c) %>%
  dplyr::mutate(
    L1int_ci = (Aic_true - Ac_true) * (Bic_true - Bc_true),
    L1int_cmean = mean(L1int_ci),
    L1int_demeaned = L1int_ci - L1int_cmean) %>%
  dplyr::ungroup() %>%
# ????????? END
  dplyr::mutate(
    Y =
      # cluster effects
      Ic_true + 
      BETA01 * (Ac_true) + 
      BETA02 * (Bc_true) +
      BETA03 * (Ac_true * Bc_true) +
      # indiv effects
      BETA10 * (Aic_true - Ac_true) +
      BETA20 * (Bic_true - Bc_true) + 
      BETA30 * L1int_demeaned # ?????????
  ) %>%
  dplyr::group_by(c) %>%
  dplyr::mutate(
    Abar = mean(Aic_true),
    Bbar = mean(Bic_true)
  ) %>%
  dplyr::ungroup()

# test against model

lme4::lmer(
  Y ~ 
    1 + Abar + Bbar + Abar:Bbar + 
    I(Aic_true - Abar) + I(Bic_true - Bbar) + I(Aic_true - Abar):I(Bic_true - Bbar) +
    (1 | c), 
  data = data) %>%
  summary(correlation = FALSE)
