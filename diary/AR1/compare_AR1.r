##### START OF CODE #####

# R version 4.4.0
library(lme4)        # 1.1-35.3
library(nlme)        # 3.1-164
library(broom.mixed) # 0.2.9.5

# generate some data
source("https://raw.githubusercontent.com/nadyamajeed/workshops/main/diary/sim_ri_AR1.r")
rm(list = setdiff(ls(), "dataFull"))

# see whether lme4 and nlme can give the same results
# when handling the AR1 differently?

# lme4 first
lme4::lmer(
  Ydi ~ 1 + Xdi + Xdi_mean + day + (1 + day | ID),
  data = dataFull
) |> broom.mixed::tidy()

# nlme next
nlme::lme(
  fixed = Ydi ~ 1 + Xdi + Xdi_mean,
  random = ~ 1 | ID,
  correlation = corCAR1(form = ~ day | ID),
  data = dataFull
) |> broom.mixed::tidy()

# looks about the same for fixed effects

##### END OF CODE #####
