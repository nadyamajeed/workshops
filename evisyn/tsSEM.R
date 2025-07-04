##### START OF CODE #####

# R version 4.5.0
library(dplyr)   # version 1.1.4
library(metaSEM) # version 1.5.0

# create artificial data

createSimMatrix = function(n, realcor) {
  data.frame(time1 = rnorm(n = n, m = 0, sd = 1)) %>%
    dplyr::mutate(
      time2 = rnorm(1, mean = realcor, sd = 0.1)*time1 + rnorm(n = nrow(.), m = 0, sd = 1),
      time3 = rnorm(1, mean = realcor, sd = 0.1)*time2 + rnorm(n = nrow(.), m = 0, sd = 1),
      time4 = rnorm(1, mean = realcor, sd = 0.1)*time3 + rnorm(n = nrow(.), m = 0, sd = 1)
      ) %>%
    cor()
}

listMatrix = list(
  sample01 = createSimMatrix(n = 90,  realcor = 0.6),
  sample02 = createSimMatrix(n = 100, realcor = 0.5),
  sample03 = createSimMatrix(n = 110, realcor = 0.4),
  sample04 = createSimMatrix(n = 120, realcor = 0.5),
  sample05 = createSimMatrix(n = 130, realcor = 0.6),
  sample06 = createSimMatrix(n = 90,  realcor = 0.5),
  sample07 = createSimMatrix(n = 100, realcor = 0.4),
  sample08 = createSimMatrix(n = 110, realcor = 0.4),
  sample09 = createSimMatrix(n = 120, realcor = 0.4),
  sample10 = createSimMatrix(n = 130, realcor = 0.5)
)

listN = list(
  sample01 = 90,
  sample02 = 100,
  sample03 = 110,
  sample04 = 120,
  sample05 = 130,
  sample06 = 90,
  sample07 = 100,
  sample08 = 110,
  sample09 = 120,
  sample10 = 130
)

# stage 1 - pooling

pooled_fixed = metaSEM::tssem1(
  Cov = listMatrix,
  n = unlist(listN),
  method = "FEM")
summary(pooled_fixed)

pooled_random = metaSEM::tssem1(
  Cov = listMatrix,
  n = unlist(listN),
  method = "REM")
summary(pooled_random)

# stage 2 - analysis

model = metaSEM::lavaan2RAM(
  "time4 ~ time3
  time3 ~ time2
  time2 ~ time1
  time1 ~~ 1*time1",
  obs.variables = c("time1", "time2", "time3", "time4"))

final_output_fixed = metaSEM::tssem2(
  pooled_fixed,
  RAM = model
) 
summary(final_output_fixed)

final_output_random = metaSEM::tssem2(
  pooled_random,
  RAM = model
) 
summary(final_output_random)

##### END OF CODE #####
