options(scipen = 99999)

# population model:
# G is a categorical (grouping) variable with 3 levels: A (reference), B, C
# create dummy codes of GB (A = 0, B = 1, C = 0) and GC (A = 0, B = 0, C = 1)
# 50% of the population is A, 25% of the population is B, and 25% of the population is C
# Y = 2 + beta_GB*GB + beta_GC*GC

# set up inputs
N = 10000
beta_GB = 5
beta_GC = 5

# generate data
dataSim = data.frame(
  ID = 1:N,
  Gi = rep(c("A", "A", "B", "C"), N/4),
  Ei = rnorm(n = N, m = 0, sd = 1)) |>
  dplyr::mutate(
    GB = ifelse(Gi == "B", 1, 0),
    GC = ifelse(Gi == "C", 1, 0),
    GnonA = ifelse(Gi == "A", 0, 1),
    Yi = 2 + beta_GB*GB + beta_GC*GC + Ei)

# get null model in sample:
m0 = lm(
  Yi ~ 1,
  data = dataSim
)

# get full model in sample:
m1full = lm(
  Yi ~ 1 + GB + GC,
  data = dataSim
) 

# get collapsed model  in sample:
m1coll = lm(
  Yi ~ 1 + GnonA,
  data = dataSim
) 

# compare full model to null model:
anova(m0, m1full)

# compare collapsed model to null model:
anova(m0, m1coll)

# conclusion
# collapsing will result in loss of power when beta_GB and beta_GC are very different from each other
# collapsing will result in gain of power when beta_GB approx equal beta_GC
