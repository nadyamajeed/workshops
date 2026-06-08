# R version 4.5.0
library(dplyr) # version 1.2.1
library(metafor) # version 5.0-1

# import artificial data
d_SMD_MULTI =
  read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/main/evisyn/data/data_multi_SMD.csv") %>%
  metafor::escalc(
    measure = "SMD",
    n1i = n1, n2i = n2,
    m1i = m1, m2i = m2,
    sd1i = sd1, sd2i = sd2,
    data = .) %>%
  dplyr::mutate(id = row.names(.))

# analyse w/ ~ 1 | sample
metafor::rma.mv(
  yi = yi, V = vi,
  random = ~ 1 | sample,
  data = d_SMD_MULTI,
  method = "REML"
) # est = 0.7236, SE = 0.0582

# analyse w/ ~ 1 | sample/id
metafor::rma.mv(
  yi = yi, V = vi,
  random = ~ 1 | sample/id,
  data = d_SMD_MULTI,
  method = "REML"
) # est = 0.7343, SE = 0.0599

# analyse w/ ~ var | sample
metafor::rma.mv(
  yi = yi, V = vi,
  random = ~ var | sample,
  data = d_SMD_MULTI,
  method = "REML"
) # est = 0.7343, SE = 0.0599

# analyse w/ vcov mx specified 
# such that var types within samples
# are taken into account
# and assume rho = .5
metafor::rma.mv(
  yi = yi, V = metafor::vcalc(
    vi = vi,
    cluster = sample,
    type = var, rho = 0.5,
    data = d_SMD_MULTI),
  random = ~ 1 | sample/id,
  data = d_SMD_MULTI,
  method = "REML"
) # est = 0.7391, SE = 0.0599

# analyse w/ vcov mx specified 
# such that var types within samples
# are taken into account
# and assume rho = .25
metafor::rma.mv(
  yi = yi, V = metafor::vcalc(
    vi = vi,
    cluster = sample,
    type = var, rho = 0.25,
    data = d_SMD_MULTI),
  random = ~ 1 | sample/id,
  data = d_SMD_MULTI,
  method = "REML"
) # est = 0.7368, SE = 0.0599

# analyse w/ vcov mx specified 
# such that var types within samples
# are taken into account
# and assume rho = .75
metafor::rma.mv(
  yi = yi, V = metafor::vcalc(
    vi = vi,
    cluster = sample,
    type = var, rho = 0.75,
    data = d_SMD_MULTI),
  random = ~ 1 | sample/id,
  data = d_SMD_MULTI,
  method = "REML"
) # est = 0.7413, SE = 0.0599
