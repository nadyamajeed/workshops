##### SET UP -----

library(dplyr)

# set up custom helper function
# to convert SDs and correlation
# into variance-covariance matrix
sigma_rho_to_vcovmx = function(sigma1, sigma2, rho) {
  matrix(c(
    sigma1^2,            rho*sigma1*sigma2,
    rho*sigma1*sigma2,   sigma2^2
  ), nrow = 2, byrow = TRUE)
}

##### INPUT PARAMETERS -----

#> N -----

N = 1000 # number of unique participants per group

#> RESIDUAL OF M -----

sigma_eM_c1 = 0.5      # SD of residual of M in condition 1
sigma_eM_c2 = 0.5      # SD of residual of M in condition 2
rho_eM_c1_eM_c2 = 0.25 # correlation between residuals of M

# generate vcov matrix for residuals of M
vcovmx_eM_c1_eM_c2 = sigma_rho_to_vcovmx(sigma_eM_c1, sigma_eM_c2, rho_eM_c1_eM_c2)

#> RESIDUAL OF ETA -----

sigma_eETA_c1 = 0.5        # SD of residual of ETA in condition 1
sigma_eETA_c2 = 0.5        # SD of residual of ETA in condition 2
rho_eETA_c1_eETA_c2 = 0.25 # correlation between residuals of ETA

# generate vcov matrix for residuals of ETAs
vcovmx_eETA_c1_eETA_c2 = sigma_rho_to_vcovmx(sigma_eETA_c1, sigma_eETA_c2, rho_eETA_c1_eETA_c2)

#> RESIDUAL OF Y1 -----

sigma_eY1_c1 = 0.5       # SD of residual of Y1 in condition 1
sigma_eY1_c2 = 0.5       # SD of residual of Y1 in condition 2
rho_eY1_c1_eY1_c2 = 0.25 # correlation between residuals of Y1

# generate vcov matrix for residuals of M
vcovmx_eY1_c1_eY1_c2 = sigma_rho_to_vcovmx(sigma_eY1_c1, sigma_eY1_c2, rho_eY1_c1_eY1_c2)

#> RESIDUAL OF Y2 -----

sigma_eY2_c1 = 0.5       # SD of residual of Y2 in condition 1
sigma_eY2_c2 = 0.5       # SD of residual of Y2 in condition 2
rho_eY2_c1_eY2_c2 = 0.25 # correlation between residuals of Y2

# generate vcov matrix for residuals of M
vcovmx_eY2_c1_eY2_c2 = sigma_rho_to_vcovmx(sigma_eY2_c1, sigma_eY2_c2, rho_eY2_c1_eY2_c2)

#> RESIDUAL OF Y3 -----

sigma_eY3_c1 = 0.5       # SD of residual of Y3 in condition 1
sigma_eY3_c2 = 0.5       # SD of residual of Y3 in condition 2
rho_eY3_c1_eY3_c2 = 0.25 # correlation between residuals of Y3

# generate vcov matrix for residuals of M
vcovmx_eY3_c1_eY3_c2 = sigma_rho_to_vcovmx(sigma_eY3_c1, sigma_eY3_c2, rho_eY3_c1_eY3_c2)

#> FACTOR LOADINGS OF Y1, Y2, Y3 ONTO ETA -----

lambda_Y1 = 0.5
lambda_Y2 = 0.5
lambda_Y3 = 0.5

#> REGRESSIONS -----

beta_X_M = 0.5
beta_X_ETA = -0.5
beta_M_ETA = -0.5

##### GENERATE DATA -----

set.seed(0)

simData = 
  # initialise dataframe with participants and groups
  data.frame(
    ID = 1:(N*3),
    G = rep(c("A", "B", "C"), N)
  ) |>
  # create dummy indicators for groups
  dplyr::mutate(
    GB = ifelse(G == "B", 1, 0),
    GC = ifelse(G == "C", 1, 0)
  ) |>
  # create residual of M in condition 1 and condition 2
  cbind(MASS::mvrnorm(
    n = N*3,
    mu = c(eM_c1 = 0, eM_c2 = 0),
    Sigma = vcovmx_eM_c1_eM_c2
  )) |> 
  # create residual of ETA in condition 1 and condition 2
  cbind(MASS::mvrnorm(
    n = N*3,
    mu = c(eETA_c1 = 0, eETA_c2 = 0),
    Sigma = vcovmx_eETA_c1_eETA_c2
  )) |>
  # create residual of Y1, Y2, Y3 in condition 1 and condition 2
  cbind(MASS::mvrnorm(
    n = N*3,
    mu = c(eY1_c1 = 0, eY1_c2 = 0),
    Sigma = vcovmx_eY1_c1_eY1_c2
  )) |>
  cbind(MASS::mvrnorm(
    n = N*3,
    mu = c(eY2_c1 = 0, eY2_c2 = 0),
    Sigma = vcovmx_eY2_c1_eY2_c2
  )) |>
  cbind(MASS::mvrnorm(
    n = N*3,
    mu = c(eY3_c1 = 0, eY3_c2 = 0),
    Sigma = vcovmx_eY3_c1_eY3_c2
  )) |>
  # create M, ETA, Y1, Y2, Y3 in each condition
  dplyr::mutate(
    M_c1 = beta_X_M*0*(GB+GC) + eM_c1,
    M_c2 = beta_X_M*1*(GB+GC) + eM_c2,
    ETA_c1 = beta_X_ETA*0*(GB+GC) + beta_M_ETA*M_c1 + eETA_c1,
    ETA_c2 = beta_X_ETA*1*(GB+GC) + beta_M_ETA*M_c2 + eETA_c2,
    Y1_c1 = lambda_Y1*ETA_c1 + eY1_c1,
    Y1_c2 = lambda_Y1*ETA_c2 + eY1_c2,
    Y2_c1 = lambda_Y2*ETA_c1 + eY2_c1,
    Y2_c2 = lambda_Y2*ETA_c2 + eY2_c2,
    Y3_c1 = lambda_Y3*ETA_c1 + eY3_c1,
    Y3_c2 = lambda_Y3*ETA_c2 + eY3_c2
  ) |>
  # keep only collected data
  dplyr::select(
    ID, G, 
    M_c1, M_c2, 
    Y1_c1, Y1_c2, Y2_c1, Y2_c2, Y3_c1, Y3_c2)

##### ANALYSE -----

#> without moderation -----

"
# latent factor structure
 ETA_c1 =~ Y1_c1 + Y2_c1 + Y3_c1
 ETA_c2 =~ Y1_c2 + Y2_c2 + Y3_c2
# regressions
 ETA_c1 ~ b*M_c1
 ETA_c2 ~ b*M_c2
# correlation of same construct across conditions
   M_c1 ~~ M_c2
 ETA_c1 ~~ ETA_c2
  Y1_c1 ~~ Y1_c2
  Y2_c1 ~~ Y2_c2
  Y3_c1 ~~ Y3_c2
" |>
  lavaan::sem(data = simData) |> 
  lavaan::summary()

#> with moderation via invariance testing -----

# i should observe Mdiff(M1, M2) to be different across groups
# this is the effect of condition on M moderated by group

# i should observe Mdiff(ETA1, ETA2) to be different across groups
# this is the effect of condition on ETA moderated by group

# all else should be constrained to equal
# assumes factor structure is invariant across groups
# assumes effect of M on ETA is invariant across groups

"
# latent factor structure
 ETA_c1 =~ Y1_c1 + Y2_c1 + Y3_c1
 ETA_c2 =~ Y1_c2 + Y2_c2 + Y3_c2
# regressions
 ETA_c1 ~ c(b,b,b)*M_c1
 ETA_c2 ~ c(b,b,b)*M_c2
# residual correlation of same construct across conditions
   M_c1 ~~ M_c2
 ETA_c1 ~~ ETA_c2
  Y1_c1 ~~ Y1_c2
  Y2_c1 ~~ Y2_c2
  Y3_c1 ~~ Y3_c2
" |>
  lavaan::sem(
    data = simData, 
    group = "G", 
    group.equal = c(
      "residuals", "residual.covariances",
      "lv.variances", "lv.covariances",
      "regressions", "loadings")) |> 
  lavaan::summary()

##### END OF CODE -----
