##### START OF CODE #####

# R version 4.3.1

# pick data-generating parameters
N = 514                # sample size
mu_X_i = 4.21          # mean of X
sigma_X_i = 2.03       # SD of X
beta_0 = 1.93          # intercept of Y
beta_1 = 0.10          # effect or slope of X on Y
sigma_epsilon_i = 1.00 # SD of residual

# generate data
X_i       = rnorm(n = N, mean = mu_X_i, sd = sigma_X_i)
epsilon_i = rnorm(n = N, mean = 0, sd = sigma_epsilon_i)
Y_i       = beta_0 + beta_1 * X_i + epsilon_i
data = cbind(X_i, Y_i) |> as.data.frame()

# run model and check output
lm(Y_i ~ 1 + X_i, data = data) |> summary()

##### END OF CODE #####
