library(MASS)

generate_data <- function(n, num_indicators, lambdaA, lambdaB, lambdaC, psiAC, psiBC, psiAB = 0) {
  # Step 1: Define the dimensions
  total_obs <- num_indicators * 3  # Total number of observed variables (3 latent variables)
  
  # Step 2: Construct the Lambda matrix
  Lambda <- matrix(0, nrow = total_obs, ncol = 3)  # Initialize Lambda with zeros
  Lambda[1:num_indicators, 1] <- lambdaA  # Loadings for A-related indicators
  Lambda[(num_indicators + 1):(2 * num_indicators), 2] <- lambdaB  # Loadings for B-related indicators
  Lambda[(2 * num_indicators + 1):(3 * num_indicators), 3] <- lambdaC  # Loadings for C-related indicators
  
  # Step 3: Construct the Psi matrix (latent variable covariance matrix)
  Psi <- matrix(c(
    1,     psiAB, psiAC,
    psiAB, 1,     psiBC,
    psiAC, psiBC, 1
  ), nrow = 3, ncol = 3)
  
  # Step 4: Construct the Theta matrix (diagonal error variances)
  Theta <- diag(1 - diag(Lambda %*% Psi %*% t(Lambda)), total_obs)
  
  # Step 5: Compute the observed covariance matrix
  Sigma_obs <- Lambda %*% Psi %*% t(Lambda) + Theta
  
  # Step 6: Generate multivariate normal data
  data <- mvrnorm(n = n, mu = rep(0, total_obs), Sigma = Sigma_obs)
  colnames(data) <- paste0("O", 1:total_obs)  # Name the columns O1 to O{total_obs}
  
  return(data)
}

# Example usage
set.seed(123)
data <- generate_data(
  n = 1000,
  num_indicators = 5,  # 5 indicators per latent variable
  lambdaA = 0.8, lambdaB = 0.7, lambdaC = 0.9,
  psiAC = 0.5, psiBC = 0.3
)

# Preview the first few rows of the generated data
head(data)
