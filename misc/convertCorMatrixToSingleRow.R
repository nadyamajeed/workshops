# Example correlation matrix
set.seed(1)
rawdata <- matrix(rnorm(6*100), nrow = 100, ncol = 6)
colnames(rawdata) = c("A","B","C","D","E","F")
cor_mat <- cor(rawdata)

# Get original variable names
var_names <- colnames(cor_mat)

# Create pair names
pairwise_names = c(); for(i in 1:(length(var_names)-1)) {
  for(j in 1:length(var_names)) {
    if(j > i) pairwise_names = c(pairwise_names, (paste0(var_names[i], "_with_", var_names[j])))
  }}; rm(i); rm(j)

# Get values from lower triangle (excluding diagonal)
values <- cor_mat[lower.tri(cor_mat, diag = FALSE)]

# Convert to 1-row data.frame
result_df <- as.data.frame(t(values))
colnames(result_df) <- pairwise_names
