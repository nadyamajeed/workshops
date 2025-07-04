# General function to reconstruct correlation matrices from wide format
make_correlation_matrices = function(df, outlabel = "var") {
  # Extract only the correlation columns (those matching 'cor' followed by digits)
  cor_cols = grep("^cor\\d+\\d+$", names(df), value = TRUE)
  
  # Identify how many variables there are
  var_nums = unique(unlist(regmatches(cor_cols, gregexpr("\\d", cor_cols))))
  var_nums = sort(as.integer(var_nums))
  n_vars = length(var_nums)
  
  # Generate all pairwise combinations (i < j)
  pairwise_names = combn(var_nums, 2, FUN = function(x) paste0("cor", x[1], x[2]))
  
  # Create matrices for each row
  matrices = lapply(seq_len(nrow(df)), function(i) {
    mat = diag(1, nrow = n_vars)
    for (colname in pairwise_names) {
      # Extract the i,j indices from the column name
      ij = as.integer(unlist(regmatches(colname, gregexpr("\\d", colname))))
      r = df[[colname]][i]
      mat[ij[1], ij[2]] = r
      mat[ij[2], ij[1]] = r
    }
    colnames(mat) = rownames(mat) = paste0(outlabel, var_nums)
    mat
  })
  
  return(matrices)
}

# Example usage
df = data.frame(
  id = c("A", "B", "C"),
  cor12 = c(0.5, 0.6, 0.6),
  cor13 = c(0.2, 0.1, 0.2),
  cor14 = c(0.1, 0.1, NA),
  cor23 = c(0.3, 0.4, 0.5),
  cor24 = c(0.2, 0.2, NA),
  cor34 = c(0.2, 0.3, NA)
)

cor_matrices = make_correlation_matrices(df, "time")
