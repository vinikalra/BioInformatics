klexpand <- function(dfmatrix)
{
  ###################################
  # Input: Data (matrix)
  #
  # Outputs: List of,
  #         - Feature values (matrix)
  #         - Feature vectors (matrix)
  #         - Eigen values (vector)
  ###################################  

  # Taking input dataframe as matrix
  # Remove Column headers
  colnames(dfmatrix) <- NULL
  
  # Correlation matrix
  CRmatrix = cor(dfmatrix)

  y <- eigen(CRmatrix)
  
  # Feature vectors are eigen vectors
  feature_vec = y$vec

  # Compute feature values by transforming data frame into feature space
  feature_values = t(y$vec) %*% t(dfmatrix)
  feature_values = t(feature_values) # Make it column major

  eigen_val = y$val
  
  # To Reconstruct original data matrix, uncomment following line
  # df <- feature_values %*% tfeature_vec)
  
  result <- list(feature_values, feature_vec, eigen_val)
  
  return(result)
}