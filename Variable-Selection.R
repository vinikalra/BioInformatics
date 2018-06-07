rm(list = ls())
source('KLExpand.R')

# Read hemo data
hemo <- read.csv('hemo.csv')

# Remove the class variable from dataset
hemo <- hemo[, -ncol(hemo)]
hemo <- as.matrix(hemo)

# Run KL expand on hemo data
output <- klexpand(hemo)

# Extract feature values, feature vectors and eigen values from output
feature_vectors <- as.data.frame(output[2])
feature_values <- as.data.frame(output[1])
eigen_values <- unlist(output[3])

# Threshold for feature reduction
threshold <- 95

# Accumulate feature vectors until threshold is met
newFeat <- vector(mode="numeric", length=0)
cum_weight <- 0
for(i in seq(1:length(eigen_values)))
{
  # Calculate percentage weight of eigen value
  per_weight <- eigen_values[i]/sum(eigen_values)*100
  newFeat <- c(newFeat, i)
  
  cum_weight <- cum_weight + per_weight
  if(cum_weight > threshold)
  {
    break
  }
}

# Calculate variable weights using mu values
mu_vals <- as.matrix(feature_vectors[newFeat[1]]*eigen_values[newFeat[1]])
for (i in seq(2,length(newFeat)))
{
  index <- newFeat[i]
  mu_vals <- cbind(mu_vals,feature_vectors[index]*eigen_values[index])
}

# Calculate and sort row sums from max to min
mu <- abs(rowSums(mu_vals))
mu <- sort(mu, decreasing=TRUE, index.return=TRUE)

# Print the rank of variables
print(paste0("Rank ", seq(1,length(mu$x)), " : " ,colnames(hemo)[mu$ix]))

# Threshold for variable selection
threshold <- 80

# Variable selection
var_weight <- vector(mode="numeric", length=0)
newVar <- vector(mode="numeric", length=0)
cum_weight <- 0
for(i in seq(1:length(mu$x)))
{
  var_weight <- 100*mu$x[i]/sum(mu$x)
  cum_weight <- cum_weight + var_weight
  newVar <- c(newVar, mu$ix[i])
  if(cum_weight > threshold)
  {
    break
  }
}

print("Selected Variables: ")
print(colnames(hemo)[newVar])

write.csv(newVar, 'SelectedVar.csv')
