rm(list = ls())
source('KLExpand.R')

# Read data frame
hemo <- read.csv('hemo.csv')

# Remove class variable from dataset
hemo <- hemo[, -ncol(hemo)]
hemo <- as.matrix(hemo)

# Run KL expand function on hemo data
output <- klexpand(hemo)

# Extract feature values, feature vectors and eigen values from output
feature_values <- as.data.frame(output[1])
feature_vectors <- as.data.frame(output[2])
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

new_feature_space <- as.matrix(feature_vectors[newFeat[1]])
for (i in seq(2,length(newFeat)))
{
  index <- newFeat[i]
  new_feature_space <- cbind(new_feature_space,feature_vectors[index])
}

print('Dimensions of feature space before reduction')
print(dim(feature_vectors))

print('Dimensions of feature space after reduction')
print(dim(new_feature_space))

