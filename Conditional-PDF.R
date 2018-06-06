install.packages("plyr")
library("plyr")

dfhu <- read.delim("hu.dat")
dfhemo <- read.csv("hemo.csv")

dfhu$HbF <- dfhu$X.HbF

#Joining both the data frames based on HbF 
dfdemo <- join(dfhu, dfhemo, by= dfhu$Hbf, type="right")
dfjoin <- dfdemo[,c("X.HbF", "FHbF")]
dfjoin <- unique(dfjoin)

#Determining the class of the data points on the basis of 15% criteria
y <- dfjoin$FHbF
for (i in seq(1:nrow(dfjoin))) {
  if(y[[i]] >= 15)
    dfjoin$class[[i]] <- 'Y'
  else dfjoin$class[[i]] <- 'N'
}


xRes <- dfjoin[dfjoin$class =='Y',][,1]
xNonRes <- dfjoin[dfjoin$class =='N',][,1]

#estimate the conditional probabilities fhbf|H(x|HR) and fhbf|H(x|HNR) using a uniform kernel. 
#Plotting the conditional PDFs
plot(density(xNonRes, kernel="rectangular"), col="green", xlab="HbF",main="Conditional PDF using Uniform kernel")
lines(density(xRes, kernel="rectangular"), col="red")
legend('topright', legend=c("Responsive", "Non Responsive"), col=c("red", "green"), lty=1)
