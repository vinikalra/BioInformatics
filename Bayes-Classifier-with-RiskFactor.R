install.packages("plyr")
library("plyr")

dfhu <- read.delim("hu.dat")
dfhemo <- read.csv("hemo.csv")
dfhu$HbF <- dfhu$X.Hb

#Setting the risk factor
riskFactor <- 5

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


d1 <- density(xNonRes, kernel="gaussian", from = min(dfjoin[,1]), to= max(dfjoin[,1]))
d2 <- density(xRes, kernel="gaussian", from = min(dfjoin[,1]), to= max(dfjoin[,1]))

#Multiplying the riskFactor in the probablities
d1$y <- d1$y * riskFactor

#Plotting the Risk behaviour
plot(d1, col="green", xlab = "HbF", ylab = "Risk", main = "")
lines(d2 , col="red") 
legend('topright', legend=c("Responsive Risk", "Non Responsive Risk"), col=c("red", "green"), lty=1)
intersX <- d1$x[as.logical(abs(diff(d1$y < d2$y)))]
intersY <- d1$y[as.logical(abs(diff(d1$y < d2$y)))]

#Setting the New Threshold
abline(v=intersX)
abline(h=intersY)
threshold <- intersX[1]
print(threshold)

