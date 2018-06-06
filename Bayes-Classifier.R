install.packages("plyr")
library("plyr")


dfhu <- read.delim("hu.dat")
dfhemo <- read.csv("hemo.csv")
dfhu$HbF <- dfhu$X.HbF

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

# Plotting the conditional PDFs with gaussian kernel

d1 <- density(xNonRes, kernel="gaussian", from = min(dfjoin[,1]), to= max(dfjoin[,1]))
d2 <- density(xRes, kernel="gaussian", from = min(dfjoin[,1]), to= max(dfjoin[,1]))
plot(d1, col="green",xlab="HbF", main="Conditional PDF")
lines(d2 , col="red")
legend('topright', legend=c("Responsive", "Non Responsive"), col=c("red", "green"), lty=1)

#FInding the intersection value
intersX <- d1$x[as.logical(abs(diff(d1$y < d2$y)))]
intersY <- d1$y[as.logical(abs(diff(d1$y < d2$y)))]


abline(v=intersX)
abline(h=intersY)

#Setting the new threshold
threshold <- intersX[1]
#Output Threshold
print(threshold)


