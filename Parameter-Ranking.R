dfhemo <- read.csv("hemo.csv")

# Creating ground Truth Classes
y <- dfhemo$FHbF
for (i in seq(1:nrow(dfhemo))) {
  if(y[[i]] >= 15)
    dfhemo$eClass[[i]] <- 'Y'
  else dfhemo$eClass[[i]] <- 'N'
}

#Baye's Classification Method
bayes_classfr_accuracy <- function(varCol){
  
  xRes <- dfhemo[dfhemo$eClass=='Y',][,varCol]
  xNonRes <- dfhemo[dfhemo$eClass=='N',][,varCol]
  
  #Conditional Pdf for Responsive and Non- Responsive classes
  
  d1 <- density(xNonRes, kernel="gaussian", from = min(dfhemo[,varCol]), to= max(dfhemo[,varCol]))
  d2 <- density(xRes, kernel="gaussian", from = min(dfhemo[,varCol]), to= max(dfhemo[,varCol]))
  plot(d1, col="green", xlab= names(dfhemo)[varCol], main="Conditional Pdf" )
  
  lines(d2, col="red")
  legend('topright', legend=c("Responsive", "Non Responsive"), col=c("red", "green"), lty=1)
  
  #Calculating the threshold value
  
  intersX <- d1$x[as.logical(abs(diff(d1$y < d2$y)))]
  intersY <- d1$y[as.logical(abs(diff(d1$y < d2$y)))]
  
  abline(v=intersX)
  abline(h=intersY)
  threshold <- intersX[1]
  
  #Predicting the class using the new threshold value
  
  y <- dfhemo[,varCol]
  for (i in seq(1:nrow(dfhemo))) {
    if(y[[i]] >= threshold)
      dfhemo$pClass[[i]] <- 'Y'
    else dfhemo$pClass[[i]] <- 'N'
  }
  
  #Creating confusion Matrix and computing Accuracy
  
  confM <- table(dfhemo$eClass, dfhemo$pClass)
  accuracy <- (confM[1,1] + confM[2,2])*100/sum(confM)
  print(accuracy)
  
}

for(i in seq(1:23))
{
  bayes_classfr_accuracy(i)
}

