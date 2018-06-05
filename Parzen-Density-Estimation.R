library(modeest)

#Uniformly distributed random variable U
U <- runif(1000, min=0, max=1)

#Normally distributed random variable N
N <- rnorm(1000, mean=0.5, sd=0.05)

#Probability Density Function

#PDFs of uniformly distributed variable with gaussian kernel and different bandwidths
GU1 <- mlv(U, method="parzen", kernel="gaussian", bw=0.01)
GU2 <- mlv(U, method="parzen", kernel="gaussian", bw=0.05)
GU3 <- mlv(U, method="parzen", kernel="gaussian", bw=0.1)

#PDFs of normally distributed variable with gaussian kernel and different bandwidths
GN1 <- mlv(N, method="parzen", kernel="gaussian", bw=0.01)
GN2 <- mlv(N, method="parzen", kernel="gaussian", bw=0.05)
GN3 <- mlv(N, method="parzen", kernel="gaussian", bw=0.1)

#PDFs of uniformly distributed variable with uniform kernel
UU1 <- mlv(U, method="parzen", kernel="uniform", bw=0.01)
UU2 <- mlv(U, method="parzen", kernel="uniform", bw=0.05)
UU3 <- mlv(U, method="parzen", kernel="uniform", bw=0.1)

#PDFs of normally distributed variable with uniform kernel
UN1 <- mlv(N, method="parzen", kernel="uniform", bw=0.01)
UN2 <- mlv(N, method="parzen", kernel="uniform", bw=0.05)
UN3 <- mlv(N, method="parzen", kernel="uniform", bw=0.1)


#Plotting pdfs of uniformly and normally distributed random variables
plot(GU1)
plot(GN1)
plot(NU1)
plot(NN1)

plot(GU2)
plot(GN2)
plot(NU2)
plot(NN2)

plot(GU3)
plot(GN3)
plot(UN3)
plot(NN3)