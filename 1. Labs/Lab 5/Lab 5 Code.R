setwd("C:/Users/WizzCon/Desktop/Machine Learning/1. Workshop/6. Computational Statistics/1. Labs/Lab 5")
RNGversion(min(as.character(getRversion()), "3.6.2"))

#1.1
library("readxl")
lottery <- read_xls("../Data/lottery.xls")
Y <- lottery$Draft_No
X <- lottery$Day_of_year
plot(X,Y)


#1.2
plot(X,Y, pch = 19, cex = 0.5)
Y_hat <- loess(Y ~ X)$fitted
points(X, Y_hat, col = "blue", pch = 19, cex = 0.5)


#1.3
T <- function(X,Y){
  Xb <- X[which.max(Y)]
  Xa <- X[which.min(Y)]
  Y_Xb <- max(Y)
  Y_Xa <- min(Y)

  t <- (Y_Xb - Y_Xa) / (Xb - Xa)
  return(t)
}


B <- 2000
n <- dim(lottery)[1]
tsamp <- rep(NA,B)
set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
for (i in 1:B){
  mysample <- lottery[sample(n, size = n, replace = TRUE),]
  X        <- mysample$Day_of_year
  Y        <- loess(mysample$Draft_No ~ mysample$Day_of_year)$fitted
  tsamp[i] <- T(X, Y)
}
hist(tsamp,breaks=100,col=gray(0.8),main="",xlab="t",ylab="",freq=FALSE,cex.axis=1.5,cex.lab=1.5)

pvalue <- sum(tsamp > 0) / B
pvalue



#1.4
test_hypothesis <- function(data, B){
  n  <- dim(data)[1]
  tx <- numeric(B)
  set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
  for (i in 1:B){
    X     <- sample(n, size = n, replace = TRUE)
    Y     <- loess(data$Draft_No ~ X)$fitted
    tx[i] <- T(X, Y) 
  }
  return(tx)
}
tx <- test_hypothesis(data = lottery, B = 2000)
hist(tx,breaks=100)

Y0 <- loess(lottery$Draft_No ~ lottery$Day_of_year)$fitted
t0 <- T(lottery$Day_of_year, Y0)
pvalue <- length(which(abs(tx) >= abs(t0))) / B
pvalue

#1.5
alpha   <- seq(0.1, 10, 0.1)
pvalues <- vector(length = length(alpha))
#set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
for(j in 1:length(alpha)){
  X <- lottery$Day_of_year
  Y <- numeric(366)
  for(i in 1:366){
    beta <- rnorm(1, mean=183, sd=10)
    YY   <- min((alpha[j] * X[i] + beta), 366)
    Y[i] <- max(0, YY)
  }
  newdata <- data.frame("Day_of_year" = X, "Draft_No" = Y)
  tvec    <- test_hypothesis(data = newdata, B = 200)
  Y0      <- loess(Draft_No ~ Day_of_year, data = newdata)$fitted
  t0      <- T(newdata$Day_of_year, Y0)
  pvalues[j] <- mean(abs(tvec) > abs(t0))
}
pvalues
hist(tvec, breaks = 50)
length(which(pvalues < 0.05)) / 100



prices <- read_xls("../Data/prices1.xls")
hist(prices$Price, breaks = 50)

mean(prices$Price)

library(boot)
mymean <- function(data, ind){
  mean(data[ind])
}
set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
samples <- boot(data = prices$Price, statistic = mymean, R = 2000)

plot(samples)


bias_correction <- 2 * mean(prices$Price) - mean(samples$t)
bias_correction


var(samples$t)[[1]]

conf_ints <- boot.ci(boot.out = samples)
conf_ints


hist(samples$t, main="95% confidence intervals", xlab="Mean Price", breaks = 50)
abline(v = conf_ints$normal[2:3], 
       col = "black", 
       lty = "dashed", 
       lwd = 2)
abline(v = mean(conf_ints$normal[2:3]), 
       col = "black", 
       lwd = 2)
abline(v = conf_ints$percent[4:5], 
       col = "blue", 
       lty = "dashed", 
       lwd = 2)
abline(v = mean(conf_ints$percent[4:5]), 
       col = "blue", 
       lwd = 2)
abline(v = conf_ints$bca[4:5], 
       col = "red", 
       lty = "dashed", 
       lwd = 2)
abline(v = mean(conf_ints$bca[4:5]), 
       col = "red", 
       lwd = 2)
legend(x      = "topright", 
       legend = c("Normal", "Percent", "Bca"), 
       fill   = c("black", "blue", "red"))



Ti <- rep(NA, length(prices$Price))
n  <- length(prices$Price)
for(i in 1:n){
  jack  <- prices$Price[-i] 
  Ti[i] <- n * mean(prices$Price) - (n-1) * mean(jack)
}
J_mean <- mean(Ti)
J_mean
J_Var  <- sum((Ti-J_mean)^2) / (n*(n-1))
J_Var

jack_means <- rep(NA, length(prices$Price))
for(i in 1:n){
  jack  <- prices$Price[-i] 
  jack_means[i] <- mean(jack)
}
hist(jack_means, breaks = 50)
