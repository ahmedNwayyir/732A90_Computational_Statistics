setwd("C:/Users/WizzCon/Desktop/Machine Learning/Workshop/6. Computational Statistics/1. Labs/Lab 2")

mortality <- read.csv2("../Data/mortality_rate.csv")
mortality$LMR <- log(mortality$Rate)

n <- dim(mortality)[1]
RNGversion(min(as.character(getRversion()),"3.6.2")) ## with your R-version
set.seed(12345, kind = "Mersenne-Twister", normal.kind = "Inversion")
id <- sample(1:n, floor(n * 0.5))
train <- mortality[id,]
test  <- mortality[-id,]

lambda <- seq(0.1, 40, 0.1)
pars   <- list(X = train$Day, 
               Y = train$LMR, 
               Xtest = test$Day, 
               Ytest = test$LMR)

myMSE <- function(lambda, pars){
    model <- loess(pars$Y ~ pars$X, enp.target = lambda)
    Ypred <- predict(model, newdata = pars$Xtest)
    MSE   <- sum((pars$Ytest - Ypred)^2) / dim(test)[1]
    k     <<- k + 1
    return(MSE)
}

MSE <- vector(length = length(lambda))
k   <- 0
for(i in 1:length(lambda)){
  MSE[i] <- myMSE(lambda[i],pars)
}
k
plot(MSE, 
     pch = ifelse(MSE == MSE[which.min(MSE)], 4, 1),
     cex = ifelse(MSE == MSE[which.min(MSE)], 1.2, 0.5),
     col = ifelse(MSE == MSE[which.min(MSE)], "red", "blue"))

df <- data.frame(lambda, MSE)
which.min(MSE)
df$MSE[which.min(MSE)]
df$lambda[which.min(MSE)]

# library(ggplot2)
# ggplot()+
#   geom_point(aes(x = lambda, y = MSE))+
#   theme_minimal()
 
k <- 0  
opt1 <- optimize(myMSE, interval = c(0.1,40), tol = 0.01, pars)
opt1
k

k <- 0 
opt2 <- optim(par =2, fn = myMSE, method = "BFGS", pars = pars, control = list(trace = TRUE))
opt2
k

# otimiser <- function(f, tol){
#   phi <- (sqrt(5)-1)/2
#   x1  <- min(lambda)
#   x2  <- max(lambda)
#   a   <- phi * (x2 - x1)
#   x3  <- x2 - a
#   x4  <- x1 + a
#   t   <- sqrt(.Machine$double.eps) * abs(f(x1, pars)) + (tol/3)
#   
#    while (abs(x1 - x2) > t){
#     if(f(x4,pars) > f(x3,pars)){
#       x1 <- x1
#       x2 <- x4
#     }
#     else{
#       x1 <- x3
#       x2 <- x2
#     }
#    }
#   x <- (x3 + x4) / 2
#   return(x)
# }
# 
# k <- 0
# otimiser(myMSE, 0.01)
# k


## Question 2: Maximizing likelihood
load("C:/Users/WizzCon/Desktop/Machine Learning/Workshop/6. Computational Statistics/1. Labs/Data/data.RData")
N     <- length(data)
mu    <- (1/N) * sum(data)
sigma <- sqrt((1/N) * sum((data-mu)^2))


#It is bad idea to calculate the likelihood directly because 
#when we have even fairly large numbers in the exponent N 
#inside the denominator of the first part or in the natural exponent 
#in the second part of the likelihood function we get underflow 
#and these exponents will be 0 and so will the likelihood value.
like <- function(pars,data){
  N   <- length(data)
  mu  <- pars[1]
  sd  <- pars[2]
  log <- -(1/(sd * (sqrt(2*pi)))^N) * exp(-((1/2 * sd^2) * sum((data-mu)^2)))
  k   <<- k + 1
  return(log)
}
print(like(pars = c(mu,sigma), data), digits = 22)

log_like <- function(pars,data){
  N   <- length(data)
  mu  <- pars[1]
  sd  <- pars[2]
  log <- -log(1/(sd * (sqrt(2*pi)))^N) + (1/2) * sum((data-mu)^2/sd^2)
  k   <<- k + 1
  return(log)
}
print(log_like(pars = c(mu,sigma), data), digits = 22)

gr <- function(pars,data){
  N  <- length(data)
  mu <- pars[1]
  sd <- pars[2]
  mu_gr <- (sum(mu-data) / sd^2)
  sd_gr <- -((-N/sd) + sum((data-mu)^2) / sd^3)
  gr <- c(mu_gr,sd_gr) 
  t  <<- t + 1
  return(gr)
}
t <- 0
print(gr(pars = c(mu,sigma), data), digits = 22)

k <- 0
t <- 0
op1 <- optim(par = c(0,1), fn = log_like, method = "CG", data = data, control = list(trace = TRUE))
k1 <- k
t1 <- t

k <- 0
t <- 0
op2 <- optim(par = c(0,1), fn = log_like, method = "BFGS", data = data)
k2 <- k
t2 <- t

k <- 0
t <- 0
op3 <- optim(par = c(0,1), fn = log_like, gr = gr, method = "CG", data = data)
k3 <- k
t3 <- t

k <- 0
t <- 0
op4 <- optim(par = c(0,1), fn = log_like, gr = gr, method = "BFGS", data = data)
k4 <- k
t4 <- t

df <- data.frame(unlist(op1), unlist(op2), unlist(op3), unlist(op4))
calls <- as.data.frame(matrix(c(k1, k2, k3, k4, t1, t2, t3, t4), 2, byrow = TRUE))
names(calls) <- names(df) 
df <- rbind(df,calls)
rownames(df) <- c("Mu", "Sigma", "Negative Log-likelihood", "function evaluations", "gradient evaluations", "Convergence", "function calls", "gradient calls")
colnames(df) <- c("CG w/o gradient", "BFGS w/o gradient", "CG with gradient", "BFGS with gradient")

options(digits = 22)
knitr::kable(t(df[1:3,]))
knitr::kable(t(round(df[c(4,5,7,8),],0)))

# options(digits = 2)
# library(ggplot2)
# ggplot(as.data.frame(data), aes(x = data))+
#   geom_density()+
#   geom_vline(xintercept = mu)+
#   theme_minimal()

# pars = c(mu,sigma)
# x <- pars[1]
# y <- pars[2]
# curve(-log(1/(y * (sqrt(2*pi)))^N) + (1/2) * sum((data-x)^2/y^2))


library("asbio")
loglik.norm.plot(data,parameter="sigma.sq")

#https://scicomp.stackexchange.com/questions/507/bfgs-vs-conjugate-gradient-method
