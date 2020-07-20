setwd("C:/Users/WizzCon/Desktop/Machine Learning/1. Workshop/6. Computational Statistics/3. Exams")

tylor <- function(x, N){
  my_sin <- x
  my_cos <- x
  my_tan <- x
  for(n in 1:N){
    my_sin <- my_sin + sum(((-1)^n) * (x^(2*n+1)) / factorial(2*n+1))
    my_cos <- my_cos + sum(((-1)^n) * (x^(2*n)) / factorial(2*n))
    br     <- pracma::bernoulli(2*n, 0)
    my_tan <- my_tan + sum(((-1)^(n-1)) * (2^(2 * n)) * (2^(2 * n -1)) 
                           * br * (x^(2*n-1)) / factorial(2*n))
  }
  qt_tan <- my_sin / my_cos
  return(list("my_sin" = my_sin, "my_cos" = my_cos, "my_tan" = my_tan, "qt_tan" = qt_tan))
}


sin_rec <- matrix(0,20,11)
sin_dif <- matrix(0,20,10)
cos_rec <- matrix(0,20,11)
cos_dif <- matrix(0,20,10)
tan_rec <- matrix(0,20,11)
tan_dif <- matrix(0,20,10)
qt_rec <- matrix(0,20,11)
qt_dif <- matrix(0,20,10)
for(x in 0:19){
  sin_rec[x+1,11] <- sin(x)
  cos_rec[x+1,11] <- cos(x)
  tan_rec[x+1,11] <- tan(x)
  qt_rec[x+1,11]  <- tan(x)
  for(n in 1:10){
    sin_rec[x+1,n] <- tylor(x = x, N = n)$my_sin
    sin_dif[x+1,n] <- abs(sin_rec[x+1,n] - sin_rec[x+1,11])
    
    cos_rec[x+1,n] <- tylor(x = x, N = n)$my_cos
    cos_dif[x+1,n] <- abs(cos_rec[x+1,n] - cos_rec[x+1,11])
    
    tan_rec[x+1,n] <- tylor(x = x, N = n)$my_tan
    tan_dif[x+1,n] <- abs(tan_rec[x+1,n] - tan_rec[x+1,11])
    
    qt_rec[x+1,n] <- tylor(x = x, N = n)$qt_tan
    qt_dif[x+1,n] <- abs(qt_rec[x+1,n] - qt_rec[x+1,11])
  }
}
# fix(sin_rec); fix(sin_dif)
# fix(cos_rec); fix(cos_dif)
# fix(tan_rec); fix(tan_dif)
# fix(qt_rec); fix(qt_dif)

library(ggplot2)
my_plot <- function(dif){
  df <- data.frame(c(0:19), 
                    "N = 1" = dif[,1], 
                    "N = 3" = dif[,4], 
                    "N = 5" = dif[,5],
                    "N = 7" = dif[,6], 
                    "N = 9" = dif[,9])
  
  ggplot(df) +
    geom_point(aes(x = df[,1], y = df[,2], col = "1")) +
    geom_point(aes(x = df[,1], y = df[,3], col = "4")) +
    geom_point(aes(x = df[,1], y = df[,4], col = "5")) +
    geom_point(aes(x = df[,1], y = df[,5], col = "6")) +
    geom_point(aes(x = df[,1], y = df[,6], col = "9")) +
    scale_color_manual(values = c("black", "gray", "blue", "red", "green"), 
                       name = "Degree of Expansion: ") +
    labs(title = "Difference between Tylor Approximation and Base R",
         x = "Index",
         y = "Difference")+
    scale_x_continuous(breaks = seq(0,19,3)) +
    theme_bw()
}
my_plot(sin_dif)
my_plot(cos_dif)
my_plot(tan_dif)
my_plot(qt_dif)





# Assignment 2
load("data.RData")

m <- rep(0,10)
S <- matrix(0,10,10)
for(r in 1:10){
  for(c in 1:10){
    if(r==c){S[r,c] <- 1}
    else{
      ratio  <- runif(1,0,1) 
      S[r,c] <- ifelse(ratio > 0.9, 0.1, 0)
    }
    #s[c,r] <- s[r,c]
    S[lower.tri(S,diag=FALSE)] <- t(S)[lower.tri(t(S),diag=FALSE)]
  }
}
library("matrixcalc")
is.positive.definite(S)
is.symmetric.matrix(S)

# In statistics, the likelihood function measures the goodness of fit 
# of a statistical model to a sample of data for given values of 
# the unknown parameters. It is formed from the joint probability distribution 
# of the sample, but viewed and used as a function of the parameters only, 
# thus treating the random variables as fixed at the observed values.
library(mvtnorm)
f <- function(x, s){
  prod(dmvnorm(x, mean = rep(0,10), sigma = s))
}
f(X, v)

crossover <- function(x,y){
  (x+y)/2
}

mutate <- function(x){
  x^2 %% 30
}

genetic <- function(maxiter, mutprob){
  n <- dim(obs)[1]
  set.seed(12345)
  id <- sample(1:n, 6)
  X  <- obs[1:11,]
  v  <- var(X)
  is.positive.definite(v)
  is.symmetric.matrix(v)
  
  Values  <- f(X, v)
  max_val <- -Inf
  for(i in 1:maxiter){
    parents <- sample(X, size = 2)
    victim  <- order(Values)[1]
    kid <- crossover(x = parents[1], y = parents[2])
    kid <- ifelse(mutprob > runif(1), mutate(kid), kid)
    X[victim] <- kid
    Values    <- f(X)
    max_val   <- max(max_val, max(Values))
  }
  list(optimum = max_val, population = X, Values = Values)
  plot(X, f(X, var(X)), type = "l",
       xlab = "X",
       ylab = "f(X)")
  points(X, f(X), col = "red", pch = 16, cex = 2)
  points(X, Values, pch = 21, col = "blue", cex = 2, lwd = 1.5)
  legend(x = "topright", legend = c("Initial", "final"), pch = c(16,21),
         col = c("red", "blue"), cex = c(1.5, 1.5))
}
maxiter = c(10, 100)
mutprob = c(0.1, 0.5, 0.9)
par(mfrow = c(3,2))
for(prob in mutprob){
  for(iter in maxiter){
    set.seed(12345)
    genetic(iter, prob)
  }
}
