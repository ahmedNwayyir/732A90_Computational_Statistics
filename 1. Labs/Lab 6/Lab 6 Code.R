setwd("C:/Users/WizzCon/Desktop/Machine Learning/1. Workshop/6. Computational Statistics/1. Labs/Lab 6")
RNGversion(min(as.character(getRversion()), "3.6.2"))

f <- function(x){
  (x^2/exp(x)) - 2 * exp(-(9 * sin(x)/(x^2+x+1)))
}

crossover <- function(x,y){
  (x+y)/2
}

mutate <- function(x){
  x^2 %% 30
}

X <- seq(from = 0, to = 30, by = 0.01)
plot(X, f(X), type = "l", main = "Objective function")

genetic <- function(maxiter, mutprob){
  sinusoid  <- seq(from = 0, to = 30, by = 0.01)
  X         <- seq(from = 0, to = 30, by = 5)
  plot(sinusoid, f(sinusoid), type = "l", main = "Objective function",
       sub = paste("maxiter = ", maxiter, ", mutprob = ", mutprob, sep = ""))
  points(X, f(X), col = "brown", pch = 20, cex = 1.5)
  
  Values <- f(X)
  for(i in 1:maxiter){
    parents <- sample(X, size = 2)
    victim  <- order(Values)[1]
    kid <- crossover(x = parents[1], y = parents[2])
    kid <- ifelse(mutprob > runif(1), mutate(kid), kid)
    X[victim] <- kid
    Values    <- f(X)
    max_val   <- max(Values)
  }
  
  points(X, Values, pch = 4, col = "blue")
  legend(x = "topright", legend = c("f(x)", "Initial", "final"), pch = c(0,20,4),
         col = c("black", "brown", "blue"), lty = c(1,0,0), pt.cex = c(0, 1, 1))
}

maxiter = c(10, 100)
mutprob = c(0.1, 0.5, 0.9)
par(mfrow = c(3,2))
set.seed(12345)
for(prob in mutprob[-3]){
  for(iter in maxiter){
    genetic(iter, prob)
  }
}

potential_kids <- numeric()
for(parent in X){
  potential_kids <- append(potential_kids, crossover(parent,parent+5))
}
potential_kids <- potential_kids[-7]
potential_kids

mutated_kids <- numeric()
for(kid in potential_kids){
  mutated_kids <- append(mutated_kids, mutate(kid))
}
mutated_kids

Values1 <- numeric()
for(kid in potential_kids){
  Values1 <- append(Values1, f(kid))
}
Values1

Values2 <- numeric()
for(kid in mutated_kids){
  Values2 <- append(Values2, f(kid))
}
Values2


## Question 2: EM algorithm
physical <- read.csv("../Data/physical1.csv")

library(ggplot2)
ggplot(physical, aes(x = X)) +
  geom_line(aes(y = Y, col = "Y"), size = .7) +
  geom_line(aes(y = Z, col = "Z"), size = .7) +
  scale_color_manual(values = c("#FF3366", "#00CCFF"), name = "Variable: ") +
  labs(y = "Value", x = "Time") +
  theme_minimal()

library("asbio")
loglik.norm.plot(physical$Y,parameter="mu")

mean(physical$Y)

n   <- dim(physical)[1]
obs <- which(!is.na(physical$Z))
mis <- which(is.na(physical$Z))
M   <- length(mis)

dif    <- Inf
lambda <- 100
t      <- 0
repeat{
  lambda_new <- (sum(physical$X * physical$Y) + 
                 M * lambda + 
                 0.5 * sum(physical$X[obs] * physical$Z[obs])) / (2*n)
  
  dif    <- abs(lambda_new - lambda)
  lambda <- lambda_new
  t      <- t + 1
  
  print(lambda)
  if(dif < 0.001)break
}
cat("\nOptimal Lambda:", lambda)
cat("\nNumber of iterations:", t)

library(ggplot2)
ggplot(physical, aes(x = X))+ 
  geom_line(mapping = aes(y = Y, color="Y")) +
  geom_line(mapping = aes(y = Z, color="Z")) +
  geom_line(mapping = aes(y = lambda/X, color="E(Y)"), linetype = "dashed") +
  geom_line(mapping = aes(y = 2*lambda/X, color="E(Z)"), linetype = "dashed") +
  scale_color_manual(values = c("#FF3366", "#00CCFF", "#FF3366", "#00CCFF"), name = "Variable: ") +
  ylab("") +
  theme(legend.position = "bottom") +
  theme_bw()
