setwd("C:/Users/WizzCon/Desktop/Machine Learning/Workshop/Computational Statistics/Lab 1")

options(digits = 22)
x1 <- 1/3; x2 <- 1/4
if (x1 - x2 == 1/12) {
  print("Substraction_is_Correct")
}else {
  print("Substraction_is_wrong")
}

x1 <- 1; x2 <- 1/2
if (x1 - x2 == 1/2) {
  print("Substraction_is_Correct")
}else {
  print("Substraction_is_wrong")
}

x1 <- 1/3; x2 <- 1/4
if (isTRUE(all.equal(x1 - x2, 1/12))) {
  print("Substraction_is_Correct")
}else {
  print("Substraction_is_wrong")
}

x1 <- 1/3; x2 <- 1/4
if (round((x1 - x2), 4) == round((1/12),4)) {
  print("Substraction_is_Correct")
}else {
  print("Substraction_is_wrong")
}

#Question 2
eps <- 10^-15
f <- function(x) {x}
df <- function(x, eps){
  (f(x + eps) - f(x)) / eps
}
df(1,eps)
df(10000,eps)


#Question 3
x_vec <- rnorm(10000, mean = 0, sd = 1)
myvar <- function(x_vec) {
  n <- length(x_vec)
  (1/(1-n)) * (sum(x_vec^2) - (1/n) * sum(x_vec)^2)
}


myvar(x_vec)
n <- length(x_vec)
var(x_vec)
sum(x_vec^2)
(1/n) * sum(x_vec)^2
sum(x_vec^2) - (1/n) * sum(x_vec)^2
(1/(1-n)) * (sum(x_vec^2) - (1/n) * sum(x_vec)^2)

library(ggplot2)
Y1 <- function(x){
  n <- length(x)
  y <- numeric(length = n)
  for(i in 2:n) {
    X    <- x_vec[1:i] 
    y[i] <- myvar(X) - var(X)
  }
  #plot(y, cex = 0.5)
  df <- data.frame(c(1:length(x)),y)
  ggplot(df) +
    geom_point(aes(x = df[,1], y = df[,2]), 
               size = 0.01, 
               color = "#FF3366") +
    labs(title = "Difference between the two Variances", 
         x = "Index",
         y = "Difference")+
    theme_minimal()
}
Y1(x_vec)


var_YC <- function(v_x){
  ## v_x is a numerical vector of length greater than 2
  ## this function calculates the sample variance 
  ## using the Youngs and Cramer algorithm
  T   <- v_x[1]
  RSS <- 0
  n   <- length(v_x)
  for (j in 2:n){
    T   <- T + v_x[j]
    RSS <- RSS + ((j * v_x[j] - T)^2) / (j*(j-1))
  }
  RSS /(n-1)
}

x_vec <- rnorm(10, mean = 10^8, sd = 1)
myvar(x_vec)
var_YC(x_vec)
var(x_vec)


Y2 <- function(x){
  n <- length(x)
  y <- numeric(length = n)
  for(i in 2:n) {
    X    <- x_vec[1:i] 
    y[i] <- var_YC(X) - var(X)
  }
  
  #plot(y, cex = 0.5)
  df <- data.frame(c(1:length(x)),y)
  options(digits = 3)
  ggplot(df) +
    geom_point(aes(x = df[,1], y = df[,2]), 
               size = 0.01, 
               color = "#FF3366") +
    labs(title = "Difference between the two Variances", 
         x = "Index",
         y = "Difference")+
    theme_minimal()
}
Y2(x_vec)

options(digits = 22)
setwd("C:/Users/WizzCon/Desktop/Machine Learning/Workshop/Computational Statistics/Lab 1")
tecator <- readxl::read_xls("tecator.xls")

X <- as.matrix(tecator[,-c(1,103)])
y <- as.matrix(tecator[,103])

A <- t(X) %*% X
b <- t(X) %*% y

solve.default(A,b)

kappa(A)

X_scaled <- scale(X)
y_scaled <- scale(y)

A_new <- t(X_scaled) %*% X_scaled
b_new <- t(X_scaled) %*% y_scaled


kappa(A_new)
