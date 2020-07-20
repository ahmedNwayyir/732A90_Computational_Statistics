setwd("C:/Users/WizzCon/Desktop/Machine Learning/Workshop/Computational Statistics/Lab 2")

mortality <- read.csv2("../732A90_VT2020_Materials/mortality_rate.csv")
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

myMSE <- function(lambda, pars, iterCounter = FALSE){
  X <- pars$X
  Y <- pars$Y
  Xtest <- pars$Xtest
  Ytest <- pars$Ytest
  
  model_temp <- loess(Y ~ X, enp.target = lambda)
  pred_temp <- predict(model_temp, newdata = Xtest)
  MSE <- (1/length(pred_temp))*sum((Ytest - pred_temp)^2)
  # If we want a iteration counter
  if(iterCounter){
    if(!exists("iterForMyMSE")){
      # Control if the variable exists in the global environemnt,
      # if not, create a variable and set the value to 1. This
      # would be the case for the first iteration
      # We will call the variable 'iterForMyMSE'
      assign("iterForMyMSE",
             value = 1,
             globalenv())
    } else {
      # This part is for the 2nd and the subsequent iterations.
      # Starting of with obtaining the current iteration number
      # and then overwrite the current value by the incremental
      # increase of the current value
      currentNr <- get("iterForMyMSE")
      assign("iterForMyMSE",
             value = currentNr + 1,
             globalenv())
    }
  }
  return(MSE)
}
MSE <- numeric()
for(i in lambda){
  MSE[i] <- myMSE(i, pars, iterCounter = TRUE)
}
plot(MSE)
iterForMyMSE