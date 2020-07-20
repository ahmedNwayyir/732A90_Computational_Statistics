setwd("C:/Users/WizzCon/Desktop/Machine Learning/1. Workshop/6. Computational Statistics/1. Labs/Lab 3")
RNGversion(min(as.character(getRversion()), "3.6.2"))
set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")

##Question 1: Cluster sampling
population <- read.csv2("../Data/population.csv", stringsAsFactors = FALSE)

sampler <- function(data, n){
  #taking the cumsum for the probabilities means 
  #we put them in a period from 0 to 1 without overlapping
  data$prob <- cumsum(data[,2] / sum(data[,2]))
  
  cities <- as.character()
  pop    <- as.numeric()
  for(i in 1:n){
    rand <- runif(1)
    #this line takes the city with the minimum distance to the roll 
    #generated from runif .. cities with higher population cover longer periods
    #so they have higher chances to be closest to the roll
    #cities[i] <- data[which.min(abs(data$prob-rand)),][[1]]
    
    #applying the generalized inverse distribution function which takes only the nearest   
    #CMD value that is equal or larger than the probability value
    cities[i] <- data[which.min((data$prob-rand)[which((data$prob-rand)>=0)]),][[1]]
    
    pop[i]    <- data[which(data[,1] == cities[i]),][[2]]
    data      <- data[-which(data[,2] == pop[i]),]
  }
  res <- data.frame(Municipality = as.character(cities), Population = as.numeric(pop))
  return(res)
}
sampler(data = population, n = 20)

par(mfrow=c(1,2))
hist(sampler(data = population, n = 20)$Population, breaks = 50, xlab = "population")
hist(population$Population, breaks = 50, xlab = "population")



inverse_cmd <- function(n){
  u <- runif(n)
  
  inverse <- c()
  for(i in 1:n){
    if(u[i] < 0.5){
      inverse[i] <- log(2 * u[i])
    }
    else{
      inverse[i] <- -log(2 * (1 - u[i]))
    }
  }
  return(inverse)
}

x <- inverse_cmd(10000)
hist(x, breaks = 50, main = "Sample of 10000 points from Lablace Distribution")




c = 2*sqrt(exp(1))/sqrt(2*pi)
cat("The majorizing constant:", c)

#Acceptance-Rejection function
fy <- function(x){
  exp(-0.5 * x ^ 2) / sqrt(2 * pi)
}

fx <- function(x){
  exp(-abs(x)) / 2
} 
  
accept_reject <- function(n){
  R <- 0
  Y <- vector(length = n)
  for(i in 1:n){
    repeat {
      y <- inverse_cmd(1)
      U <- runif(1)
      h <- fy(y) / (c * fx(y))
      if(U <= h){
        Y[i] <- y 
        break
        }
      else{R = R+1}
    }
  }
  return(list(Y=Y, Reject=R))
}

set.seed(12345)
Z <- accept_reject(2000)
Z

cat("The number of rejections: ",Z$Reject)


hist(Z$Y, xlab = "Z", main = "Histogram of standard normal distribution", breaks = 50)



#1/c
cat(1/c)

#1-1/c
cat(1-1/c)

# The number of times that rejections occured
Z$Reject

cat("The rejection rate: ", Z$Reject/(Z$Reject+2000))

set.seed(12345)
Z = rnorm(2000)

hist(Z, main = "Histogram of standard normal distribution using R function")







