setwd("C:/Users/WizzCon/Desktop/Machine Learning/1. Workshop/6. Computational Statistics/1. Labs/Lab 4")
RNGversion(min(as.character(getRversion()), "3.6.2"))

library(ggplot2)
target <- function(x){
  x^5 * exp(-x)
}

proposed <- function(x, mean){
  dlnorm(x, meanlog = mean, sdlog = 1)
}

MH <- function(x0, n, prop){
  x    <- rep(0,n)
  x[1] <- x0
  for(i in 1:n){
    y     <- rlnorm(1,x[i],1)
    u     <- runif(1)
    alpha <- min(1, (target(y)/target(x[i])) * (prop(x[i], y)/prop(y, x[i])))
    ifelse(u < alpha, x[i+1] <- y, x[i+1] <- x[i])
  }
  return(x)
}
set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
res <- MH(x = rlnorm(1), n = 10000, prop = proposed)

x <- seq(from = 0.1, to = 10, by = 0.001)
ggplot(as.data.frame(x), aes(x = x))+
  geom_line(aes(y = 120 * target(x)))+
  geom_density(stat = "density")+
  ggtitle("Target Distribution")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))


df <- data.frame(x = x, y = 120 * target(x))
ggplot(df,aes(x = x))+
  geom_line(aes(y = y), stat = "density")+
  ggtitle("Target Distribution")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))


geom_density()myplot <- function(data){
  g1 <- ggplot(as.data.frame(data), aes(x = 1:length(data), y = as.numeric(data)))+
    geom_point(size = 0.3)+
    geom_line()+
    ylab("x value")+
    xlab("Data Points")+
    ylim(c(0,10))+
    ggtitle("Sample Chain")+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5))
  
  g2 <- ggplot(as.data.frame(data), aes(x = as.numeric(data)))+
    geom_histogram(bins = 50, 
                   alpha = 0.5, 
                   color = "white", 
                   fill = "black",
                   size = 0.4)+
    xlim(c(0,10))+
    ylim(c(0,3000))+
    ylab("Data Points")+
    xlab("x value")+
    ggtitle("Sample Distribution")+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5))
  
  list(g1, g2)
}
myplot(res)[[1]]
myplot(res)[[2]]



proposed_2 <- function(x, df){
  dchisq(x, floor(df+1))
}

set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
res_2 <- MH(x = rlnorm(1), n = 10000, prop = proposed_2)
myplot(res_2)


set.seed(12345, kind="Mersenne-Twister", normal.kind="Inversion")
res_3 <- sapply(1:10, function(x0) MH(x = x0, n = 10000, prop = proposed_2))
# for(i in 1:10){
#   print(myplot(res_3[[i]]))
# }

# Need to run your MCMC several times from different starting points.
# and put them all into mcmc objects and then collect those into a mcmc.list
# Run the MCMC from at least 3 diffuse starting points.
# If you have a high dimensional parameter set use >10 MCMC runs
# Try combinations of near and far from 0.  
# The same rules apply as with using multiple starts for 
# optimization.   Make good choices.
#  At the end of each MCMC run make a mcmc object with a 
# different name:
library("coda")
mc_list <- mcmc.list()
for(i in 1:10){
  mc_list[[i]] <- as.mcmc(res_3[[i]])
} 
gelman.diag(mc_list)
plot(mc_list[[1]])
summary(mc_list[[1]])
