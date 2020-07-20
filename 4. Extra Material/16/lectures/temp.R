mu=matrix(0, ncol=50,nrow=1000)

for (i in 2:1000){
  mu[i,1]=rnorm(1,(mu[i-1,2]+Y[1])/2, sqrt(0.2/2))
  for (j in 2:49){
    mu[i,j]=rnorm(1,(mu[i,j-1]+mu[i-1,j+1]+Y[j])/3,sqrt(0.2/3))
  }
  mu[i,50]=rnorm(1,(mu[i,49]+Y[50])/2, sqrt(0.2/2))
}

plot(X, colMeans(mu), type="l")
points(X,Y)
plot(mu[,50], main="trace plot of mu1", type="l")
  
