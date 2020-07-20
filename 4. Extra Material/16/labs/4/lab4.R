#uppgift 1
pf<-function(x) exp(-x+5*log(x));

x<-c();
#note! Values should be given on log-scale
x[1]<-log(10);

r<-0;
y<-0;
for(i in 1:10000) {
 y<-rlnorm(1,x[i],1);
 u<-runif(1);
 r<-min(1,pf(y)*dlnorm(x[i],y,1)/(pf(x[i])*dlnorm(y,x[i],1)));
 if (u<=r) x[i+1]<-y else x[i+1]<-x[i];
}
plot(x, type="l")
title("X0=10")
print(mean(x[500:1000]))

chigen<-

pf<-function(x) exp(-x+5*log(x));

x<-c();
x[1]<-10;
r<-0;
y<-0;
for(i in 1:10000) {
 y<-rchisq(1,floor(x[i]+1));
 r<-min(1,pf(y)*dchisq(x[i],floor(y+1))/(pf(x[i])*dchisq(y,floor(x[i]+1))));
 if (u<=r) x[i+1]<-y else x[i+1]<-x[i];
}
plot(x[1:2000], type="l")
title(" Chi- square X0=10")
print(mean(x[1000:10000]))
 

n<-2000
chigen<-function(start) {
  
  pf<-function(x) exp(-x+5*log(x));
  
  x<-c();
  x[1]<-start;
  r<-0;
  y<-0;
  for(i in 1:(n-1)) {
    y<-rchisq(1,floor(x[i]+1));
    r<-min(1,pf(y)*dchisq(x[i],floor(y+1))/(pf(x[i])*dchisq(y,floor(x[i]+1))));
    if (u<=r) x[i+1]<-y else x[i+1]<-x[i];
  }
  return(x);
  
}

v<-matrix(ncol=10, nrow=n,c(0));

for (s in 1:10) v[,s]<-chigen(s);
library(coda)
f=mcmc.list()
for (i in 1:10) f[[i]]=as.mcmc(v[,i])
gelman.diag(f)


#u2
setwd("Z:/732A38/16/labs/4")
load("chemical.RData")
plot(X,Y)
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
plot(mu[,50], main="trace plot of mu50", type="l")
