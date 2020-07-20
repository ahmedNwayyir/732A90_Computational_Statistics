f=function(X) 0.5*X+0.3*X^2
X=runif(100)
Y0=f(X)
plot(X,f(X))

set.seed(12345)
Y=rbinom(100,1,Y0)
data=data.frame(X=X,Y=Y)
write.csv(data, "process.csv", row.names=F)

#u2
data=read.csv("process.csv")

plot(X,Y)
res=loess(Y~X)
points(X, predict(res), cex=0.2)

data1=data.frame(X=data$X, Z=data$X^2, Y=data$Y)

ll=function(pars,data){
  b1=pars[1]
  b2=pars[2]
  n=dim(data)[1]
  p=numeric(n)
  for(i in 1:n) p[i]=max(0.01,min(0.99,b1*data$X[i]+b2*data$Z[i]))
  return(sum(-(data$Y*log(p)+(1-data$Y)*log(1-p))))
}

res=optim(c(0.1,0.1),ll,method="BFGS", data=data1)

optim(c(0.3,0.3),ll,method="BFGS", data=data1)

optim(c(0.9,0.1),ll,method="BFGS", data=data1)

n=dim(data)[1]
p=numeric(n)
for(i in 1:n) p[i]=max(0.01,min(0.99,res$par[1]*data1$X[i]+res$par[2]*data1$Z[i]))
plot(data$X,data$Y)
points(data$X,p, col="red", cex=0.2)

perm=function(data0,B){
  stat=numeric(B)
  for(b in 1:B){
    print(b)
    Z=sample(data0$Z, n)
    data2=data.frame(X=data0$X,Y=data0$Y, Z=Z)
    res=optim(c(0.1,0.1),ll,method="BFGS", data=data2)
    stat[b]=res$par[2]
  }
  hist(stat,30)
  res0=optim(c(0.1,0.1),ll,method="BFGS", data=data0)
  stat0=res0$par[2]
  print(stat0)
  return(mean(abs(stat)>abs(stat0)))
}

perm(data1,200)

#not succeeded ones
perm1=function(data0,B){
  stat=numeric(B)
  for(b in 1:B){
    print(b)
    Z=sample(data0$Z, n)
    data2=data.frame(X=data0$X,Y=data0$Y, Z=Z)
    res=lm(Y~X+Z,data=data2)
    stat[b]=coef(res)[3]
  }
  hist(stat,30)
  res0=lm(Y~X+Z,data=data0)
  stat0=coef(res0)[3]
  print(stat0)
  return(mean(abs(stat)>abs(stat0)))
}

perm1(data1,200)

#u1

invCDF=function(n){
  U=runif(n)
  return(U^(2/3))
}

sample=invCDF(1000)
hist(sample,30, freq=F)
z=seq(0,1,by=0.01)
points(z, 1.5*sqrt(z), type="l")


pf<-function(x) sqrt(x)/(x+0.1);
pf1<-function(x) x*sqrt(x)/(x+0.1);

x<-c();#note! Values should be given on log-scale
x[1]<-0.1;

r<-0;
y<-0;
for(i in 1:1000) {
  y<-rbeta(1,x[i],0.5);
  u<-runif(1);
  r<-min(1,pf(y)*dbeta(x[i],y,0.5)/(pf(x[i])*dbeta(y,x[i],0.5)));
  if (u<=r) x[i+1]<-y else x[i+1]<-x[i];
}
plot(x, type="l")
title("X0=0.1")

print(mean(x))
print(2/3*mean(sample/(sample+0.1)))
integrate(pf1,0,1)

