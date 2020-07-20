setwd("Z:/732A38/16/labs/5")
data1=read.csv2("lottery.csv")
data1$Day=c()
data1$Month=c()
data1$Mo.Number=c()

plot(data1$Day_of_year,data1$Draft_No)
m1=loess(Draft_No~Day_of_year,data=data1)
points(data1$Day_of_year, predict(m1), col="blue", type="l")

#bootstrap test
library(boot)

fT=function(X,Y){
  n=length(X)
  ind=order(Y)
  return((Y[ind[n]]-Y[ind[1]])/(X[ind[n]]-X[ind[1]]))
}

stat1<-function(data,ind){
  data2=data[ind,];
  m1=loess(Draft_No~Day_of_year,data=data2)
  Yp=predict(m1)
  ret=fT(data2$Day_of_year, Yp)
}
res=boot(data1,stat1,R=2000)
hist(res$t,50)

#p-value is 0.0025
mean(res$t>0)
#
perm=function(data,B){
  stat=numeric(B)
  n=dim(data)[1]
  for(b in 1:B){
    X=sample(data$Day_of_year, n)
    m1=loess(data$Draft_No~X)
    stat[b]=fT(X,predict(m1))
  }
  return(stat)

}

res=perm(data1,200)
hist(res,50)

stat0=fT(data1$Day_of_year, predict(m1))

mean(abs(res)>abs(stat0))

n=dim(data1)[1]
stat1=numeric(100)
alpha=0
reject=0
fun= function(x) max(0,min(366,x))
for (i in 1:100){
  alpha=alpha+0.01
  Yb=rnorm(n, alpha*data1$Day_of_year+183,10)
  Y=sapply(Yb, FUN=fun)
  data3=data.frame(Day_of_year=data1$Day_of_year, Draft_No=Y)
  res=perm(data3,200)
  m1=loess(Draft_No~Day_of_year,data=data3)
  stat0=fT(data3$Day_of_year, predict(m1))
  pv=mean(abs(res)>abs(stat0))
  if(pv<0.05) reject=reject+1
}

reject/100



?loess.as
#assignment 2
library(boot);
data <- read.csv("prices1.csv",sep=";"); 
Price <- data$Price;
SqFt <- data$SqFt;
Feats <-data$FEATS;
Taxes <-data$Taxes;

hist(Price,20);
mymean<-function(set, indices) mean(set[indices]);
bootres<-boot(Price,mymean,1000)
plot.boot(bootres)
#bias correction
mycorr <-2*bootres$t0-mean(bootres$t);
#variance estimate
myvar<-var(bootres$t);
myci <-boot.ci(bootres);

#envelope

mymean1<-function(set,indices) {
	x<-set$SqFt[indices];
	y<-set$Price[indices];
	data1<-data.frame(x,y);
	data2<-data.frame(x=set$SqFt, y=set$Price);
	fit<-glm(y~x,data=data1);
	prediction<-as.vector(predict(fit, data2));
}

bootres<-boot(data,mymean1,1000);
env<-envelope(bootres);
ci<-env$point;
plot(SqFt, Price,type="n");
ii=order(SqFt)
points(SqFt, Price,type="p");
points(SqFt, mymean1(data,1:110),type="l",col="red");
points(SqFt[ii],ci[1,ii],type="l",col="blue");
points(SqFt[ii],ci[2,ii],type="l",col="blue");
	

#jack

r<-length(Price);
T<-rep(0,r);
for (i in 1:r) {
	T[i]=r*mean(Price)-(r-1)*mean(Price[-i]);
}
JT=mean(T);
varJack=sum((T-JT)^2)/(r*(r-1))





	
	


