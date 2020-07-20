invcdf=function(a,n){
  U=runif(n)
  return(log(U*(a-1)+1)/log(a))
}

S=invcdf(0.1,1000)
hist(S,30, freq=F)

n=1000
X=numeric(n)
Y=numeric(n)
#X[1]=0.1
#Y[1]=0.1

X[1]=0.5
Y[1]=0.9

for (i in 1:n){
  X[i+1]=rbeta(1,Y[i],4)
  Y[i+1]=invcdf(X[i+1],1)
}
plot(X, type="l")
plot(Y, type="l")

Z=X^2+Y^2

mean(Z)

var(Z)/1000


#u2
set.seed(123456)
X=rchisq(100,240)

write.csv(X, "X.csv", row.names=F)

X=read.csv("X.csv")$x
fitness=function(X,a){
  return(-1/sum(log(dchisq(X,a))))
}

crossover=function(X,Y){
  return(list(ch1=floor((2*X+Y)/3), ch2=floor((X+Y*2)/3)))
}


mutation=function(X){
  return((X*2559+107)%%311)
}

genetic=function(X,maxiter,mutprob){
Y=seq(1,50,1)
for (i in 1:maxiter){
  Fit=numeric(50)
  for(j in 1:50) Fit[j]=fitness(X,Y[j])
  P=Fit/sum(Fit)
  ind=sample(1:50, 20,replace=T,prob=P)
  Yn=numeric(50)
  index=0
  for(j in 1:25){
    p1=sample(ind,1)
    p2=sample(ind,1)
    res=crossover(Y[p1], Y[p2])
    index=index+1
    Yn[index]=ifelse(fitness(Y[p1],X)>fitness(res$ch1,X), Y[p1], res$ch1)
    index=index+1
    Yn[index]= ifelse(fitness(Y[p2],X)>fitness(res$ch2,X), Y[p2], res$ch2)
    if(runif(1)<mutprob) Yn[index]=mutation(Yn[index])
  }
  Y=Yn
}
return(list(Y=Y, fit=Y[order(-Fit)[1]], out=max(Fit) ))
}

genetic(X,100,0.9)
library(boot)
stat=function(data, index){
  print("p")
  X1=data[index]
  res=genetic(X1,100,0.2)
  return(res$fit)
}
bt=boot(X,stat,100)
mean(bt$t<200)

