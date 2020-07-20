#u1
data=read.csv2("mortality_rate.csv")
data$LMR=log(data$Rate)

n=dim(data)[1]
set.seed(123456) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,] 

myMSE=function(lambda,pars){
  res=loess(pars$Y~pars$X, enp.target=lambda)
  Yp=predict(res,newdata=pars$Xtest)
  MSE=mean((pars$Ytest-Yp)^2)
  print(MSE)
  return(MSE)
}

MSEs=numeric(400)
st=0
for (i in 1:400) {
  st=st+0.1
  MSEs[i]=myMSE(st, pars=list(X=train$Day, Y=train$LMR, Xtest=test$Day, Ytest=test$LMR))
}

plot(seq(0.1,by=0.1, to=40), MSEs, type="l")

min(MSEs)
which.min(MSEs)

optimize(myMSE, interval=c(0.1,40),pars=list(X=train$Day, Y=train$LMR, Xtest=test$Day, Ytest=test$LMR), tol=0.01)
optim(30, myMSE, pars=list(X=train$Day, Y=train$LMR, Xtest=test$Day, Ytest=test$LMR), method="BFGS")




#u2
X<-data;
n<-length(X);

mu0=mean(data)
sigma0=sqrt(var(data))

#minus loglik
myfun <- function(pars) {
mu <-pars[1];
sigma <-pars[2];

result<- n*log(sqrt(2*pi)*sigma)+sum((X-mu)^2)/(2*sigma^2);
}

#gradient minusloglik
mygrad <- function(pars) {
mu <-pars[1];
sigma <-pars[2]

result1<- -sum(X-mu)/(sigma^2);
result2<- n/sigma - sum((X-mu)^2)/(sigma^3);

c(result1,result2)
}

optim(c(0,1), myfun, NULL, method = "CG")
optim(c(0,1), myfun, gr=mygrad, method = "CG")
optim(c(0,1), myfun, NULL, method = "BFGS")
optim(c(0,1), myfun, gr=mygrad, method = "BFGS")

