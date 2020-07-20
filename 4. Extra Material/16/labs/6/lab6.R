f<-function(x){
(x^2)/exp(x) - 2*exp(-9*sin(x)/(x^2+ x +1))
}

crossover<-function(x1,x2){
(x1+x2)/2
}

mutate<- function(x) {
x^2%%30;
}

x=seq(0,30, by=0.01);
y=f(x);
plot(x,y, type='l');

genetic<-function(maxiter, mutprob){
  x=seq(0,30, by=0.01);
  y=f(x);
  plot(x,y, type='l');  

mypoints <-seq(0,30,by=5);
Values<-f(mypoints);
#points(mypoints, Values, type="p")


for (i in 1:maxiter) {

sam<-sample(mypoints,2);
ord<-order(Values);
kid<-crossover(sam[1],sam[2]);
u<-runif(1,0,1);
if (u<0.4) {
kid<-mutate(kid)
};
mypoints[ord[1]]<-kid;
Values[ord[1]]<-f(kid);
}
points(mypoints, Values, col=1:7);

}

genetic(10,0.1)



#data generation
set.seed(123456)
X=seq(1.1,11,0.1)
Y=rexp(100, X/10)
Z=rexp(100, X/20)
Z[c(22,23,66,67,68,69, 90,91)]=NA
data=data.frame(X=X,Y=Y,Z=Z)
write.csv(data, "physical.csv", row.names=F)

data=read.csv("physical1.csv")
attach(data)
plot(X,Z, type="l", col="red")
points(X,Y, type="l")


lambda=100
diff=Inf
o=which(!is.na(Z))
u=which(is.na(Z))
n=length(X)

while(diff>0.001){
  lambdanew=(sum(X*Y)+0.5*sum(X[o]*Z[o])+length(u)*lambda)/(2*n)
  diff=abs(lambda-lambdanew)
  lambda=lambdanew
  print(lambda)
}
print(lambda)

points(X, lambda/X,type="l", col="blue")
points(X, 2*lambda/X,type="l", col="blue")

