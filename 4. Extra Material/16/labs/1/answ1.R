#uppgift 1

x1<-1/3;
x2<-1/4;
if (abs(x1-x2-1/12)<.Machine$double.eps){
  print("Teacher said true")
} else{
  print("Teacher lied")}

#uppgift 2

myd<-function(f,x,e) (f(x+e)-f(x))/e
myd(function(x) x, 100000,10^-15)

#uppgift 3

myvar<-function(x) {
  n<-length(x)
 return(1/(n-1)*(sum(x^2)-1/n*(sum(x))^2))
}

X<-rnorm(10000,10^8,1)
Y<-rep(0,10000);
for (i in 1:length(X)) {
  Y[i]<-myvar(X[1:i])-var(X[1:i])
}
plot(Y,type="p")
plot(Y[1:100],type="b")

plot((1:100)*Y[1:100],type="b")

#4

myframe<-read.csv("tecator.csv",sep=";");
mydata<-as.matrix(myframe);
X0<-mydata[, c(-103)]
X<-cbind(1,X0);
Y<-mydata[,103];

A<-t(X)%*%X;
b<-t(X)%*%Y;
solve(A,b)
#Error in solve.default(A, b) : 
# system is computationally singular: reciprocal condition number = 3.29689e-17
kappa(A)

mydata=scale(mydata)
X0<-mydata[, c(-103)]
X<-cbind(1,X0);
Y<-mydata[,103];

A<-t(X)%*%X;
b<-t(X)%*%Y;
solve(A,b)

kappa(A)

