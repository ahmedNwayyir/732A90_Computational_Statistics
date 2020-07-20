mynorm<- function(e=0){
x<-rnorm(2);
mu<-c(1,2);
sigma <-matrix(c(4,3,3,9),2,2);
A<-chol(sigma);
Y<-mu+t(A)%*%x;
Y
}

X1<-c();
X2<-c();
for (i in 1:1000) {
	res<-mynorm();
	X1[i]=res[1];
	X2[i]=res[2];
}



myframe<-read.csv("tecator.csv",sep=";");
mydata<-as.matrix(myframe);
X0<-mydata[, c(-1, -103)]
X<-cbind(1,X0);
Y<-mydata[,103];
A<-t(X)%*%X;
b<-t(X)%*%Y;
qrres<-qr(X);
R<-qr.R(qrres);
Q<-qr.Q(qrres);
b1<-t(Q)%*%Y;
beta<-backsolve(R,b1);

hist(A%*%beta-b,20);

myfunction <- function(x) {
0.5*t(x)%*%A%*%x-t(x)%*%b
}
x<-1:103;
x[]<-0;
 res2<-optim(x,myfunction, method="CG", control=list(maxit=1000));
 beta2<-res2$par;
 hist(A%*%beta2-b,20);