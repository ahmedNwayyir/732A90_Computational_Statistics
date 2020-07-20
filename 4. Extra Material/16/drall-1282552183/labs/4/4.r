data <- read.csv("prices1.csv",sep=";"); 
Price <- data$Price;
SqFt <- data$SqFt;
Feats <-data$FEATS;
Taxes <-data$Taxes;
plot(SqFt, Price);
plot(Feats, Price);
plot(Taxes, Price);
fit <-glm(Price~SqFt, data=data);
cv.err1<-cv.glm(data, fit, K=110)
fit <-glm(Price~FEATS, data=data);
cv.err2<-cv.glm(data, fit, K=110)
fit <-glm(Price~Taxes, data=data);
cv.err3<-cv.glm(data, fit, K=110)
print(cv.err1$delta);
print(cv.err2$delta);
print(cv.err3$delta);

fit <-glm(Price~SqFt, data=data);
cv.err1<-cv.glm(data, fit, K=11)
fit <-glm(Price~FEATS, data=data);
cv.err2<-cv.glm(data, fit, K=11)
fit <-glm(Price~Taxes, data=data);
cv.err3<-cv.glm(data, fit, K=11)
print(cv.err1$delta);
print(cv.err2$delta);
print(cv.err3$delta);

#assignment 2
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
	




	
	


