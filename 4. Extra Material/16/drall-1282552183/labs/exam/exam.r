#assignment 1

dat<-read.csv("wind.csv",sep=";");
x<-dat$Otago;
hist(x);

myfun<-function(par,dat) {
	sh<-par[1];
	sc<-par[2];
	sum(-dweibull(dat,sh,sc,log=TRUE));
}

myopt<-function(d) {
	res<-optim(c(5,5),myfun,gr=NULL,dat=d,method="BFGS");
	res$par
}

print(myopt(x));

myopt1<-function(d, index) {
	res<-optim(c(5,5),myfun,gr=NULL,dat=d[index],method="BFGS");
	res$par[1]
}

b<-boot(x,myopt1,100);
plot(b);
print(boot.ci(b, type="perc"));


#assignment2

mygen<-function(n) {
	acc<-0;
	rej<-0;
	result<-numeric(n);
	while(acc<n) {
		y<-runif(1);
		u<-runif(1);
		if(u<sin(pi*y)) {
			acc<-acc+1;
			result[acc]<-y;
		}else {
			rej<-rej+1;
		}
	}
	
	print(rej/(acc+rej));
	result
}

sam<-mygen(1000);
hist(sam);
ER<-(pi/2-1)/(pi/2);

res<- mean(2/(pi*(5+sam*sam)));

f<-function(x) sin(pi*x)/(5+x*x);
res1<-integrate(f,0,1);
print(res1);



	

			

