pf<-function(x) exp(-x+5*log(x));

x<-c();
x[1]<-3;
r<-0;
y<-0;
for(i in 1:1000) {
 y<-rlnorm(1,x[i],1);
 r<-min(1,pf(y)*dlnorm(x[i],y,1)/(pf(x[i])*dlnorm(y,x[i],1)));
 if (u<=r) x[i+1]<-y else x[i+1]<-x[i];
}
plot(x, type="l")
print(mean(x)) 

pf<-function(x) exp(-x+5*log(x));

x<-c();
x[1]<-1;
r<-0;
y<-0;
for(i in 1:2000) {
 y<-rchisq(1,floor(x[i]+1));
 r<-min(1,pf(y)*dchisq(x[i],floor(y+1))/(pf(x[i])*dchisq(y,floor(x[i]+1))));
 if (u<=r) x[i+1]<-y else x[i+1]<-x[i];
}
plot(x, type="l")
print(mean(x[1000:2000]))
 