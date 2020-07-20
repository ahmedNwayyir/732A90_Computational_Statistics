laplace<-function(u){
  result<-0;
  if(u<0.5){
    result<-log(2*u);
  }else {
    result<- -2*log(2*(1-u));
  }
  result;
};

#Acceptance rejection
c<-1.5;

mynorm<-function(n)
{
  k<-1;
  c<-1.31;
  result<-1:n;
  rejection <-1:n;
  rejection[]<-0;
  while(k<n)
  {
    u1<-runif(1);
    y<-laplace(u1);
    u<-runif(1);
    if(u <=dnorm(y)/(c*0.5*exp(-abs(y))) ){
      k<-k+1;
      result[k]<-y;
    } else {
      rejection[k]<-rejection[k]+1;
    }
  }
  list(result,rejection)
}

res<-mynorm(1000);
hist(res[[1]],100);
#hist(rnorm(1000),100);

mean(res[[2]])
