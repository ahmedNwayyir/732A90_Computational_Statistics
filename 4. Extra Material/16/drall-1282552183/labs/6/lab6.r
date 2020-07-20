X<-data;
n<-length(X);

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

result1<- -sum(X-mu)/(2*sigma^2);
result2<- n/sigma - sum((X-mu)^2)/(sigma^3);

c(result1,result2)
}

optim(c(0,1), myfun, NULL, method = "CG")
optim(c(0,1), myfun, gr=mygrad, method = "CG")
optim(c(0,1), myfun, NULL, method = "BFGS")
optim(c(0,1), myfun, gr=mygrad, method = "BFGS")

