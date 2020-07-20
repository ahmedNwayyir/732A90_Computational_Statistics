t=0
n=100
x1=numeric(n)
x2=numeric(n)
x1[1]=5
x2[1]=6

for (t in 2:n){
  x1[t]=rnorm(1,1+0.5*(x2[t-1]-2), sqrt(0.75))
  x2[t]=rnorm(1,2+0.5*(x1[t]-1),sqrt(0.75))
}

plot(x1,x2, col="red")
plot(x1, type="l", main="Trace plot for x1")
plot(x2, type="l", main="Trace plot for x2")
