#crossover:: creates two chidren from two parents
#Inputs: obj1 obj2 - indexes of two points, n -maximum possible index
#Output: vector with two components representing indexes of children
crossover<-function(obj1, obj2, n) {
p<-ceiling(log(n)/log(2));
mid<-floor(p/2);
div<-2^mid;
p11<-obj1 %/% div;
p12<-obj1 %% div;
p21<-obj2 %/% div;
p22<-obj2 %% div;
c((p11*div+p22) %% n ,(p21*div+p12) %% n)
}


#mutate:: reverses randomly two bits in the object
#Inputs: obj - index of a point, n -maximum possible index
#output: mutated index
mutate<-function(obj, n) {
p<-log(n)/log(2);
mut<-floor(runif(2,0,p+1));

for (i in 1:2)
{
	div<-2^mut[i];
	o1<-obj %/% div;
	o2<- obj %% div;
	o11<-o1;
	if (o1 %% 2 !=0) o11<-o11-1 else o11<-o11+1;
	obj<-(o11*div+o2) %% n;
}
obj
}

#getC:: computes grid position from an index
#Input: index - index of a point or vector containing several indexes
#output: matrix. The first column is the shape position, the second column
#        is the scale position on the grid
getC <-function(index) {
sh <- (index-1) %% 10 +1;
sc <- ((index-1) %/% 10 )*0.1+0.1;
cbind(sh,sc)
}