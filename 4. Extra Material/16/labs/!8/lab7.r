myfun<-function(sh,sc) sum(dgamma(x, sh, scale=sc, log=TRUE));

MM=-Inf;
sh0<-0;
sc0<-0;
for (sh in 1:20) {
for (sc in seq(0.1,10, by=0.1)) {
curr<- myfun(sh,sc);
if(curr>MM) {
MM<-curr;
sh0<-sh;
sc0<-sc;
}
}
}






y<-c()
z<-seq(1,15,by=0.1);
n<-length(z);
for(i in 1:n)
{
y[i]<-myfun(4,z[i]);
}
plot(z,y)


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

getSample4<-function(values) {
n<-length(values);
s<-sample(1:n,4);
o<-order(values[s]);
s<-s[o];
}






#genetic algorithm

mygen<- function(numiter, mutperc) {
X <-sample(1:1000,10);
XC<-getC(X);
plot(1:10,1:10,type="n");
points(XC[,1],XC[,2],col="black");
#X=1:10;
fun<-c();
for (i in 1:10) {
	coord=getC(X[i]);
	fun[i]<-myfun(coord[1], coord[2]);
}
	
objective<-numeric(numiter);

for (i in 1:numiter) {
	f1<-fun;
	# print(sort(fun));
	# s4<-getSample4(fun);
	# print(s4);
	# parent1<-s4[3];
	# parent2<-s4[4];
	# min1i<-s4[1];
	# min2i<-s4[2];	
	o<-order(f1);
	s<-sample(1:10,2);
	parent1<-s[1];
	parent2<-s[2];
	min1i<-o[1];
	min2i<-o[2];

	K1<-X[parent1];
	K2<-X[parent2];
	K<-crossover(K1,K2, 1000);
	p<-runif(1);
	
	if (p<mutperc) {
		K[1]<-mutate(K[1],1000);
	    K[2]<-mutate(K[2],1000);
	}
	X[min1i]<-K[1];
	X[min2i]<-K[2];
	coord1<-getC(K[1]);
	coord2<-getC(K[2]);
	fun[min1i]<-myfun(coord1[1], coord1[2]);
	fun[min2i]<-myfun(coord2[1], coord2[2]);
	objective[i]<-max(fun);
}
	
XC<-getC(X);
points(XC[,1],XC[,2],col="red",cex = 1.5);	
objective
#plot(objective);
}










