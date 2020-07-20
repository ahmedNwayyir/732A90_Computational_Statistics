X=rnorm(10,5,1)

s=var(X)
M=10000
n=10
t=numeric(M)
for (i in 1:M) {
  Y=rnorm(10,4,s)
  t[i]=(mean(Y)-4)/(sd(Y)/sqrt(10))
}
hist(t,50)

mouse=read.csv2("mouse.csv")
heatmap(as.matrix(mouse1), Rowv=NA, Colv=NA, scale="column", labCol=NA)
heatmap(cbind(sample(mouse1$Group,16),mouse$Value),Rowv=NA, Colv=NA, scale="column", labCol=NA)


B=1000
stat=numeric(B)
n=dim(mouse)[1]
for(b in 1:B){
  Gb=sample(mouse$Group, n)
  stat[b]=mean(mouse$Value[Gb=='z'])-mean(mean(mouse$Value[Gb=='y']))
}
hist(stat,50)

stat0=mean(mouse$Value[mouse$Group=='z'])-mean(mean(mouse$Value[mouse$Group=='y']))

mean(stat>stat0)
