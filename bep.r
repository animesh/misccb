pdf("bac-end-plot.pdf");
t=read.table('t2',sep='\t')
t2=read.table('t',sep='\t')
t4=read.table('t4')
plot(density(abs(t4$V11)),col="green",xlab="Distance in bp",main="BAC-end mapping",ylab="Density of Pairs",xlim=range(10000,200000))
lines(density(abs(t2$V3)),col="red")
legend(x="topright", legend=c("Celera","Newbler","CeleraUsed"),text.col=c("blue","red","green"))
dens=(density(abs(t4$V3)))
peak=dens$x[which(dens$y==max(dens$y))]  
abline(v=peak,lty=6,col=gray(0.4))
text(peak,0,labels=round(peak),cex=.6,adj = c(0,0))
dens=(density(abs(t2$V3)))
peak=dens$x[which(dens$y==max(dens$y))]  
abline(v=peak,lty=6,col=gray(0.4))
text(peak,0,labels=round(peak),cex=.6,adj = c(1,1))
lines(density(abs(t$V3)),col="blue")
dens=(density(abs(t$V3)))
peak=dens$x[which(dens$y==max(dens$y))]
abline(v=peak,lty=6,col=gray(0.4))
text(peak,0,labels=round(peak),cex=.6,adj = c(2,2))

