jpeg("depth.jpg")
tech20map=read.table('ecoltech20pa.txt.depth.txt')
plot(density(tech20map$V2),col="black",xlab="Coverage",main="gsMapped Reads",ylab="Density",xlim=c(0,200))
allmap=read.table('allmapread.depth.txt')
lines(density(allmap$V2),col="red")
srmap=read.table('srmapread.depth.txt')
lines(density(srmap$V2),col="blue")
rnd75x=read.table('ecol750rndpa.txt.depth.txt')
lines(density(rnd75x$V2),col="green")
mlmap=read.table('mlmapread.depth.txt')
lines(density(mlmap$V2),col="orange")
legend(x="topright", legend=c("GS20","Titanium","Qual","Rand75X","ML"),text.col=c("black","red","blue","green",'orange'))
