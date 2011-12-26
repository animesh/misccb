NEWBLER = read.table('c10x454.prop')
hist(NEWBLER$V2, xlab = "Contig Length", col="blue", border="black", main = "N10x", breaks=1000,xlim=c(0,10000))

