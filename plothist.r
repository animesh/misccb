NEWBLER = read.table('NEWBLER.length')
MIRA = read.table('MIRA.length')
PHRAP = read.table('PHRAP.length')
CAP3 = read.table('CAP3.length')
par(mfrow = c(2,2))
hist(MIRA$V1, xlab = "Contig Length", col="blue", border="grey", main = "MIRA", br=c(0,100,500,1000,2000,3000,5000,10000,100000,170000),freq = TRUE)
hist(PHRAP$V1, xlab = "Contig Length", col="red", border="grey", main = "PHRAP", br=c(0,100,500,1000,2000,3000,5000,10000,100000,170000),freq = TRUE)
hist(CAP3$V1, xlab = "Contig Length", col="green", border="grey", main = "CAP3", br=c(0,100,500,1000,2000,3000,5000,10000,100000,170000),freq = TRUE)
hist(NEWBLER$V1, xlab = "Contig Length", col="black", border="grey", main = "NEWBLER", br=c(0,100,500,1000,2000,3000,5000,10000,100000,170000),freq = TRUE)



truehist(NEWBLER$V1, nbins = "Scott", h, x0 = -h/1000,breaks, prob = TRUE, xlim = range(breaks),ymax = max(est), col,xlab = deparse(substitute(data)), bty = "n", ...)

