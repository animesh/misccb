#source("http://bioconductor.org/biocLite.R")
#biocLite("GeneAnswers")
#biocLite("org.Hs.eg.db")
#biocLite("GO.db")
#biocLite("biomaRt")
#rm(list = ls())
data=read.csv("L:/Elite/gaute/test/SHBER.csv")
gene=as.character(data[,1])
data=data[,-1]
prco=prcomp(data)
sco <- prco$x
loa <- prco$rotation
plot(sco[,1], sco[,2], col=row.names(data))
plot(loa[,1], loa[,2], col=row.names(data))
     

library(org.Hs.eg.db)
egmap <- revmap(org.Hs.egUNIPROT)[as.character(gene)]

x <- org.Hs.egGO
mapped_genes <- mappedkeys(x)
xx <- as.list(x[mapped_genes])
goids <- xx[2:3]
names(goids[[1]])


