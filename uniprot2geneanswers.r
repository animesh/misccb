#source("http://bioconductor.org/biocLite.R")
#biocLite("gage")
#biocLite("pathview")
#biocLite("GeneAnswers")
#biocLite("org.Hs.eg.db")
#biocLite("GO.db")
#biocLite("biomaRt")
#rm(list = ls())

## pathview 
library(gage)
hda=readExpData("L:/Elite/gaute/test/SHBER.txt",row.names=1)
library(pathview)
pathview(hda,pathway.id="hsa03410",gene.idtype="UNIPROT")
hda=as.matrix(hda)
hda.d=hda[,1:3]-hda[,4:6]
pathview(hda.d,pathway.id="hsa03410",gene.idtype="UNIPROT", limit = list(gene = 5, cpd = 1), out.suffix="fcnn")
pv.out <- pathview(hda.d,pathway.id="hsa03410",gene.idtype="UNIPROT", limit = list(gene = 5, cpd = 1), out.suffix="fcnn", kegg.native = TRUE)
str(pv.out)
head(pv.out$plot.data.gene)

dev.off() 

## MS to SNP

source("http://bioconductor.org/biocLite.R")
biocLite("sapFinder")
library("sapFinder")
browseVignettes("sapFinder")
vcf <- system.file("extdata/sapFinder_test.vcf",
                   package="sapFinder")
annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
                          package="sapFinder")
refseq <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
                      package="sapFinder")
outdir <- "db_dir"
prefix <- "sapFinder_test"
db.files <- dbCreator(vcf=vcf, annotation=annotation,
                      refseq=refseq, outdir=outdir,prefix=prefix)


#DEMO

#load data

#KEGG view: gene data only
i <- 1
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id =
                     demo.paths$sel.paths[i], species = "hsa", out.suffix = "gse16873",
                   kegg.native = TRUE)
str(pv.out)
head(pv.out$plot.data.gene)
#result PNG file in current directory

#Graphviz view: gene data only
data(gse16873.d)
data(demo.paths)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i], species = "hsa", out.suffix = "gse16873",kegg.native = FALSE, sign.pos = demo.paths$spos[i])
#result PDF file in current directory

png('test.png')
plot(sin(seq(-10,10,0.1)),type = "l", cex = .1, col = "dark red")
dev.off()

?plot

# from Luo
hda=as.matrix(hda)
hda.d=hda[,1:3]-hda[,4:6]
pathview(hda.d,pathway.id="hsa03410",gene.idtype="UNIPROT", limit = list(gene = 5, cpd = 1), out.suffix="fc")

## PCA
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

mod <- prcomp(E)

## scores (sample space)
scores <- mod$x
## loadings (gene space)
loads <- mod$rotation

## gene symbols
sym <- as.character(mget(colnames(E), illuminaHumanv3SYMBOL))

# plot
pc1 = 1 # ok, checked
pc2 = 2
# score plot (sample space)
x11()
expVar <- 100 *mod$sdev / sum(mod$sdev) 
hist(expVar)
plot(scores[,pc1], scores[,pc2], col=cl, pch=19, main="Score-plot (Sample map)", xlab=paste("PC:", expVar[pc1]), ylab=paste("PC:", expVar[pc2]))
text(scores[,pc1], scores[,pc2], rownames(E), pos=3)


# loadings (gene space)
plot.factor.name <- "lrT - T"
col <- factor(a[,plot.factor.name])
plot(loads[,pc1], loads[,pc2], col=col, pch='.', main="Gene weights", xlab=paste("PC:", expVar[pc1]), ylab=paste("PC:", expVar[pc2]))
text(loads[,pc1], loads[,pc2], sym, pos=3, cex=0.4)



# extract a quad 
kk <- loads[,pc1] > 0 & loads[,pc2] > 0
plot(kk)


# GO mapping
x <- org.Hs.egGO
mapped_genes <- mappedkeys(x)
xx <- as.list(x[mapped_genes])
goids <- xx[2:3]
names(goids[[1]])


