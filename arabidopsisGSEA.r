library(org.Hs.eg.db)
library('GeneAnswers')
data=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5.txt")
data
data=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5filtered.txt")
egmap <- revmap(org.Hs.egUNIPROT)[as.character(data$Entry)]
egmap <- revmap(org.Hs.egUNIPROT)[as.character(data$Entry)]library('GeneAnswers')
x <- geneAnswersBuilder(toTable(egmap), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(toTable(data$Entry), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
toTable(data$Entry)
(data$Entry)
as.vector(data$Entry)
gl=as.vector(data$Entry)
x <- geneAnswersBuilder(gl, 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(gl, 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=gl)
x <- geneAnswersBuilder(toTable(egmap), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(gl, 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
biocl
l
lsource("http://bioconductor.org/biocLite.R")
source("http://bioconductor.org/biocLite.R")
biocLite("org.At.tair.db")
data=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5GN.txt")
egmap <- as.character(data$Entry)
egmap
x <- geneAnswersBuilder(toTable(egmap), 'in', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(toTable(egmap), 'in', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(toTable(egmap), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(egmap, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(toTable(egmap), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(data, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(egmap, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
toTable(egmap)
library(org.Hs.eg.db)
library(org.Hs.eg.db)
library(org.At.tair.db)
toTable(egmap)
x <- geneAnswersBuilder(egmap, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(egmap, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
x <- geneAnswersBuilder(egmap, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
data$Entry
as.vector(data$Entry)
x <- geneAnswersBuilder(as.vector(data$Entry), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
x <- geneAnswersBuilder(as.vector(data$Entry), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
as.vector(data$Entry)
data=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5.txt")
data
as.vector(data$Entry)
egmap <- revmap(org.Hs.egUNIPROT)[as.character(data$Entry)]
egmap <- revmap(rg.At.tairUNIPROT)[as.character(data$Entry)]
egmap <- revmap(org.At.tairUNIPROT)[as.character(data$Entry)]
data=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5GN.txt")
egmap <- revmap(org.Hs.egENTREZID)[as.character(data$Entry)]
egmap <- revmap(org.At.egENTREZID)[as.character(data$Entry)]
x <- geneAnswersBuilder(data$Entry, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(data$Entry, 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(as.vector(data$Entry), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
x <- geneAnswersBuilder(as.vector(data$Entry), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
biocLite("org.Hs.eg.db")
biocLite("org.At.eg.db")
biocLite("org.At.db")
egmap <- revmap(org.At.tair.dbUNIPROT)[as.character(data$Entry)]
egmap <- revmap(org.At.tairUNIPROT)[as.character(data$Entry)]
egmap <- revmap(org.At.tair)[as.character(data$Entry)]
data=read.csv("L:/Elite/gaute/test/SHBER.csv")
library(org.Hs.eg.db)
egmap <- revmap(org.Hs.egUNIPROT)[as.character(data$Entry)]
egmap
egmap <- revmap(org.At.tairUNIPROT)[as.character(data$Entry)]
data=read.csv("L:/Elite/gaute/test/SHBER.csv")
egmap <- revmap(org.Hs.egUNIPROT)[as.character(data$Entry)]
toTable(egmap)
toTable(egmap)
at=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5GN.txt")
at
x <- geneAnswersBuilder(toTable(egmap), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
atx <- geneAnswersBuilder(toTable(egmap), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
atx <- geneAnswersBuilder(at, 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
atx <- geneAnswersBuilder(at, 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
atx <- geneAnswersBuilder(at, 'org.At.tair', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
atx <- geneAnswersBuilder(at, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
at
atx <- geneAnswersBuilder(at, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
library("GO.db")
atx <- geneAnswersBuilder(at, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
library(AnnotationDbi")
library(AnnotationDbi")
library(AnnotationDbi)
help(package="GO.db")
atx <- geneAnswersBuilder(at, 'org.At.tair.db', categoryType='GO', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
org.At.tair.db
org.Hs.eg.db
atx <- geneAnswersBuilder(at, 'org.At.tair.db', categoryType='GO', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
x <- org.At.tairENTREZID
x
mapped_genes <- mappedkeys(x)
mapped_genes
x <- org.At.tairENTREZID
x
tairENID <- as.list(x[mapped_genes])
tairENID
basicProfile(genelist=tairENID, onto = "MF", level = 2, orgPackage = "org.At.tair.db")
require(goProfiles)
biocLite(goProfiles)
install.packages(goProfiles)
biocLite("goProfiles")
require(goProfiles)
basicProfile(genelist=tairENID, onto = "MF", level = 2, orgPackage = "org.At.tair.db")
basicProfile(genelist=at, onto = "MF", level = 2, orgPackage = "org.At.tair.db")
at
basicProfile(genelist=tairENID, onto = "MF", level = 2, orgPackage = "org.At.tair.db")
basicProfile(genelist=as.character(tairENID), onto = "MF", level = 2, orgPackage = "org.At.tair.db")
basicProfile(genelist=as.character(at), onto = "MF", level = 2, orgPackage = "org.At.tair.db")
basicProfile(genelist=as.character(at), onto = "MF", level = 2, orgPackage = "org.At.tair.d")
basicProfile(genelist=as.character(at), onto = "MF", level = 1, orgPackage = "org.At.tair.db")
basicProfile(genelist=as.character(at), onto = "BP", level = 1, orgPackage = "org.At.tair.db")
basicProfile(genelist=as.character(at$gene_id), onto = "BP", level = 1, orgPackage = "org.At.tair.db")
at$gene_id
as.vector(at$gene_id)
toTable(at$gene_id)
toTable(at)
as.vector(at$gene_id)
as.vector(tairENID)
basicProfile(genelist=as.vector(at$gene_id), onto = "BP", level = 1, orgPackage = "org.At.tair.db")
basicProfile(genelist=(at$gene_id), onto = "BP", level = 1, orgPackage = "org.At.tair.db")
class(at)
class(at$gene_id)
class(as.character(at$gene_id))
(as.character(at$gene_id))
as.character(at$gene_id)
basicProfile(genelist=as.character(at$gene_id), onto = "BP", level = 1, orgPackage = "org.At.tair.db")
gl=as.character(at$gene_id)
gl
class (gl)
basicProfile(genelist=gl, onto = "MF", level = 2, orgPackage = "org.At.tair.db")
basicProfile(genelist=gl orgPackage = "org.At.tair.db")
basicProfile(genelist=gl)
basicProfile(genelist=gl , orgPackage = "org.At.tair.db")
data(prostateIds)
singh.MF <- basicProfile(singh01EntrezIDs, onto = "MF",level = 2, orgPackage = "org.Hs.eg.db")
print(arab.MF)
print(singh.MF)
basicProfile(gl , orgPackage = "org.At.tair.db")
basicProfile(singh01EntrezIDs, onto = "MF",level = 2, orgPackage = "org.Hs.eg.db")
basicProfile(singh01EntrezIDs, onto = "MF",level = 2, orgPackage = "org.At,tair.db")
basicProfile(singh01EntrezIDs, onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile(at, onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile(gl, onto = "MF",level = 2, orgPackage = "org.At.tair.db")
org.At.tair.db"
org.At.tair.db
*
org.At.tair.db
org.At.tair.db"
org.At.tair.db
basicProfile("6389 , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile("6389" , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
print(singh.MF)
singh01EntrezIDs
basicProfile("834435 " , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile("7374 " , onto = "MF",level = 2, orgPackage = "org.Hs.eg.db")
basicProfile("7374" , onto = "MF",level = 2, orgPackage = "org.Hs.eg.db")
basicProfile("834435" , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile("828586 " , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile("828586" , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile("828586" , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile("7374" , onto = "BP",level = 2, orgPackage = "org.Hs.eg.db")
basicProfile("7374" , onto = "BP",level = 1, orgPackage = "org.Hs.eg.db")
basicProfile("7374" , onto = "BP",level = 3, orgPackage = "org.Hs.eg.db")
''
''
basicProfile("828586" , onto = "BP",level = 2, orgPackage = "org.At.tairdb")
basicProfile("828586" , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile("7374" , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile("7374" , onto = "BP",level = 2, orgPackage = "org.Hs.eg.db")
basicProfile("828586" , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile("828587" , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR", 
keytype="ENTREZID")[[2]])
tairIDs <-  as.character(select(org.At.tair.db, keys=gl, cols="TAIR", keytype="ENTREZID")[[2]])
tairIDs <-  as.character(select(org.At.tair.db, keys=gl, colsegIDs <- c("839235", "838362", "838961", "837091", "837455", "837543")="TAIR", keytype="ENTREZID")[[2]])
tairIDs <-  as.character(select(org.At.tair.db, keys=gl, colsegIDs <- c("839235", "838362", "838961", "837091", "837455", "837543")="TAIR", keytype="ENTREZID")[[2]])
egIDs <- c("839235", "838362", "838961", "837091", "837455", "837543")
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR", keytype="ENTREZID")[[2]])
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR", keytype="ENTREZID")[[2]]))
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR", keytype="ENTREZID")[[2]])
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR", keytype="ENTREZID")[[2]])
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR", keytype="ENTREZID"))
tairIDs <-  as.character(select(org.At.tair.db, keys=egIDs, cols="TAIR")
)
egIDs
basicProfile(egIDs , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
at=read.csv("L:/Elite/LARS/2013/desember/ishita/listThr1p5TAIR.txt")
basicProfile(at , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile(at$TAIR , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile(at$TAIR , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
x <- geneAnswersBuilder(at$TAIR, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
x <- geneAnswersBuilder(at$TAIR, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=gl)
basicProfile(at$TAIR , onto = "MF",level = 2, orgPackage = "org.At.tair.db")
basicProfile(at$TAIR , onto = "CC",level = 2, orgPackage = "org.At.tair.db")
basicProfile(at$TAIR , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
x <- geneAnswersBuilder(at$TAIR, 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
at$TAIR
x <- geneAnswersBuilder(as.vector(at$TAIR), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
geneAnswers(x)
geneAnswers(x)
geneAnswersReadable(x)
x <- geneAnswersBuilder(as.vector(at$TAIR), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.01, FDR.correction=TRUE)
geneAnswersReadable(x)
x <- geneAnswersBuilder(as.vector(at$TAIR), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=gl)
geneAnswersReadable(x)
at=read.csv("L:/Elite/LARS/2013/desember/ishita/Min2PepRatio.TAIR.csv")
x <- geneAnswersBuilder(as.vector(at$TAIR), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
geneAnswersReadable(x)
at
head(at)
geneAnswersReadable(x)
x <- geneAnswersBuilder(toTable(as.vector(at$TAIR)), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at)
x <- geneAnswersBuilder((as.vector(at$TAIR)), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=at$Ratio)
geneAnswersReadable(x)
x <- geneAnswersBuilder((as.vector(at$TAIR)), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
geneAnswersReadable(x)
at=read.csv("L:/Elite/LARS/2013/desember/ishita/Min2PepRatioDown.TAIR.csv")
x <- geneAnswersBuilder((as.vector(at$TAIR)), 'org.At.tair.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE)
geneAnswersReadable(x)
basicProfile(at$TAIR , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile(as.vector(at$TAIR) , onto = "BP",level = 2, orgPackage = "org.At.tair.db")
basicProfile(as.vector(at$TAIR) , onto = "CC",level = 2, orgPackage = "org.At.tair.db")
basicProfile(as.vector(at$TAIR) , onto = "MF",level = 2, orgPackage = "org.At.tair.db")

library(org.Hs.eg.db)
gn <- c("XRCC1","UNG")
a <- select(org.Hs.eg.db,keys = as.vector(gn)) ,columns=c("ENTREZID", "SYMBOL","OMIM"),keytype="SYMBOL")
select(org.Hs.eg.db, rep("ADORA2A", 4), "ENTREZID", "ALIAS")
egmap <- revmap(org.Hs.egSYMBOL)[as.character(gn)]
head(toTable(egmap)$gene_id)
basicProfile(as.vector(head(toTable(egmap)$gene_id)), onto = "MF",level = 2, orgPackage = "org.Hs.eg.db")

sc=read.table('M:/SILACrd.txt',header=TRUE)
egmap <- revmap(org.Hs.egSYMBOL)[as.character(sc$Symbol)]
basicProfile(as.vector((toTable(egmap)$gene_id)), onto = "BP",level = 2, orgPackage = "org.Hs.eg.db")
