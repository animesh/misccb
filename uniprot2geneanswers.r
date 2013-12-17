data=read.csv("C:/Users/animeshs/Downloads/SHBER.csv")
attach(data)
library(org.Hs.eg.db)
egmap <- revmap(org.Hs.egUNIPROT)[Entry]
library('GeneAnswers')
x <- geneAnswersBuilder(toTable(egmap), 'org.Hs.eg.db', categoryType='GO.BP', testType='hyperG',  pvalueT=0.1, FDR.correction=TRUE, geneExpressionProfile=data)
geneAnswersReadable(x)
