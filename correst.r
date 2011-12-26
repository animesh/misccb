a=read.table('CatG9.aln.aln2hydro.txt')

b <- cor(a, method="pearson")
write.table( b , file = "CatG9.aln.aln2hydro.txt.cor.txt", sep = "\t",col.names  = FALSE, row.names = FALSE )

