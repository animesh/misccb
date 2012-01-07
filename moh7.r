setwd('D:\\animesh\\projects\\research\\mohan\\res')

res1 <- read.table("res244219.txt")

t.test(res1$WT,res1$WT244, paired=TRUE)
t.test(res1$WT,res1$WT297, paired=TRUE)
t.test(res1$WT,res1$WTY219F, paired=TRUE)
t.test(res1$WT,res1$WTY244A, paired=TRUE)


wt = res1$WT
wt347 = res1$WT_347
wt356 = res1$WT_356
dm = res1$WT_347_356
scores = data.frame(wt,wt347,wt356,dm)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)

res2 <- read.table("res244219_wnc.txt")
boxplot(res2)

t.test(res1$WT,res1$WT244)
t.test(res1$WT,res1$WT297)
t.test(res1$WT,res1$WTY219F)
t.test(res1$WT,res1$WTY244A)

