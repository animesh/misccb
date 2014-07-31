setwd('D:\\animesh\\projects\\research\\mohan\\res')

res1 <- read.table("WT347356DM.txt")

t.test(res1$WT,res1$WT_347, paired=TRUE)
t.test(res1$WT,res1$WT_356, paired=TRUE)
t.test(res1$WT,res1$WT_347_356, paired=TRUE)
t.test(res1$WT_347,res1$WT_356, paired=TRUE)
t.test(res1$WT_347,res1$WT_347_356, paired=TRUE)
t.test(res1$WT_356,res1$WT_347_356, paired=TRUE)


wt = res1$WT
wt347 = res1$WT_347
wt356 = res1$WT_356
dm = res1$WT_347_356
scores = data.frame(wt,wt347,wt356,dm)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)
