setwd('D:\\animesh\\projects\\research\\mohan\\res')

res1 <- read.table("WT347356.txt")

t.test(res1$WT,res1$WT_347)
t.test(res1$WT,res1$WT_356)
t.test(res1$WT,res1$WT_347_356)
t.test(res1$WT_347,res1$WT_356)
t.test(res1$WT_347,res1$WT_347_356)
t.test(res1$WT_356,res1$WT_347_356)


p = res1$WT
q = res1$WT_347
r = res1$WT_356
s = res1$WT_347_356
scores = data.frame(p,q,r,s)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)
