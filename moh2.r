setwd('D:\\animesh\\projects\\research\\mohan\\res')

res1 <- read.table("mohan_res_flgp_dm.txt")
res2 <- read.table("mohan_res_adgp_dm.txt")

boxplot(res1)

wilcox.test(res1$flgp,res1$flgp_dm)
t.test(res1$flgp,res1$flgp_dm)


boxplot(res2)

wilcox.test(res2$adgp,res2$adgp_sm_356)
wilcox.test(res2$adgp_sm_356,res2$adgp_dm_356_347)
wilcox.test(res2$adgp,res2$adgp_dm_356_347)
t.test(res2$adgp,res2$adgp_sm_356)
t.test(res2$adgp_sm_356,res2$adgp_dm_356_347)
t.test(res2$adgp,res2$adgp_dm_356_347)

x = res2$adgp
y = res2$adgp_sm_356
z = res2$adgp_dm_356_347
scores = data.frame(x,y,z)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)
