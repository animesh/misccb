setwd('D:\\animesh\\projects\\research\\mohan\\res')

res <- read.table("mohan_res_m.txt")

boxplot(res)

wilcox.test(res$flop_wt,res$dmwtp)
wilcox.test(res$flop_wt,res$dmwtp)

t.test(res$flop_wt,res$dmwtp)
t.test(res$adap_flop,res$dmadp)

t.test(res$flop_wt,res$dmwtp)
t.test(res$adap_flop,res$dmadp)

cor(res$pconc,res$flop_wt)
cor(res$pconc,res$dmwtp)
cor(res$pconc,res$adap_flop)
cor(res$pconc,res$dmadp)

t.test(res$pconc,res$flop_wt)
t.test(res$pconc,res$dmwtp)
t.test(res$pconc,res$adap_flop)
t.test(res$pconc,res$dmadp)


t.test(cphyll$Uninoculated,cphyll$GN25)
t.test(nodule$Uninoculated,nodule$GN25)
t.test(wsf$Uninoculated,wsf$GN25)
t.test(wrf$Uninoculated,wrf$GN25)

t.test(cphyll$GN25,cphyll$GN25pFJ4)
t.test(nodule$GN25,nodule$GN25pFJ4)
t.test(wsf$GN25,wsf$GN25pFJ4)
t.test(wrf$GN25,wrf$GN25pFJ4)

t.test(cphyll$GN25,cphyll$GN25pFJ9)
t.test(nodule$GN25,nodule$GN25pFJ9)
t.test(wsf$GN25,wsf$GN25pFJ9)
t.test(wrf$GN25,wrf$GN25pFJ9)

t.test(cphyll$Uninoculated,cphyll$GN25pFJ4)
t.test(nodule$Uninoculated,nodule$GN25pFJ4)
t.test(wsf$Uninoculated,wsf$GN25pFJ4)
t.test(wrf$Uninoculated,wrf$GN25pFJ4)

t.test(cphyll$Uninoculated,cphyll$GN25pFJ9)
t.test(nodule$Uninoculated,nodule$GN25pFJ9)
t.test(wsf$Uninoculated,wsf$GN25pFJ9)
t.test(wrf$Uninoculated,wrf$GN25pFJ9)

t.test(cphyll$GN25,cphyll$GN25.Um)
t.test(nodule$GN25,nodule$GN25.Um)
t.test(wsf$GN25,wsf$GN25.Um)
t.test(wrf$GN25,wrf$GN25.Um)

t.test(cphyll$GN25pFJ4,cphyll$GN25pFJ4.Um)
t.test(nodule$GN25pFJ4,nodule$GN25pFJ4.Um)
t.test(wsf$GN25pFJ4,wsf$GN25pFJ4.Um)
t.test(wrf$GN25pFJ4,wrf$GN25pFJ4.Um)

t.test(cphyll$GN25pFJ9,cphyll$GN25pFJ9.Um)
t.test(nodule$GN25pFJ9,nodule$GN25pFJ9.Um)
t.test(wsf$GN25pFJ9,wsf$GN25pFJ9.Um)
t.test(wrf$GN25pFJ9,wrf$GN25pFJ9.Um)

t.test(cphyll$Uninoculated,cphyll$Um)
t.test(nodule$Uninoculated,nodule$Um)
t.test(wsf$Uninoculated,wsf$Um)
t.test(wrf$Uninoculated,wrf$Um)


str(cphyll)
summary(cphyll)
