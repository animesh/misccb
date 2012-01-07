setwd('D:\\animesh\\projects\\research\\mohan\\res')

res1 <- read.table("mohan_res_apipflwp.txt")
res2 <- read.table("mohan_res_ap_m_347_356.txt")

t.test(res1$ap,res1$ap_ip)
t.test(res1$ap,res1$flwp)
t.test(res1$flwp,res1$ap_ip)

t.test(res2$ap_wt,res2$ap_m347)
t.test(res2$ap_wt,res2$ap_m356)
t.test(res2$ap_wt,res2$ap_m347m356)
