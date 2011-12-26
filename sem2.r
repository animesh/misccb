#R --no-save < sem1.r > sem1.log
#getwd()
#source("sem1.r")
library(sem)
med <- read.table("top5_tr_r.txt")
med
med.eqn1 <- tsls(C ~ P + P.lag + I(Wp + Wg), instruments=~G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=med)
med.eqn2 <- tsls(I ~ P + P.lag + K.lag, instruments=~G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=med)
med.eqn3 <- tsls(Wp ~ X + X.lag + I(Year - 1931), instruments=~G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=med)
