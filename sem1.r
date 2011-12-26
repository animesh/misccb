#sem1.r --no-save < sem1.r > sem1.log
#getwd()
#source("sem1.r")
library(sem)
data(Klein)
Klein$P.lag <- c(NA, Klein$P[-22])
Klein$X.lag <- c(NA, Klein$X[-22])
Klein
Klein.eqn1 <- tsls(C ~ P + P.lag + I(Wp + Wg), instruments=~G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=Klein)
Klein.eqn2 <- tsls(I ~ P + P.lag + K.lag, instruments=~G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=Klein)
Klein.eqn3 <- tsls(Wp ~ X + X.lag + I(Year - 1931), instruments=~G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=Klein)
Klein.eqn1

