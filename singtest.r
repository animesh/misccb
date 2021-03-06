options(na.action=na.exclude) # preserve missings
options(contrasts=c('contr.treatment', 'contr.poly')) #ensure constrast type
library(survival)

#
# A simple test of the singular=ok option
#
test1 <- data.frame(time=  c(4, 3,1,1,2,2,3),
		    status=c(1,NA,1,0,1,1,0),
		    x=     c(0, 2,1,1,1,0,0))

temp <- rep(0:3, rep(7,4))

stest <- data.frame(start  = 10*temp,
		    stop   = 10*temp + test1$time,
		    status = rep(test1$status,4),
		    x      = c(test1$x+ 1:7, rep(test1$x,3)),
		    epoch  = rep(1:4, rep(7,4)))

fit1 <- coxph(Surv(start, stop, status) ~ x * factor(epoch), stest)
fit1$coef    # elements 2:4 should be NA
