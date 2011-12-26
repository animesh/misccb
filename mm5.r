resap <- read.table("ap.txt")
resfl <- read.table("fl.txt")
resm347 <- read.table("m347.txt")
resm356 <- read.table("m356.txt")
resdm <- read.table("dm.txt")
resapc3 <- resap[-c(3),]
resflc3 <- resfl[-c(3),]


resap$Pbtrans <- resap$Pf/resap$Pb
lmfit <- lm(Pbtrans~Pf, data=resap, na.action=na.omit)
plot(resap$Pf, resap$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resap, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resap$Pf, resap$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")






resapc3$Pbtrans <- resapc3$Pf/resapc3$Pb
lmfit <- lm(Pbtrans~Pf, data=resapc3, na.action=na.omit)
plot(resapc3$Pf, resapc3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resapc3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resapc3$Pf, resapc3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")



resfl$Pbtrans <- resfl$Pf/resfl$Pb
lmfit <- lm(Pbtrans~Pf, data=resfl, na.action=na.omit)
plot(resfl$Pf, resfl$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resfl, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resfl$Pf, resfl$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")




resflc3$Pbtrans <- resflc3$Pf/resflc3$Pb
lmfit <- lm(Pbtrans~Pf, data=resflc3, na.action=na.omit)
plot(resflc3$Pf, resflc3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resflc3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resflc3$Pf, resflc3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")




resm347$Pbtrans <- resm347$Pf/resm347$Pb
lmfit <- lm(Pbtrans~Pf, data=resm347, na.action=na.omit)
plot(resm347$Pf, resm347$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resm347, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resm347$Pf, resm347$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")





resm356$Pbtrans <- resm356$Pf/resm356$Pb
lmfit <- lm(Pbtrans~Pf, data=resm356, na.action=na.omit)
plot(resm356$Pf, resm356$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resm356, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resm356$Pf, resm356$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")



resdm$Pbtrans <- resdm$Pf/resdm$Pb
lmfit <- lm(Pbtrans~Pf, data=resdm, na.action=na.omit)
plot(resdm$Pf, resdm$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=resdm, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(resdm$Pf, resdm$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(conc=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")








t.test((resap$Pb/(resap$Pb+resap$Pf)),(resfl$Pb/(resfl$Pb+resfl$Pf)))
t.test((resap$Pb/(resap$Pb+resap$Pf)),(resm347$Pb/(resm347$Pb+resm347$Pf)))
t.test((resap$Pb/(resap$Pb+resap$Pf)),(resm356$Pb/(resm356$Pb+resm356$Pf)))
t.test((resap$Pb/(resap$Pb+resap$Pf)),(resdm$Pb/(resdm$Pb+resdm$Pf)))

t.test((resfl$Pb/(resfl$Pb+resfl$Pf)),(resm347$Pb/(resm347$Pb+resm347$Pf)))
t.test((resfl$Pb/(resfl$Pb+resfl$Pf)),(resm356$Pb/(resm356$Pb+resm356$Pf)))
t.test((resfl$Pb/(resfl$Pb+resfl$Pf)),(resdm$Pb/(resdm$Pb+resdm$Pf)))

t.test((resm347$Pb/(resm347$Pb+resm347$Pf)),(resm356$Pb/(resm356$Pb+resm356$Pf)))
t.test((resm347$Pb/(resm347$Pb+resm347$Pf)),(resdm$Pb/(resdm$Pb+resdm$Pf)))

t.test((resm356$Pb/(resm356$Pb+resm356$Pf)),(resdm$Pb/(resdm$Pb+resdm$Pf)))




names(lmfit)
summary(lmfit)
plot(lmfit)
class(lmfit)
coef(lmfit)



p = res1$WT
q = res1$WT_347
r = res1$WT_356
s = res1$WT_347_356
scores = data.frame(p,q,r,s)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)
