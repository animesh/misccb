ap1    <- read.table("ap1.txt")
ap2    <- read.table("ap2.txt")
ap3    <- read.table("ap3.txt")
fl1    <- read.table("fl1.txt")
fl2    <- read.table("fl2.txt")
fl3    <- read.table("fl3.txt")
d347_1 <- read.table("d347_1.txt")
d347_2 <- read.table("d347_2.txt")
d347_3 <- read.table("d347_3.txt")
d356_1 <- read.table("d356_1.txt")
d356_2 <- read.table("d356_2.txt")
d356_3 <- read.table("d356_3.txt")
dm1    <- read.table("dm1.txt")
dm2    <- read.table("dm2.txt")
dm3    <- read.table("dm3.txt")

kd_res    <- read.table("kd_res.txt")
bm_res    <- read.table("bm_res.txt")

KD <- read.table("kdest.txt")
BM <- read.table("bmest.txt")
summary(KD)
summary(BM)

sd(KD$ap_est)
sd(KD$fl_est)
sd(KD$d347_est)
sd(KD$d356_est)
sd(KD$dm_est)
sd(BM$ap_est)
sd(BM$fl_est)
sd(BM$d347_est)
sd(BM$d356_est)
sd(BM$dm_est)

		             


ap1$Pbtrans <- ap1$Pf/ap1$Pb
lmfit <- lm(Pbtrans~Pf, data=ap1, na.action=na.omit)
plot(ap1$Pf, ap1$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=ap1, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(ap1$Pf, ap1$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

ap2$Pbtrans <- ap2$Pf/ap2$Pb
lmfit <- lm(Pbtrans~Pf, data=ap2, na.action=na.omit)
plot(ap2$Pf, ap2$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=ap2, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(ap2$Pf, ap2$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

ap3$Pbtrans <- ap3$Pf/ap3$Pb
lmfit <- lm(Pbtrans~Pf, data=ap3, na.action=na.omit)
plot(ap3$Pf, ap3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=ap3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(ap3$Pf, ap3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")



fl1$Pbtrans <- fl1$Pf/fl1$Pb
lmfit <- lm(Pbtrans~Pf, data=fl1, na.action=na.omit)
plot(fl1$Pf, fl1$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=fl1, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(fl1$Pf, fl1$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

fl2$Pbtrans <- fl2$Pf/fl2$Pb
lmfit <- lm(Pbtrans~Pf, data=fl2, na.action=na.omit)
plot(fl2$Pf, fl2$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=fl2, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(fl2$Pf, fl2$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

fl3$Pbtrans <- fl3$Pf/fl3$Pb
lmfit <- lm(Pbtrans~Pf, data=fl3, na.action=na.omit)
plot(fl3$Pf, fl3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=fl3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(fl3$Pf, fl3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")


d347_1$Pbtrans <- d347_1$Pf/d347_1$Pb
lmfit <- lm(Pbtrans~Pf, data=d347_1, na.action=na.omit)
plot(d347_1$Pf, d347_1$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=d347_1, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(d347_1$Pf, d347_1$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

d347_2$Pbtrans <- d347_2$Pf/d347_2$Pb
lmfit <- lm(Pbtrans~Pf, data=d347_2, na.action=na.omit)
plot(d347_2$Pf, d347_2$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=d347_2, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(d347_2$Pf, d347_2$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

d347_3$Pbtrans <- d347_3$Pf/d347_3$Pb
lmfit <- lm(Pbtrans~Pf, data=d347_3, na.action=na.omit)
plot(d347_3$Pf, d347_3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=d347_3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(d347_3$Pf, d347_3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")



d356_1$Pbtrans <- d356_1$Pf/d356_1$Pb
lmfit <- lm(Pbtrans~Pf, data=d356_1, na.action=na.omit)
plot(d356_1$Pf, d356_1$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=d356_1, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(d356_1$Pf, d356_1$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

d356_2$Pbtrans <- d356_2$Pf/d356_2$Pb
lmfit <- lm(Pbtrans~Pf, data=d356_2, na.action=na.omit)
plot(d356_2$Pf, d356_2$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=d356_2, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(d356_2$Pf, d356_2$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

d356_3$Pbtrans <- d356_3$Pf/d356_3$Pb
lmfit <- lm(Pbtrans~Pf, data=d356_3, na.action=na.omit)
plot(d356_3$Pf, d356_3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=d356_3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(d356_3$Pf, d356_3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")



dm1$Pbtrans <- dm1$Pf/dm1$Pb
lmfit <- lm(Pbtrans~Pf, data=dm1, na.action=na.omit)
plot(dm1$Pf, dm1$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=dm1, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(dm1$Pf, dm1$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

dm2$Pbtrans <- dm2$Pf/dm2$Pb
lmfit <- lm(Pbtrans~Pf, data=dm2, na.action=na.omit)
plot(dm2$Pf, dm2$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=dm2, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(dm2$Pf, dm2$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")

dm3$Pbtrans <- dm3$Pf/dm3$Pb
lmfit <- lm(Pbtrans~Pf, data=dm3, na.action=na.omit)
plot(dm3$Pf, dm3$Pbtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb~Bm*Pf/(Kd+Pf),data=dm3, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(dm3$Pf, dm3$Pb)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf=x))
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
resapc3 <- resap[-c(3),]
resflc3 <- resfl[-c(3),]

#c(bm_val$ap_est,bm_val$fl_est,bm_val$d347_est,bm_val$d356_est,bm_val$dm_est) = c(bm_res$ap_est,bm_res$fl_est,bm_res$d347_est,bm_res$d356_est,bm_res$dm_est)
#kd_val = c(kd_res$ap_est,kd_res$fl_est,kd_res$d347_est,kd_res$d356_est,kd_res$dm_est)


