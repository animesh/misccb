ap_m    <- read.table("ap_m.txt")
fl_m   <- read.table("fl_m.txt")
m347_m <- read.table("m347_m.txt")
m356_m <- read.table("m356_m.txt")
dm_m    <- read.table("dm_m.txt")

		             


ap_m$Pb_mtrans <- ap_m$Pf_m/ap_m$Pb_m
lmfit <- lm(Pb_mtrans~Pf_m, data=ap_m, na.action=na.omit)
plot(ap_m$Pf_m, ap_m$Pb_mtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb_m~Bm*Pf_m/(Kd+Pf_m),data=ap_m, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(ap_m$Pf_m, ap_m$Pb_m)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf_m=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")
write.table((cbind(c(x),c(y1),c(y2))), file = "ap_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )


fl_m$Pb_mtrans <- fl_m$Pf_m/fl_m$Pb_m
lmfit <- lm(Pb_mtrans~Pf_m, data=fl_m, na.action=na.omit)
plot(fl_m$Pf_m, fl_m$Pb_mtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb_m~Bm*Pf_m/(Kd+Pf_m),data=fl_m, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(fl_m$Pf_m, fl_m$Pb_m)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf_m=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")
write.table((cbind(c(x),c(y1),c(y2))), file = "fl_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )


m347_m$Pb_mtrans <- m347_m$Pf_m/m347_m$Pb_m
lmfit <- lm(Pb_mtrans~Pf_m, data=m347_m, na.action=na.omit)
plot(m347_m$Pf_m, m347_m$Pb_mtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb_m~Bm*Pf_m/(Kd+Pf_m),data=m347_m, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(m347_m$Pf_m, m347_m$Pb_m)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf_m=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")
write.table((cbind(c(x),c(y1),c(y2))), file = "m347_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )



m356_m$Pb_mtrans <- m356_m$Pf_m/m356_m$Pb_m
lmfit <- lm(Pb_mtrans~Pf_m, data=m356_m, na.action=na.omit)
plot(m356_m$Pf_m, m356_m$Pb_mtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb_m~Bm*Pf_m/(Kd+Pf_m),data=m356_m, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(m356_m$Pf_m, m356_m$Pb_m)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf_m=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")
write.table((cbind(c(x),c(y1),c(y2))), file = "m356_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )




dm_m$Pb_mtrans <- dm_m$Pf_m/dm_m$Pb_m
lmfit <- lm(Pb_mtrans~Pf_m, data=dm_m, na.action=na.omit)
plot(dm_m$Pf_m, dm_m$Pb_mtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb_m~Bm*Pf_m/(Kd+Pf_m),data=dm_m, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(dm_m$Pf_m, dm_m$Pb_m)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf_m=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")
write.table((cbind(c(x),c(y1),c(y2))), file = "dm_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )

