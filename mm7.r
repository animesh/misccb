apip_m    <- read.table("apip_m.txt")
fl_m   <- read.table("fl_m.txt")
		             


apip_m$Pb_mtrans <- apip_m$Pf_m/apip_m$Pb_m
lmfit <- lm(Pb_mtrans~Pf_m, data=apip_m, na.action=na.omit)
plot(apip_m$Pf_m, apip_m$Pb_mtrans)
abline(coef(lmfit))
Bm <- 1/coef(lmfit)[2]
Kd <- Bm*coef(lmfit)[1]
Bm
Kd
nlsfit <- nls(Pb_m~Bm*Pf_m/(Kd+Pf_m),data=apip_m, start=list(Kd=Kd, Bm=Bm))
summary(nlsfit)
plot(apip_m$Pf_m, apip_m$Pb_m)
x <- seq(0, 60, length=120)
y2 <- (coef(nlsfit)["Bm"]*x)/(coef(nlsfit)["Kd"]+x)
y2 <- predict(nlsfit,data.frame(Pf_m=x))
lines(x, y2)
y1 <- (Bm*x)/(Kd+x)
lines(x, y1, lty="dotted", col="red")
write.table((cbind(c(x),c(y1),c(y2))), file = "apip_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )


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
