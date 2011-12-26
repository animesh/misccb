resdataap <- read.table("ap.txt")
resdataapip <- read.table("apip.txt")
resdatafl <- read.table("fl.txt")
resdata <- read.table("data.txt")
resconc <- read.table("concval.txt")

px=c(resdataap$ConcPm,(resdataapip$ConcPm),resdatafl$ConcPm)
py=c(resdataap$Ap,resdataapip$Ap_Ip,resdatafl$Fl)
plot(px,py)


lmfit <- lm(Ap~ConcPm, data=resdataap, na.action=na.omit)
summary(lmfit)
plot(resdataap$ConcPm,resdataap$Ap)
abline(coef(lmfit))

lmfit <- lm(Ap_Ip~ConcPm, data=resdataapip, na.action=na.omit)
summary(lmfit)
plot(resdataapip$ConcPm,resdataapip$Ap_Ip)
abline(coef(lmfit))

lmfit <- lm(Fl~ConcPm, data=resdatafl, na.action=na.omit)
summary(lmfit)
plot(resdatafl$ConcPm,resdatafl$Fl)
abline(coef(lmfit))

write.table((cbind(c(x),c(y1),c(y2))), file = "ap_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )
write.table((cbind(c(x),c(y1),c(y2))), file = "ap_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )
write.table((cbind(c(x),c(y1),c(y2))), file = "ap_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )

