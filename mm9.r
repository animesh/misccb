resdataecol <- read.table("data2.txt")
dline <- read.table("res_line2.txt")

plot(resdataecol$ConcPm,resdataecol$Ecol)

lmfit <- lm(Ecol~ConcPm, data=resdataecol, na.action=na.omit)
summary(lmfit)
plot(resdataecol$ConcPm,resdataecol$Ecol)
abline(coef(lmfit))

x <- seq(0, 2000, length=201)
yecol <- (dline$Ecol[1]*x+dline$Ecol[2])

write.table((cbind(c(x),c(yecol))), file = "ecol_otp.txt", sep = "\t",col.names = FALSE, row.names = FALSE )
