data <- read.table("d1.txt")
pred <- prediction(data[,1], data[,2])
## ROC curve
plot(performance(pred, measure = "tpr", x.measure = "fpr"))