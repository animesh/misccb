library("affyQCReport")
library("affydata")
data("Dilution")

nD = updateObject(Dilution)

xxx = affyQAReport(nD)
openQAReport(xxx)

## to delete it
## rmQAReport(xxx)

