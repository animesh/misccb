###################################################
### chunk number 1: preliminaries
###################################################
options(width=75)


###################################################
### chunk number 2: diag-class
###################################################
library(Matrix)
(D4 <- Diagonal(4, 10*(1:4)))
str(D4)
diag(D4)


###################################################
### chunk number 3: diag-2
###################################################
diag(D4) <- diag(D4) + 1:4
D4


###################################################
### chunk number 4: unit-diag
###################################################
str(I3 <- Diagonal(3)) ## empty 'x' slot

getClass("diagonalMatrix") ## extending "denseMatrix"


###################################################
### chunk number 5: Matrix-ex
###################################################
(M <- spMatrix(4,4, i=1:4, j=c(3:1,4), x=c(4,1,4,8))) # dgTMatrix
m <- as(M, "matrix")
(M. <- Matrix(m)) # dsCMatrix (i.e. *symmetric*)


###################################################
### chunk number 6: sessionInfo
###################################################
toLatex(sessionInfo())


