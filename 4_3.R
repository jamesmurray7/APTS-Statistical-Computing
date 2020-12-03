#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 3 --------------------------------------------------------------

rm(list = ls())
set.seed(1)
n <- 1000
A <- matrix(runif(n * n), n, n)
x <- runif(n)
W <- diag(x)

# xtAx
t(x) %*% A %*% x # 127265.6
# tr(A)
sum(diag(A)) # 511.6692
# tr(AtWA)
sum(diag(t(A) %*% W %*% A)) # 168205.1