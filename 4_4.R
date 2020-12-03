#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 4 --------------------------------------------------------------

# a) ----
# Ax=y
?solve
set.seed(0)
n <- 1000
A <- matrix(runif(n*n), n, n)
x.true <- runif(n)
y <- A%*%x.true

# b) ----
A.solve <- solve(A)
system.time(x1 <- A.solve %*% y) # quick (0s)
# mean absolute difference
mean(abs(x1-x.true)) # 2.95e-11

# c) ----
system.time(x2 <- solve(A,y)) # bit longer (0.15s)
# mean absolute difference
mean(abs(x2-x.true)) # 1.35e-12, factor 10 more accurate.

# d) ----
#' There is a small 'gain' in speed calculating the inverse first, 
#' for loss of accuracy of appx. magnitude 3e-11.


