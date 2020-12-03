#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 7 --------------------------------------------------------------

# a) ----
?optim

# b) ---- 
# From the last exercise (4_6.R)
rb <- function(X){
  x <- X[1]
  z <- X[2]
  return(
    100 * (z - x^2)^2 + (1-x)^2
  )
}

rb.grad <- function(X){
  x <- X[1]
  z <- X[2]
  dfdx <- 2 * (200 * x^3 - 200 * x * z + x - 1)
  dfdz <- 200 * (z - x^2)
  return(c(dfdx, dfdz))
}

# c) ----
optim(c(-0.5, 1), fn = rb, gr = rb.grad)
# Converges (code:0),
# Minimum accurate to 3 d.p.

# d) ----
optim(c(-0.5, 1), fn = rb, method = "BFGS")
# 119 Function evaluations (i.e. of rb?) used
# Minimum accurate to 4 d.p.

# e) ----
optim(c(-0.5, 1), fn = rb, gr = rb.grad, method = "BFGS")
# Uses fewer function evaluations (111)
# And finds extremeley accurate values of x=1,z=1 reported.

# f) ----
optim(c(-0.5, 1), fn = rb, gr = rb.grad, method = "CG")
# Reaches maxit with estimate 1.2, 1.5 and 400+ function calls.
optim(c(-0.5, 1), fn = rb, gr = rb.grad, method = "CG",
      control = list(maxit = 1000))
d <- optim(c(-0.5, 1), fn = rb, gr = rb.grad, method = "CG",
           control = list(maxit = 1000))

# Do a while loop to get ballpark answer...
conv <- 1
max.iter <- 1000
while(conv != 0){
  d <- optim(c(-0.5, 1), fn = rb, gr = rb.grad, method = "CG",
  control = list(maxit = max.iter))
  conv <- d$convergence
  max.iter <- max.iter + 100
  message(max.iter, " ", conv)
}

# About 10.4k iterations needed for convergence of this method.