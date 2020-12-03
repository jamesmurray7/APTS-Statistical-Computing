#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 6 --------------------------------------------------------------

# a) ----

rb <- function(x,z){
  if(length(x) != length(z)){
    stop("Must supply equal length vectors x and z")
  }
  f <- c()
  for(i in 1:length(x)){
    f[i] <- 100 * (z[i] - x[1]^2)^2 + (1 - x[i])^2
  }
  return(f)
}

# b) ----
x <- seq(-1.5, 1.5, length.out = 100)
z <- seq(-0.5, 1.5, length.out = 100)
f <- rb(x,z)

f <- outer(c(x,z), f)

# I have no clue if this is correct!
contour(x = seq(-1.5, 1.5, length.out = nrow(f)),
        y = seq(-0.5, 1.5, length.out = ncol(f)),
        z = f)

# c) ----
contour(x = seq(-1.5, 1.5, length.out = nrow(f)),
        y = seq(-0.5, 1.5, length.out = ncol(f)),
        z = log10(f),
        xlim = c(-1.5, 1.5),
        ylim = c(-0.5, 1.5))
# Looks worse?

# d) ----

rb.grad <- function(x,z){
  
  dfdx <- 2 * (200 * x^3 - 200 * x * z + x - 1)
  dfdz <- 200 * (z - x^2)
  
  return(c(dfdx, dfdz))
}

test <- data.frame(x,z)
rb.grad(test$x, test$z)

# e) ----
delta <- 1e-7
(rb(x+delta,rep(1,100))-rb(x,rep(1,100)))/delta
rb.grad(x[2], 1)
# Doesn't seem to work past first one for some reason.
# How hold Z constant?

# f) ----
rb.hess <- function(x,z){
  h11 <- 2 * (600 * x^2 - 200 * z + 1) # d2fdx2
  h21 <- h12 <- -400 * x #d2fdxdz
  h22 <- 200 #d2fdz2
  hess <- matrix(c(h11, h12, h21, h22), 2, 2, byrow = T)
  return(hess)
}
rb.hess(x[1],z[1])

# g) Skip for now ----

# h) ----

# Function that provies MV Taylor Expansion
# a = x*, b = z*
q6h <- function(a, b){
  fab <- rb(a, b) 
  # Grad
  fxab <- rb.grad(a, b)[1]
  fzab <- rb.grad(a, b)[2]
  # Hessian
  hess <- rb.hess(a, b)
  fxxab <- hess[1,1]
  fzzab <- hess[2,2]
  fxzab <- fzxab <- hess[1,2]
  Q <- fab + fxab * (x-a) + fzab * (z-b) + 
    .5 * (fxxab*(x-a)^2 + 2*fxzab*(x-a)*(z-b)+fzzab*(z-b)^2)
  
  #contour(x = a, y = b, z = Q, add = T)
  
  return(Q)
}

# And add plotting function (cant figure out how to put this in above step)
q6hplot <- function(a, b, colour = "red"){
  eval.fn <- q6h(a,b)
  outer.fn <- outer(c(a, b), eval.fn)
  contour(x = seq(-1.5, 1.5, length.out = nrow(outer.fn)),
          y = seq(-0.5, 1.5, length.out = ncol(outer.fn)),
          z = outer.fn, add = T, col = colour)
}

# i) ----

# Re-run this to get base plot
basecontour <- function(){
  dev.off()
  contour(x = seq(-1.5, 1.5, length.out = nrow(f)),
  y = seq(-0.5, 1.5, length.out = ncol(f)),
  z = f, col = "black")
}

basecontour()
q6hplot(-1, 0.5, "blue")
q6hplot(0, 0, "magenta")
q6hplot(1, 1, "red")

# j) ----
basecontour()
q6hplot(.5,.5)
