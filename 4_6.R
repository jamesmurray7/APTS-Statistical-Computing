#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 1 --------------------------------------------------------------

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







        