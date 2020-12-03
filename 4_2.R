#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 2 --------------------------------------------------------------

# a) ----
X <- matrix(runif(100000), 1000, 100)
z <- rep(0, 1000)
system.time(
  for(i in 1:1000){
    for(j in 1:100){
      z[i] <- z[i] + X[i, j]
    }
  }
) # This takes 0.03 seconds
X[1:5, 1:5]; z[1:5]
z.loop <- z
# First using apply
system.time(
  z.apply <- apply(X, 1, sum)
)
# And then using rowSums
system.time(
  z.rowsums <- rowSums(X)
)
which(z.loop != z.apply)
which(z.loop != z.rowsums)
# Both faster and give same results.

# b) ----
rm(list=ls())
n <- 100000
z <- rnorm(n)
zneg <- 0; j <- 1
system.time(
  for(i in 1:n){
    if(z[i] < 0){
      zneg[j] <- z[i]
      j <- j + 1
    }
  }
) # This takese 0.04 seconds and gives 50,146 negative values.
zneg.loop <- zneg
zneg <- c()

system.time(
  zneg <- unlist(lapply(z, function(x) if(x < 0) x))
) # This is a bit slower, oops.

system.time(
  zneg <- subset(z, z < 0)
) # okay this is better!

