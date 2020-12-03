#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# a)
eps <- 1
x <- 1
while(x + eps != x) eps <- eps/2
eps/x

# b)
x == x + eps #TRUE

# c)
2*eps == .Machine$double.eps # TRUE

# d)
epsx <- function(x){
  eps <- 1
  while(x + eps != x) eps <- eps/2
  return(eps/x)
}
store <- list()
p <- 1
for(i in c(1/8, 1/4, 1/2, 1, 2, 4, 8)){
  store[[p]] <- epsx(i)
  p <- p+1
}

length(which(store == epsx(1/2))) # Cant remember how to use isTRUE(all)!
# e)
epsx(2+1e-5) # Not the same number
# f)

