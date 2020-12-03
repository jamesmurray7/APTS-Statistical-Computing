#' ###
#' APTS 4. "Some exercises"
#' ###

rm(list=ls())

# Question 5 --------------------------------------------------------------

# a) ----

q5 <- function(x){
  n <- length(x) # Get number of entries in 
  store <- c() # Initialise empty vector 
  for(i in 1:n){ # For each x_i value
    store[i] <- length(which(x < x[i]))/n # Calculate c.d.f
  }
  return(store) # Return in order
}
x <- 1:10 # Test on trivial set 1:10
q5(x) # Looks like it works

# Bigger, random sample
set.seed(1995)
x <- runif(1e3,1, 20) # Slows down a lot after 1000!
q5(x) # Looks as though it works.

# b) ----
# Modifying the above function to plot something if required.

q5b <- function(x, make.plot = F){
  n <- length(x) # Get number of entries in 
  store <- c() # Initialise empty vector 
  for(i in 1:n){ # For each x_i value
    store[i] <- length(which(x < x[i]))/n # Calculate c.d.f
  }
  
  if(make.plot){ # If we want to make a plot...
    df <- data.frame(x, store)
    df <- dplyr::arrange(df, x) # Arrange or will look a mess.
    plot(df$x, df$store, "s", col = "red", xlab = "x", ylab = "c.d.f") # Step plot.
  }
  
  return(store) # Return in order
}

x <- 1:10 # Small sample 1:10 to see if it works.
q5b(x)
q5b(x, make.plot = T) # cool
x <- runif(100,1, 20) # Bigger sample
q5b(x, make.plot = T)
x <- runif(1000, 1, 100)
q5b(x, make.plot = T) # Approaching a straight line at this point.
