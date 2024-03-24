# Mini Project:

# The kth moment about zero is defined by E((X - μ_x)^k), the kth standardized moment is defined by
# E(((X - μ_x)/σx)^k) when k = 4. We call the last term the kurtosis (bulging).
# Calculation
# Use the following table to calculate the kurtosis:

#   Values of x and P(x)
x <- 0:6                                    # random variable x
x_str <- sprintf("%s", x)
px = c(0.1, 0.2, 0.2, 0.1, 0.2, 0.1, 0.1)

given_df <- data.frame(
  rbind(x_str,
        px),
  stringsAsFactors = FALSE,
  row.names = c("y", "p(y)")
)
names(given_df) <- NULL
given_df

# Make an R function to carry out the calculation. It must print a named list containing the
# mean, variance, and kurtosis.

calculate_values <- function(x, px) { # Be sure to add notes in the Rmd
  mu <- sum(x * px)       # returns the E(X), mean
  
  i <- 0                  # probability counter
  variance <- 0           # initialize the summation of the variance to 0
  k <- 2                  # set k to 2 to obtain the variance
  
  for (xi in x) {
    i <- i + 1
    variance <- variance + ((xi - mu)^k * px[i])
  }
  
  i <- 0                  # reset the probability counter
  kurtosis <- 0           # initialize the summation of the kurtosis to 0
  k <- 4                  # set k to 4 to obtain the kurtosis
  stdv <- sqrt(variance)  # set the standard deviation by rooting the variance
  
  for (xi in x) {
    i <- i + 1
    kurtosis <- kurtosis + (((xi - mu)/stdv)^k * px[i])
  }
  
  list(mean = mu, variance = variance, kurtosis = kurtosis)
}

x <- 0:6                                    # random variable x
px <- c(0.1, 0.2, 0.2, 0.1, 0.2, 0.1, 0.1)
calculate_values(x, px)




