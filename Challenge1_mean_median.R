## Coding challenge 1
# Prompt from ChatGPT: 

# Task: Write an R function that takes a numeric vector as input and returns 
# the mean and median of the vector.

# Constraints:
  
# The function should be named mean_median.
# It should take a single argument, x, which is a numeric vector.
# It should return a named list with two elements: mean and median.

mean_median <- function(x) {
  # if(class(x) != "numeric") {
  #   stop("Please enter a numeric vector")
  # }
  ## After testing the above error message, found it returns error for x of class
  ## integer. Can add this condition to if() or use is.numeric: 
  if(!is.numeric(x)) {
    stop("Please enter a numeric vector")
  }
  
  len <- length(x)
  out <- list(mean = numeric(), median = numeric())
  out$mean <- sum(x)/len
  out$median <- ifelse(len %% 2 == 1, # test if odd
                       x[len %/% 2 + 1], # if T, return middle
                       sum(x[(len %/% 2):(len %/% 2 + 1)])/2)
  out
}
