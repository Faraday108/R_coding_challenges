# Task: Write an R function that takes a list of integers and a target integer 
# as input and returns a list of all pairs of integers from the input list that 
# sum up to the target integer.

# Constraints:

# The function should be named find_pairs.
# It should take two arguments: lst (a list of integers) and target (an integer).
# The function should return a list of all pairs of integers from lst that sum 
#   up to target.
# Each pair should be represented as a vector of length 2.
# If no pairs are found, the function should return an empty list.

find_pairs <- function(lst, target) {
  islist <- is.list(lst) # error condition 1
  isinteger <- all(sapply(lst, function(x) (x %% 1) == 0)) # error condition 2
  tgt_integer <- (target %% 1)  == 0
  if(!islist) {# ensure a list was entered
    stop("Enter a list of numeric integers")
  } else if(!isinteger){ # ensure that arguments are integers
    stop("Check all elements of list are numeric")
  } else if(!tgt_integer) {
    stop("Check the target value is an integer")
  }
  
  unlst <- unique(unlist(lst)) # unlist the input, remove duplicates
  remain <- target - unlst # find difference between target and list
  # Find pairs: 
  # Condition 1: if "remain" is in unlst, a pair that adds to target is found
  # Condition 2: prevent duplicates by limit remain to be >= than half of target
  match_index <- (remain %in% unlst) & (remain > target/2)
  
  # Requires list of pairs as return: mapply is a good option for this
  mapply(function(x,y) c(x,y), 
         remain[match_index], 
         unlst[match_index], SIMPLIFY = FALSE)
}
