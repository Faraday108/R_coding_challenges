# Coding Challenge 3
# Prompt from ChatGPT:

# Task: Write an R function that takes a list of integers as input and returns 
# the longest consecutive sequence of increasing integers in the list.

# Constraints:
# The function should be named longest_sequence.
# It should take a single argument, lst, which is a list of integers.
# It should return a vector containing the longest consecutive sequence of 
# increasing integers. If there are multiple sequences of the same length, 
# return the first one encountered.

longest_sequence <- function(lst) {
  # Start with error handling
  islist <- is.list(lst) # error condition 1
  isnumeric <- all(sapply(lst, is.numeric)) # error condition 2
  if(!islist) {# ensure a list was entered
    stop("Enter a list of numeric integers")
  } else if(!isnumeric){ # ensure that arguements are numeric
    stop("Check all elements of list are numeric")
  }
  
  unlst <- unlist(lst) # convert list to vector
  diffs <- diff(unlst) # find difference between i and i+1
  lens <- rep(NA, length(diffs)) # stores lengths of runs
  count <- 0 # initialize count of lengths
  
  for(i in 1:length(diffs)) { # find consecutive runs lengths
    if(diffs[i] == 1) { # if consecutive, add 1
      count <- count + 1
      lens[i] <- count
    } else { # if not consecutive, reset count
      count <- 0
      lens[i] <- count
    }
  }
  
  ml <- max(lens) # find max consecutive length
  mi <- min(which(lens == ml)) + 1 # max index (end of sequence), only returns first
  unlst[(mi-ml):mi] # returns sequence
}

# More compact function that uses `rle` to find lengths of runs
longest_sequence2 <- function(lst) {
  # appropriate error handling would go here
  
  unlst <- unlist(lst) # unlist input
  diffs <- diff(unlst) # find diffs
  rle_diffs <- rle(diffs) # find run lengths
  ml <- max(rle_diffs$length) # find max run length
  # add max and all prior lengths 
  mi <- sum(tst$lengths[1:which.max(tst$lengths)]) + 1 
  # return appropriate index
  unlst[(mi-ml):mi]
}
