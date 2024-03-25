# Task: Write an R function that takes a list of numbers as input and returns 
# the second smallest number in the list.

# Constraints:
  
# The function should be named second_smallest.
# It should take a single argument, lst, which is a list of numeric values.
# If the input list has fewer than two elements, the function should return NULL.
# The function should return the second smallest number in the list.

second_smallest <- function(lst) {
  lst <- unlist(lst)
  rnk <- rank(lst)
  lst[rnk == 2]
}
