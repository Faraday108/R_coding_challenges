# Coding Challenge 2
# Prompt from ChatGPT: 

# Task: Write an R function that takes a string as input and returns the count of each unique word in the string.

#Constraints:
  
# The function should be named word_count.
# It should take a single argument, text, which is a character string.
# It should return a named integer vector where each name is a unique 
  # word in the input string and the corresponding value is the count of that 
  # word in the string.
# Words are case-insensitive, so "Hello" and "hello" should be considered the 
  # same word.

word_count <- function(text, base = FALSE) {
  # Start with error handling
  if(!is.character(text)) {
    stop("Enter a character string")
  } else if(nchar(gsub("[^[:alnum:]]+", "", text)) == 0){
    stop("Enter a string of with at least one letter")
  }
  
  # Using stringi
  if(!base) {
    lower_text <- stringi::stri_trans_tolower(text)
    summary <- table(stringi::stri_extract_all_words(lower_text))
    return(sort(summary, decreasing = TRUE))
  }
  
  # Base R
  if(base) {
    # Splits lower case text on one or more non-word character
    return(sort(table(strsplit(tolower(text), "\\W+")), decreasing = TRUE))
  }
}
