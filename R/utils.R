# function to calculate the mode of a vector (most common value)
extract_mode <- function(x) {
  
  # work out frequency of all values
  freq <- table(x)
  
  # return the name of the value with the highest frequency
  names(freq)[order(freq, decreasing = TRUE)][1]
  
}
