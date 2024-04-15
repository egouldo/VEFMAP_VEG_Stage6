#' Calculate the mode of a vector (most common value)
#' 
#' This function calculates the mode of a vector, i.e. the most common value.
#' 
#' @param x A vector of values
#' @return The mode of the vector
#' @export
extract_mode <- function(x) {
  
  # work out frequency of all values
  freq <- table(x)
  
  # return the name of the value with the highest frequency
  names(freq)[order(freq, decreasing = TRUE)][1]
  
}

#' Calculate a constant based on the variable `pr_cover`
#' 
#' This function calculates a constant value to add to the response variable.
#' @details 
#' The constant is calculated as the minimum value of `pr_cover` where `pr_cover > 0` plus 0.5, multiplied by 0.5.
#' The constant is to be added to the response variable to avoid log(0) errors in the lognormal models.
#' 
#' @param df A data frame containing the response variable
#' @return A constant value to add to the response variable
#' @export
#' @importFrom purrr pluck
#' @importFrom dplyr filter
#' @examples
#' calc_constant(veg_cover_ar)
calc_constant <- function(df) {
  constant <- df %>% 
    filter(pr_cover > 0) %>% 
    pluck("pr_cover", 
          min) %>%
    sum(0.5)
  
  constant * 0.5
}