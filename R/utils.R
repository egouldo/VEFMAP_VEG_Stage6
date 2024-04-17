# function to calculate the mode of a vector (most common value)
extract_mode <- function(x) {
  
  # work out frequency of all values
  freq <- table(x)
  
  # return the name of the value with the highest frequency
  names(freq)[order(freq, decreasing = TRUE)][1]
  
}

#' Function to extract the rightmost n characters from a string
#' 
#' @param x A character vector
#' @param n The number of characters to extract
#' @return A character vector
#' @examples
#' substrRight("hello", 2)
#' substrRight("hello", 3)
#' @export
#' @details
#' From https://stackoverflow.com/a/7963963/4593464
#' @author Andrie de Vries
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#' `quarto::quarto_render()`, but output file is moved to `output_dir`
#'
#' The default `quarto::quarto_render()` function can only render outputs
#' to the current working directory. This is a wrapper that moves the rendered
#' output to `output_dir`.
#' @param input Path to the input qmd file.
#' @param output_file The name of the output file. If using `NULL` then the
#' output filename will be based on filename for the input file.
#' @param output_dir Path to the output directory.
#' @param ... Other args passed to `quarto::quarto_render()`
#' @author John Paul Helveston
#' @details
#' Borrowed from https://github.com/jhelvy/jph/tree/master SHA `0b589cd`
#' 
#' @export
quarto_render_move <- function(
    input,
    output_file = NULL,
    output_dir = NULL,
    ...
) {
  
  # Get all the input / output file names and paths
  x <- quarto::quarto_inspect(input)
  output_format <- names(x$formats)
  output <- x$formats[[output_format]]$pandoc$`output-file`
  if (is.null(output_file)) { output_file <- output }
  input_dir <- dirname(input)
  if (is.null(output_dir)) { output_dir <- input_dir }
  output_path_from <- file.path(input_dir, output)
  output_path_to <- file.path(output_dir, output_file)
  
  # Render qmd file to input_dir
  quarto::quarto_render(input = input, ... = ...)
  
  # If output_dir is different from input_dir, copy the rendered output
  # there and delete the original file
  if (input_dir != output_dir) {
    
    # Try to make the folder if it doesn't yet exist
    if (!dir.exists(output_dir)) { dir.create(output_dir) }
    
    # Now move the output to the output_dir and remove the original output
    file.copy(
      from = output_path_from,
      to = output_path_to,
      overwrite = TRUE
    )
    file.remove(output_path_from)
    
    # If the output_dir is the same as input_dir, but the output_file
    # has a different name from the input file, then just rename it
  } else if (output_file != output) {
    file.rename(from = output_path_from, to = output_path_to)
  }
}