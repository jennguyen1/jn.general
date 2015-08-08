
#' Split and Scan a String
#'
#' Splits the string by the delimiter and grabs the index-th value
#'
#' @param string input vector
#' @param index position of string want to isolate after splitting
#' @param delimiter what to split the string by
#'
#' @return string vector
#' @export
#'
#' @examples
#' x <- c("language1_r", "language2_python", "language3_java")
#' str_scan(x, 2, "_")

str_scan <- function(string, index = 1, delimiter = " "){

  # error checking
  if( !is.character(string) ) stop("Missing string argument")

  # split & extract
  str_i <- strsplit(x = string, split = delimiter) %>% sapply(function(x) x[index])
  return(str_i)
}
