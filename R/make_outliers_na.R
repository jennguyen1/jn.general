
#' Makes Outliers NA
#' 
#' Makes all values beyond specified standard deviations from the mean NA.
#'
#' @param x a numeric vector, with more than 2 values
#' @param multiplier a number greater than 0, sd is multiplied by this to determine boundaries
#'
#' @return numeric vector with outliers NA
#'
#' @export
#'
#' @examples
#' make_outliers_na(rnorm(1000), 2)
#' 

make_outliers_na <- function(x, multiplier){
  "Make all values beyond specified standard deviations from the mean NA"
  
  assertthat::assert_that(!missing(x), !missing(multiplier), msg = "Inputs are missing")
  assertthat::assert_that(is.numeric(x), is.numeric(multiplier))
  assertthat::assert_that(length(x) > 1, msg = "Input x must contain more than 2 values")
  assertthat::assert_that(
    length(multiplier) == 1, 
    multiplier > 0,
    msg = "Input multiplier should a number greater than 0"
  )
  
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  boundaries <- m + c(-1, 1) * multiplier * s
  ifelse( dplyr::between(x, boundaries[1], boundaries[2]), x, NA)
}

