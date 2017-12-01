
#' Extract Data Accumulated in a List
#'
#' Loops through an iterative list and rearranges data structures. Accumulates structures of the same
#' name into a subsetted list which can be accessed using \code{$}. \cr
#' See \code{\link{is_iterative_list}} to see what qualifies.
#'
#' @param l a list
#' @param rbind logical; if TRUE rbinds/stacks all extracted elements into a data.table (if they are data frames);
#' else they are returned as lists. Elements that are not data frames are returned as lists
#' @param recursive logical; if TRUE extraction applied to list components of x
#'
#' @return a list
#'
#' @export
#'
#' @examples
#' x <- purrr::rerun(3, 
#'   WI = list(a = matrix(1:6, nrow = 2), b = data.frame(i = 1:10, j = 1:10)),
#'   MN = list(cold = list(blue = runif(5), red = runif(15)), hot = rnorm(3)),
#'   CA = LETTERS
#' )
#' lst <- extract_list(x)
#' lst$MN$cold$red
#'

extract_list <- function(l, rbind = TRUE, recursive = TRUE){
  "Accumulates data structures of the same name (from an iterative list) into a common data structure."
  
  assertthat::assert_that(!missing(l), msg = "Input l is missing")
  assertthat::assert_that(is_iterative_list(l), msg = "Input l is not an iterative list")
  assertthat::assert_that(is.logical(rbind), msg = "rbind is not a boolean")
  assertthat::assert_that(is.logical(recursive), msg = "recursive is not a boolean")
  extraction(l, rbind = rbind, recursive = recursive)
}
