
#' Iterative List Check
#'
#' An iterative list is one which is generated from an lapply loop.
#' The elements of l does not need to be named. The elements of l
#' should be named objects. Each element within l should be an object 
#' with the same names. No element in l should be null, but it is ok 
#' if an element of l contains a null object (as long as it is named).
#'
#' @param l a list
#'
#' @return logical
#'
#' @export
#'
#' @examples 
#' x <- purrr::rerun(5, x = list(a = runif(1), b = rnorm(15), c = list(1, 2)), y1 = runif(12), i = iris)
#' is_iterative_list(x)

is_iterative_list <- function(l){
  assertthat::assert_that(
    is.list(l),
    !is.data.frame(l)
  )
  
  everything_named <- function(l){
    n <- names(unlist(l))
    all(n != "")
  }
  if(!everything_named(l)) return(FALSE)

  # TODO currently does not detect names contain numbers at the end
  rm_numbers <- function(e) unique(stringr::str_replace(names(unlist(e)), "[0-9]*$", ""))
  nm <- rm_numbers(l[[1]])
  check_all <- lapply(l, function(x) identical(rm_numbers(x), nm))
  all(unlist(check_all))
}

## TODO remove this?
