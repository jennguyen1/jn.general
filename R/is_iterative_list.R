
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
#' x <- purrr::rerun(5, x = list(a = runif(1), b = rnorm(15)), y1 = runif(12), i = if(sample(c(TRUE, FALSE), 1)) head(iris) else NULL)
#' is_iterative_list(x)

is_iterative_list <- function(l){
  assertthat::assert_that(
    is.list(l),
    !is.data.frame(l)
  )
  assertthat::assert_that(length(l) > 1, msg = "Input l length must be greater than 1")
  
  first <- check_names(l[[1]])
  unnamed_items <- any(stringr::str_detect(first, "\\$ :"))
  if(unnamed_items) return(FALSE)
  
  check_all <- lapply(l, function(x) identical(check_names(x), first))
  all(unlist(check_all))
}

