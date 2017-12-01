
#' To Be or Not To Be
#'
#' Obtains the subset and the opposite of the subset (anti-subset) of data frames. 
#'
#' @param x an object to be subsetted
#' @param f a subsetting function (subset, dplyr::slice, dplyr::filter, dplyr::select, etc) that applies to only one dimension of the data frame
#' @param ... additional arguments to subsetting function f
#'
#' @return list \cr
#' Element "to_be" contains the subsetted object \cr
#' Element "not_to_be" contains the opposite of the subsetted object (anti-subset) \cr
#'
#' @export
#'
#' @examples
#' to_be(mtcars, dplyr::slice, 1:5)
#' to_be(mtcars, subset, cyl != 4)
#' to_be(mtcars, dplyr::filter, cyl != 4, mpg > 20)
#' to_be(mtcars, dplyr::select, -one_of(c("gear", "vs")))
#' to_be(iris, view_duplicated)
#'

to_be <- function(x, f, ...){
  "Obtains the subset and the opposite of a subset"
  
  assertthat::assert_that(
    is.data.frame(x),
    is.function(f)
  )

  the_question <- x
  to_be <- tryCatch({
    f(the_question, ...)  
  }, error = function(err){
    message(err$message)
    stop("The function could not be applied to the dataframe as specified")  
  })
  
  # check in case drop = TRUE
  assertthat::assert_that(is.data.frame(to_be), msg = "The function did not return a data frame")

  if( (nrow(the_question) != nrow(to_be)) & (ncol(the_question) != ncol(to_be)) ){
    stop("The function should only apply one one dimension of the data frame")
  } else if( nrow(the_question) != nrow(to_be) ){
    not_to_be <- dplyr::anti_join(the_question,to_be, by = colnames(the_question))
  } else if( ncol(the_question) != ncol(to_be) ){
    not_to_be <- dplyr::select(the_question, -dplyr::one_of(colnames(to_be)))
  } else{
    not_to_be <- head(the_question, 0) # function did not change anything, return empty df
  }

  list(to_be = to_be, not_to_be = not_to_be)
}

