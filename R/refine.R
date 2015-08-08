
#' Refine
#'
#' Wrapper for Filter. (change the order of args)
#' Returns subset of lists that meets conditions supplied by a function.
#' Applies the function f onto the list and returns that which are true.
#'
#' @param obj An object to be filtered
#' @param f Function to apply on each element of object. Function should return a logical vector
#'
#' @return a object that has been filtered as requested
#'
#' @import magrittr
#'
#' @examples
#' l <- list(NULL, mtcars, iris, NULL, mtcars, NULL, iris)
#' refine(l, function(x) !is.null(x))
#' refine(l, function(x) x %>% colnames %>% str_detect("mpg") %>% any)
#' rm_na_list(l)
#'

#' @name refine
NULL

#' @export
#' @rdname refine
refine <- function(obj, f) Filter(f, obj)

#' @export
#' @rdname refine
rm_na_list <- function(list) jn.general::refine(list, function(x) !is.null(x))
