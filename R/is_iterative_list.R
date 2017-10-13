
#' Iterative List Check
#'
#' An iterative list is one which is generated from an lapply loop.
#' The lapply function call should return a list with consistent names.
#' Therefore each element within l should be a list with the same names.
#' No element in l should be null, but it is ok if an element of l
#' consists of a named items that are null
#'
#' @param l a list
#'
#' @return logical; is l an iterative list?
#'
#' @import magrittr
#' @export
#'
#' @examples
#' # generate data
#' x <- function(i){
#'  r <- list(
#'   iteration = i,
#'   WI = list(
#'     circle = list(a = matrix(1:4, nrow = 2), b = matrix(rnorm(12), nrow = 2)),
#'     triangle = list(c = matrix(1:8, nrow = 2), d = list(four = 1:10))
#'   ),
#'   WI = matrix(-10:9, nrow = 4)
#'  )
#'  return(r)
#' }
#' y <- function(i) list(iteration = i, a = rnorm(1), b = rnorm(1))
#'
#' # is it an iterative list? yes!
#' ex1 <- lapply( 1:5, function(i) return(list(x = x(i), y = y(i))) )
#' is_iterative_list(ex1)
#'
#' # is it an iterative list? yes!
#' ex2 <- lapply( 1:5, function(i){
#'  if(i %% 2 == 1) {
#'    x <- x(i)
#'    y <- y(i)
#'  } else{
#'    x <- NULL
#'    y <- NULL
#'  }
#'  return( list(x = x, y = y) )
#' })
#' is_iterative_list(ex2)
#'
#' # is it an iterative list? no
#' ex3 <- lapply( 1:5, function(i){
#'  if(i %% 2 == 1) {
#'    x <- x(i)
#'    y <- y(i)
#'    return( list(x = x, y = y) )
#'  } else{
#'    return(NULL)
#'  }
#' })
#' is_iterative_list(ex3)
#'
#' # The idea is to return a named list at every iteration.
#' # It is important it is named, even if it is completely NULL

is_iterative_list <- function(l){

  # is l a list?
  if( !is.list(l) | is.data.frame(l) ) stop("input list is not a list")

  # name of first items
  nm <- names(l[[1]])

  # test that name of all items are same as the name of the first item
  lapply(l, function(x) identical(names(x), nm)) %>% unlist %>% all %>% return
}