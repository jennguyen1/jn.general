
#' Rename Data Nested within a List
#'
#' Loops through the elements of a list and adds a name_id column to data.frames 
#' corresponding to the provided names.
#'
#' @param my_list a list; list elements may or may not have names
#' @param names character vector; the names of the elements of the list, if the list is unnamed
#'
#' @return a list
#'
#' @export
#'
#' @examples
#'
#' #############
#' # Example 1 #
#' #############
#' paths <- list.files(directory)
#' files <- lapply(paths, fread)
#' rename_list(list = files, names = paths)
#'
#' #############
#' # Example 2 #
#' #############
#' l <- purrr::rerun(5, x = list(a = head(mtcars), b = head(iris), c = list(o = 1, p = list(q = head(airquality), u = 1:10))), y = head(mtcars))
#' rename_list(l, names = paste0("id", 1:5))
#' 
#' names(l) <- paste0("id", 1:5)
#' rename_list(l)

rename_list <- function(my_list, names = NULL){
  assertthat::assert_that(!missing(my_list), msg = "Input my_list is missing")
  assertthat::assert_that(is.list(my_list) & !is.data.frame(my_list), msg = "Input my_list must be a list (not a data.frame)")
  assertthat::assert_that(!(is.null(names) & is.null(names(my_list))), msg = "Input names is missing")
  
  if(is.null(names)){
    names <- names(my_list)
  } else{
    assertthat::assert_that(is.character(names), msg = "Invalid names argument")  
  }
  check_names <- unique(Filter(function(x) x != "" & !is.na(x), names))
  assertthat::assert_that(
    length(names) == length(check_names),
    length(my_list) == length(check_names), 
    msg = "Invalid names provided; names should be distinct identifiers corresponding to each element of my_list"
  )
  names(my_list) <- names
  
  new_l <- Map(function(l, n) aux_rename_list(l, n), my_list, names)
  return(new_l)
}
