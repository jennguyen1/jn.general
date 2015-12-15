
#' To Be or Not To Be
#'
#' Returns the subset and the opposite of the subset (anti-subset) of data frames
#'
#' @param x an object to be subsetted
#' @param f a subsetting function (subset, dplyr::slice, dplyr::filter, dplyr::select, etc)
#' @param ... additional arguments to subsetting function f; see details
#'
#' @details
#' Any type of arguments normally passed to the subsetting function given. If both subset and
#' select arguments are given the not_to_be object is a data frame of the same size as x, but
#' values that were extracted are converted to NA.
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
#' to_be(mtcars, subset, subset = mpg > 20, select = c(cyl, hp, wt))
#' to_be(iris, view_duplicated)
#'
#' # outputs data that contains no na's (complete cases) and data that contains na's
#' x <- rdata(50, gen_char(values = c("A", "B"), name = "grp"), gen_num(5, 1:7, add.na = TRUE))
#' y <- to_be(x, dplyr::filter, is.na(X_1)) %>% lapply(data.table::data.table)
#'
#' # replace all na's in X_1 with the mean of X1 by group and combine back
#' replace_values <- y[["not_to_be"]] %>% .[, list(X_1 = mean(X_1)), by = "grp"]
#' replace_na <- y[["to_be"]] %>% dplyr::select(-X_1) %>% merge(replace_values, by = "grp")
#'
#' # combine back with original data
#' newx <- rbind(replace_na, y[["not_to_be"]])
#'

to_be <- function(x, f, ...){

  # error checking
  if( !is.function(f) ) stop("Input f is not a function")
  if( !is.data.frame(x) ) stop("Input x must be a data frame")

  # rename x
  the_question <- x

  # save the_question's rownames in its own column/variable, to ensure they are conserved when filtering
  the_question$rownames <- rownames(the_question)

  # apply subset on the_question
  ## fix: for functions like view_duplicated that act on all columns, including the rownames columns results in an error;
  ## so for only these functions, rownames cannot be conserved (which makes sense because wouldn't want to run distinct/duplicated on rownames anyways)
  args <- substitute(list(...))
  if( length(args) == 1 ){
    suppressWarnings(the_question$rownames <- NULL)
    to_be <- f(the_question)
  } else{
    to_be <- f(the_question, ...)
  }

  # check in case drop = TRUE
  if( !is.data.frame(to_be) ) stop("The subset function did not return a data frame")

  # if the original output was not a data.table and the function return was, return output to be data.table
  if( !data.table::is.data.table(x) & data.table::is.data.table(to_be)) to_be <- data.frame(to_be)

  # subset and select
  if( (nrow(the_question) != nrow(to_be)) & (ncol(the_question) != ncol(to_be)) ){
    # replace values of subset with NA
    not_to_be <- the_question
    not_to_be[rownames(to_be), colnames(to_be)] <- NA

  # subset
  } else if( nrow(the_question) != nrow(to_be) ){
    # find the rows from the_question not in to_be
    not_to_be <- dplyr::anti_join(the_question,to_be, by = colnames(the_question))

    # replace the row names
    rownames(to_be) <- to_be$rownames
    rownames(not_to_be) <- not_to_be$rownames

  # select
  } else if( ncol(the_question) != ncol(to_be) ){
    # find cols in the_question that are not in to_be
    not_col <- colnames(to_be)
    not_to_be <- dplyr::select(the_question, -one_of(not_col))

  # subset is the same
  } else{
    not_to_be <- NULL
  }

  # remove the rownames column
  to_be$rownames <- NULL
  not_to_be$rownames <- NULL

  # return results
  return_list <- list(to_be = to_be, not_to_be = not_to_be)
  return(return_list)
}

