
#' Show Duplicated Rows
#'
#' Finds all duplicated rows by given columns. Equivalent to doing data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
#'
#' @param data a data frame
#' @param ... columns to search for duplicated values; defaults to all arguments
#'
#' @return a data table with duplicated rows by given columns
#'
#' @import data.table
#'
#' @export
#'
#' @seealso \code{\link{remove_duplicated}} to remove duplicates
#'
#' @examples
#' d <- data.frame(x = rep(1:3, each=4), y = rep(1:4, each=3), z = rep(1:2, 6), a = rep(1:6, 2))
#' view_duplicated(data = d, y, z)
#' view_duplicated(d, a, z)
#' view_duplicated(d)
#'

view_duplicated <- function(data, ...){

  ##################
  # Error Checking #
  ##################

  # throw errors if there are missing data
  if( missing(data) ) stop("Missing data argument")

  # check types
  if( !is.data.frame(data) ) stop("input data is not a data frame")

  #################
  # Function Work #
  #################

  # convert data to data table if it is not already
  if( !data.table::is.data.table(data) ) data <- data.table::data.table(data)

  # set the key to use for duplicates
  keys <- as.character(substitute(list(...))[-1])
  if( any(!(keys %in% colnames(data))) ) stop("Columns specified not in input data")
  suppressWarnings( data.table::setkeyv(data, keys) )

  # find duplicates
  d <- subset(data, duplicated(data) | duplicated(data, fromLast = TRUE))

  # return results
  return(d)
}
