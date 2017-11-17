
#' Show Duplicated Rows
#'
#' Finds all duplicated rows by given columns. Equivalent to doing data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
#'
#' @param data a data frame
#' @param ... columns to search for duplicated values; defaults to all arguments
#'
#' @return a data frame with duplicated rows by given columns
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
  assertthat::assert_that(!missing("data"), msg = "Missing data argument")
  assertthat::assert_that(is.data.frame(data))

  if(nrow(data) == 0) return(data)
  
  keys <- as.character(substitute(list(...))[-1])
  assertthat::assert_that(all(keys %in% colnames(data)), msg = "Columns specified not in input data")
  if(length(keys) == 0) keys <- colnames(data)
  
  d <- subset(data, select = keys)
  dups <- unique(subset(d, duplicated(d)))
  dplyr::semi_join(data, dups, by = keys)
}
