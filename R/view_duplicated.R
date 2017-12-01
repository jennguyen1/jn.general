
#' Show Duplicated Rows
#'
#' Finds all duplicated rows by column(s). Equivalent to doing data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
#'
#' @param data a data frame
#' @param ... columns to search for duplicated values, defaults to all arguments; can also apply dplyr special functions
#'
#' @return a data frame with duplicated rows by given columns
#'
#' @export
#'
#' @seealso \code{\link{remove_duplicated}} to remove duplicates
#'
#' @examples
#' d <- data.frame(x1 = rep(1:3, each=4), x2 = rep(1:4, each=3), z = rep(1:2, 6), a = rep(1:6, 2))
#' view_duplicated(data = d, x2, z)
#' view_duplicated(d, a, z)
#' view_duplicated(d)
#' view_duplicated(d, starts_with("x"))
#'

view_duplicated <- function(data, ...){
  "Finds all duplicated rows by column(s)"
  
  assertthat::assert_that(!missing("data"), msg = "Missing data argument")
  assertthat::assert_that(is.data.frame(data))
  
  if(nrow(data) == 0) return(data)
  
  keys <- as.character(substitute(list(...))[-1])
  if( length(keys) == 0 ){
    d <- data
  } else{
    d <- tryCatch({
      out <- dplyr::select(data, ...)
      assertthat::assert_that(length(colnames(out)) > 0, msg = "0 columns selected")
      out
    }, error = function(err){
      message(err$message)
      stop("The function could not be applied to the dataframe as specified")
    })
  }
  dups <- unique(subset(d, duplicated(d)))
  dplyr::semi_join(data, dups, by = colnames(d))
}
