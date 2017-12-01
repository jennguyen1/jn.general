
#' Removes NA Observations By Column
#'
#' @param data a data frame
#' @param ... columns to remove NA obs, defaults to all arguments; can also apply dplyr special functions
#'
#' @return a data frame with duplicated rows by given columns
#'
#' @export
#'
#' @seealso \code{\link{remove_duplicated}} to remove duplicates
#'
#' @examples
#' d <- data.frame(x1 = rep(1:3, each=4), x2 = rep(1:4, each=3), z = rep(1:2, 6), a = rep(1:6, 2))
#' complete_cases(data = d, x2, z)
#' complete_cases(d, a, z)
#' complete_cases(d)
#' complete_cases(d, starts_with("x"))
#'

complete_cases <- function(data, ...){
  "Removes NA observations by column(s)"
  
  assertthat::assert_that(!missing(data), msg = "Input data is missing")
  assertthat::assert_that(is.data.frame(data))

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
  subset(data, complete.cases(d))
}
