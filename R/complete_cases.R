
#' Removes NA Observations By Column
#'
#' Deprecated. Use tidyr::drop_na.
#'
#' @param data a data frame
#' @param ... columns to remove NA obs, defaults to all arguments; can also apply dplyr special functions
#'
#' @return a data frame
#'
#' @export
#'
#'

complete_cases <- function(data, ...){
  stop("Deprecated. Use tidyr::drop_na")
}
