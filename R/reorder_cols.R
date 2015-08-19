
#' Reorder Columns in Data Frame
#'
#' Places specified columns at the front or end and includes other variables.
#' Compatible with dplyr's special select functions.
#'
#' @param df a data frame
#' @param ... arguments to reorder; arguments should be in the order they
#' should appear on the ordered data frame
#'
#' @return a data frame whose columns are ordered as specified
#'
#' @export
#'
#' @examples
#' df <- plyr::rdply(5, rnorm(26))
#' colnames(df) <- c("id", letters)
#' reorder_cols(df, matches("[aeiou]"))

reorder_cols <- function(df, ...){

  # include everything() to the specifications
  df_out <- dplyr::select(df, ..., everything())
  
  # return results
  return(df_out)
}
