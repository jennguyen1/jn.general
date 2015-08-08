
#' Reorder Columns in Data Frame
#'
#' Brins selected columns at the beginning and adds other columns at the end.
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

  # uses to_be to extract the columns for the beginning
  ordered_df <- jn.general::to_be(df, dplyr::select, ...)

  # combines the ordered columns at the beginning and non-ordered columns at the end
  df_out <- cbind(ordered_df[["to_be"]], ordered_df[["not_to_be"]])

  # return results
  return(df_out)
}
