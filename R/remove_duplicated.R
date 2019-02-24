
#' Removes Duplicated Rows
#'
#' Removes duplicated data by column(s)
#'
#' @param data a data frame
#' @param ... columns to search for duplicated values; defaults to all arguments
#' @param opt_keep string, which value to keep; takes the values "first", "last", or "none". "none" deletes all duplicated entries, "first" keeps the first duplicate, "last" keeps the last duplicate. Default is "first".
#' @param opt_summary boolean, whether to log drop summaries
#'
#' @return Returns a data frame with duplicated rows given by columns removed
#'
#' @export
#'
#' @import dplyr
#' @seealso \code{\link{view_duplicated}} to look at duplicates (but not remove)
#'
#' @examples
#' d <- data.frame(x = rep(1:3, each=4), y = rep(1:4, each=3), z = rep(1:2, 6), a = rep(1:6, 2))
#' view_duplicated(d, y, z)
#' remove_duplicated(d, y, z)

remove_duplicated <- function(data, ..., opt_keep = "first", opt_summary = TRUE){
  "Remove duplicates by column(s)"

  assertthat::assert_that(!missing("data"), msg = "Missing data argument")
  assertthat::assert_that(opt_keep %in% c("none", "first", "last"), msg = "Invalid delete option")
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.logical(opt_summary), msg = "opt_summary is not a boolean")

  split_data <- to_be(data, view_duplicated, ...)
  dups <- split_data$to_be
  no_dups <- split_data$not_to_be

  if(opt_keep == "none"){
    out_data <- no_dups

  } else {
    group_vars <- as.character(substitute(list(...))[-1])

    if(length(group_vars) == 0){
      fix_dups <- subset(dups, !duplicated(dups))
    } else{
      filter_f <- switch(
        opt_keep,
        "first" = head,
        "last" = tail
      )
      fix_dups <- dups %>%
        dplyr::group_by_at(group_vars) %>%
        tidyr::nest() %>%
        dplyr::mutate(d = purrr::map(data, ~ filter_f(.x, 1))) %>%
        dplyr::select(-data) %>%
        tidyr::unnest()
    }

    out_data <- rbind(fix_dups, no_dups)
  }
  out_data <- subset(out_data, select = colnames(data))

  if(opt_summary){
    n_dups <- nrow(dups)
    left_over <- nrow(data) - nrow(out_data)
    logging::loginfo( paste(n_dups, "duplicates were found") )
    logging::loginfo( paste(left_over, "duplicates were dropped") )
  }
  out_data
}
