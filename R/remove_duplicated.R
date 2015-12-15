
#' Removes Duplicated Rows
#'
#' Removes duplicated data by given columns
#'
#' @param data a data frame
#' @param ... columns to search for duplicated values; defaults to all arguments
#' @param opt_delete string, options for deletion; takes the values "from first", "from last", or "all". "all" deletes all duplicated entries, "from first" keeps the first duplicate and deletes the rest, "from last" keeps the last duplicate and deletes the rest. Default is "from first".
#' @param opt_summary boolean, whether to print drop summaries
#'
#' @return Returns a data table with duplicated rows given by columns removed
#'
#' @export
#'
#' @seealso \code{\link{view_duplicated}} to look at duplicates (but not remove)
#'
#' @examples
#' remove_duplicated(iris, Species, opt_delete = "from last")
#'

remove_duplicated <- function(data, ..., opt_delete = "from first", opt_summary = TRUE){

  # split data up into duplicates and non-duplicates
  split_data <- jn.general::to_be(data, jn.general::view_duplicated, ...)

  # save duplicates and non-duplicates
  dups <- split_data$to_be
  no_dups <- split_data$not_to_be

  # remove duplicates (all duplicates)
  if(opt_delete == "all"){
    out_data <- data.table(no_dups)

  # remove duplicates with options
  } else {
    # convert data to data table if it is not already
    if( !data.table::is.data.table(dups) ) dups <- data.table::data.table(dups)

    # set the key to use for duplicates
    keys <- as.character(substitute(list(...))[-1])
    if( any(!(keys %in% colnames(dups))) ) stop("Columns specified not in input data")
    suppressWarnings( data.table::setkeyv(dups, keys) )

    # delete duplicates from last (keep the last duplicated value and delete the rest)
    if(opt_delete == "from last"){
      out_data <- subset(dups, !duplicated(dups, fromLast = TRUE))

    # delete duplicates from first (keep the first duplicated value and delete the rest)
    } else if (opt_delete == "from first") {
      out_data <- subset(dups, !duplicated(dups))

    # error when any other delete option given
    } else{
      stop("Invalid delete option")
    }

    # combined unique values (after duplicate deletion) with no_dups
    out_data <- data.table::rbindlist( list(out_data, no_dups) )
  }

  # print summary of drops
  if(opt_summary){
    # find sample sizes
    n_dups <- nrow(dups)
    left_over <- nrow(data) - nrow(out_data)

    # print summary of drops
    print( paste0(n_dups, " duplicates were found") )
    print( paste0(left_over, " duplicates were dropped") )
  }

  # return final data
  return(out_data)

}
