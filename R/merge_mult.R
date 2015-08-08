
#' Merge Multiple Data Frames
#'
#' Fast merge multiple data frames, via data.table merge method
#'
#' @param ... data frames to be merged
#' @param by vector of shared column names to merge on or list of vectors where each element of list refers to the by value for the ith merge
#' @param all logical; if FALSE, then an inner join on all data frames; else a full outer join on all data frames.
#' @param all.x logical; if TRUE, then a left outer join on the first data frame listed
#' @param suffixes vector specifying suffixes to be used for making unique the names of columns in the result which are not used for merging
#' @return A data table
#'
#' @export
#'
#' @examples
#' # generate random data
#' x1 <- rdata(n = 28, gen_random())
#' x2 <- rdata(n = 11, gen_random())
#' x3 <- rdata(n = 54, gen_random(2))
#' x4 <- rdata(n = 9, gen_random())
#'
#' # merge by "id"
#' merge_mult(x1, x2, x3, x4, by = "id")
#' # equivalent to:
#' merge(x1, x2, by = "id") %>% merge(x3, by = "id") %>% merge(x4, by = "id")
#'
#' # merge by "id", keep all of the 1st df
#' merge_mult(x1, x2, x3, x4, by = "id", all.x = TRUE)
#' # equivalent to:
#' merge(x1, x2, by = "id", all.x = TRUE) %>% merge(x3, by = "id", all.x = TRUE) %>% merge(x4, by = "id", all.x = TRUE)
#'
#' # generate more random data
#' y1 <- data.frame(id = rep(1:5, 3), val = rnorm(15))
#' y2 <- data.frame(id = 1:15, id2 = rep(c("A", "B", "C"), 5), est = rnorm(15))
#' y3 <- data.frame(id = 1:15, id2 = rep(c("A", "B", "C"), 5), var = runif(15))
#'
#' # merge by various attributes
#' merge_mult(y1, y2, y3, by = list("id", c("id", "id2")))
#' # equivalent to:
#' merge(y1, y2, by = "id") %>% merge(y3, by = c("id", "id2"))

merge_mult <- function(..., by, all = FALSE, all.x = NULL, suffixes = letters){

  ##################
  # Error Checking #
  ##################

  # generate the list of data tables
  l <- list(...)

  # check that arguments were passed
  if( length(l) < 2 ) stop("At least 2 data frames must be supplied for merge")
  if( missing(by) ) stop("Missing by argument")

  # check that there are enough suffixes for the list
  if(length(suffixes) < length(l)) stop("The number of suffixes must be equal than the number of data frames")

  # check that by is either 1 key or has enough keys for the merge
  if( !((length(by) == 1) | (length(l) - length(by) == 1)) ) stop("Length of by argument must be either 1 or 1 less than the total data frames")

  #################
  # Function Work #
  #################

  # add prefix to suffixes
  suffix <- paste0("_", suffixes)

  # initalize the merge as a data.table
  merged <- data.table::data.table(l[[1]])

  # if there is 1 id for all merge, makes it a replicated list to ensure consistency
  if(length(by) == 1) by <- rep(by, length(l)-1)

  # merge all the data frames via a for loop (faster than Reduce); maintain suffixes
  for(i in 2:length(l)){
    if(is.null(all.x)){
      merged <- merge(merged, l[[i]], by = by[[i-1]], all = all, suffix = c(suffix[i-1], suffix[i]))
    } else{
      merged <- merge(merged, l[[i]], by = by[[i-1]], all.x = all.x, suffix = c(suffix[i-1], suffix[i]))
    }
  }
  # note: must do all and all.x separately; doing them together in one call yields inconsistent results

  # return the results
  return(merged)
}
