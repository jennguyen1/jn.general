
#' Merge Multiple Data Frames
#'
#' @param ... data frames to be merged or one list of all data frames to be merged
#' @param by vector of shared column names to merge on or list of vectors where each element of list refers to the by value for the ith merge
#' @param f merge function, valid options are dplyr::inner_join, dplyr::left_join, dplyr::right_join, dplyr::full_join, dplyr::semi_join, dplyr::anti_join, base::merge (default options)
#' @param suffixes vector specifying suffixes to be used for making unique the names of columns in the result which are not used for merging
#' @return A data frame
#'
#' @export
#'
#' @examples
#' # generate more random data
#' y1 <- data.frame(id = rep(1:5, 3), val = rnorm(15))
#' y2 <- data.frame(id = 1:15, id2 = rep(c("A", "B", "C"), 5), est = rnorm(15))
#' y3 <- data.frame(id = 1:15, id2 = rep(c("A", "B", "C"), 5), var = runif(15))
#'
#' # merge by various attributes
#' merge_mult(y1, y2, y3, by = list("id", c("id", "id2")))
#' # equivalent to:
#' merge(y1, y2, by = "id") %>% merge(y3, by = c("id", "id2"))

merge_mult <- function(..., by, f, suffixes = letters){
  l <- list(...)
  if( !is.data.frame(l[[1]]) ) l <- l[[1]] # TODO did they apply a second list?

  ## checks
  assertthat::assert_that(all(lapply(l, is.data.frame)), msg = "Inputs should be data frames or a list of data frames")
  assertthat::assert_that(length(l) >= 2, msg = "At least 2 data frames must be supplied for merge")
  assertthat::assert_that(!missing(by), msg = "Missing by argument")
  
  assertthat::assert_that(!missing(f), msg = "Missing f argument")
  merge_options <- c(dplyr::inner_join, dplyr::left_join, dplyr::right_join, dplyr::full_join, dplyr::semi_join, dplyr::anti_join, base::merge)
  valid_function <- any(unlist(lapply(merge_options, function(x) identical(x, f))))
  assertthat::assert_that(valid_function, msg = "Invalid merge function")
  
  assertthat::assert_that(length(suffixes) == length(l), msg = "The number of suffixes must be equal to the number of data frames")
  assertthat::assert_that((length(by) == 1) | (length(l) - length(by) == 1), msg = "Length of by argument must be either 1 or 1 less than the total data frames") # enough by keys for merge

  ## function work
  no_suffix_merges <- c(dplyr::semi_join, dplyr::anti_join)
  f_is_no_suffix_merge <- any(unlist(lapply(no_suffix_merges, function(x) identical(x, f))))
  
  use_suffix <- paste0("_", suffixes)
  if(length(by) == 1) by <- rep(by, length(l)-1) # maintain consistency with a list of by vars

  if(length(l) == 2){
    merged <- if(f_is_no_suffix_merge) f(l[[1]], l[[2]], by = by[1]) else f(l[[1]], l[[2]], by = by[1], suffix = use_suffix)
  } else{
    for(i in 2:length(l)){
      use_suffixes <- if(i == length(l)){ # not last merges
        c(use_suffix[i-1], "")
      } else{ # last merges
        use_suffix[(length(l)-1):length(l)]
      }
      merged <- if(f_is_no_suffix_merge) f(l[[1]], l[[2]], by = by[1]) else f(l[[1]], l[[2]], by = by[1], suffix = use_suffixes)
    }
  }
  return(merged)
}
