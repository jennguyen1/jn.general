
#' Group Vector by its Percentiles
#'
#' Given the number of equally spaced bins, groups a vector by its percentiles
#'
#' @param x a numeric vector
#' @param opt_n_bins int, number of bins/groups to generate. Percentile splits are equally spaced
#' @param group_prefix string, the prefix to add to the bin labels
#'
#' @return a vector with opt_n_bins factors
#' @export
#'
#' @examples
#' x <- rpois(20, 4)
#' percentile_bin(x)

percentile_bin <- function(x, opt_n_bins = 5, group_prefix = "group "){

  # error checking
  if(!is.numeric(x)) stop("x must be a numeric vector")

  # generate quantile bins
  perc <- quantile( x, probs = seq(0,1,1/opt_n_bins) )
  bins <- cut( x, breaks = perc, include.lowest = TRUE )

  # make crosswalk for the bins and the labels
  bin_factors <- levels(bins)
  num <- 1:opt_n_bins
  percentiles <- paste0(group_prefix, num, ": ", bin_factors)

  # convert bins rangers (not interpretable) into labeled groupings
  out_bins <- plyr::mapvalues(as.character(bins), bin_factors, percentiles)

  # return values
  return(out_bins)
}

