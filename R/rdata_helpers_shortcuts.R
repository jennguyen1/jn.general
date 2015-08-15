
#' rdata Argument Generators
#'
#' Shortcut rdata generators. Values are predefined.
#'
#' @return an rdata_generator list object that can be passed into \code{rdata} to generate a columns of the random data frame
#'
#' @details
#' Default values: \cr
#'
#' gen_gender: male, female \cr
#' gen_age: integers from 0 to 100 \cr
#' gen_race: white, black, hispanic, asian, native, multi \cr
#' gen_directions: north, west, south, east \cr
#' gen_state: all US states; for full name set kind = "full", for abbreviations set kind = "abb" \cr
#' gen_rank: low, medium, high \cr
#' gen_subject: math, science, read, ss \cr
#' gen_month: months of the year; for full name set kind = "full", for abbreviations set kind = "abb" \cr
#' gen_year: sequence of integers from 1500 to current year; start and end year are customizable \cr
#'
#' @name shortcut_generators
NULL

######################
# human demographics #
######################

#' @rdname shortcut_generators
#' @export
gen_gender <- function(ncol = 1, name = "gender", probs = NULL, add.na = FALSE, as.factor = TRUE) {
  gen_char(name = name, values = c("male", "female"), ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor)
}

#' @rdname shortcut_generators
#' @export
gen_age <- function(ncol = 1, name = "age", replace = TRUE, add.na = FALSE, as.factor = FALSE){
  gen_num(name = name, values = 0:100, ncol = ncol, replace = replace, add.na = add.na, as.factor = as.factor)
}

#' @rdname shortcut_generators
#' @export
gen_race <- function(ncol = 1, name = "race", replace = TRUE, probs = NULL, add.na = FALSE, as.factor = TRUE){
  gen_char(name = name, values = c("white", "black", "hispanic", "asian", "native", "multi"), ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor)
}

##########
# places #
##########

#' @rdname shortcut_generators
#' @export
gen_directions <- function(ncol = 1, name = "directions", probs = NULL, add.na = FALSE, as.factor = TRUE) {
  gen_char(name = name, values = c("north", "west", "south", "east"), ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor)
}

#' @rdname shortcut_generators
#' @export
gen_state <- function(ncol = 1, name = "state", kind = "full", probs = NULL, add.na = FALSE, as.factor = TRUE){
  state <- switch(kind, full = state.name, abb = state.abb, stop("Invalid kind argument"))
  return( gen_char(name = name, values = state, ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor) )
}

##########
# things #
##########

#' @rdname shortcut_generators
#' @export
gen_rank <- function(ncol = 1, name = "rank", probs = NULL, add.na = FALSE, as.factor = TRUE){
  gen_char(name = name, values = c("low", "medium", "high"), ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor)
}

#' @rdname shortcut_generators
#' @export
gen_subject <- function(ncol = 1, name = "subject", probs = NULL, add.na = FALSE, as.factor = TRUE){
  gen_char(name = name, values = c("math", "read", "science", "ss"), ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor)
}

###############
# time & date #
###############

#' @rdname shortcut_generators
#' @export
gen_month <- function(ncol = 1, name = "month", kind = "full", probs = NULL, add.na = FALSE, as.factor = TRUE){
  month <- switch(kind, full = month.name, abb = month.abb, stop("Invalid kind argument"))
  return( gen_char(name = name, values = month, ncol = ncol, probs = probs, add.na = add.na, as.factor = as.factor) )
}

#' @rdname shortcut_generators
#' @export
gen_year <- function(start = 1500, end = format(Sys.Date(), "%Y"), ncol = 1, name = "year", replace = TRUE, add.na = FALSE, as.factor = FALSE) {
  gen_seq(start = start, end = end, by = 1, name = name, ncol = ncol, replace = replace, add.na = add.na, as.factor = as.factor)
}
