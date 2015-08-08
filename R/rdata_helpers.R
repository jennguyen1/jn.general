
#' rdata Argument Generators
#'
#' Generate the list with the appropriate column specifications to pass to \code{\link{rdata}}.
#' Column attributes and sampling specifications are fully customizable. \cr \cr
#' Currently there are 3 available column types: numeric(gen_num), character(gen_char), and boolean(gen_bool).
#' gen_random chooses one of these three default generators at random. \cr \cr
#' Additional customizable generators allow for generating a sequence (gen_seq) and choosing random values from a specified distribution (gen_dist). \cr \cr
#' Predefined \code{\link{shortcut_generators}} for commonly used values are also available.
#'
#' @param ncol how many columns of these specific attributes to generate; defaults to 1
#' @param values vector of values to sample from
#' @param start,end starting and end values of sequence to sample from
#' @param dist random distribution generator function, such as rnorm, runif, etc
#' @param ... additional arguments depending on generate function. See details
#' @param name name(s) of the column(s) for the rdata; length should be 1 or equal to ncol
#' @param replace logical; if TRUE values will be sampled with replacement
#' @param probs vector of probability weights for obtaining the elemnts of the vector to be sampled
#' @param add.na logical; if TRUE NA list of possible values altered to contain NA
#' @param as.factor logical; if TRUE the column will be converted to factor column
#'
#' @details
#' For gen_seq, additional arguments can be passed to the seq function. \cr
#'
#' For gen_dist, additional arguments for parameters of distribution. Not required if the random generator already has defaults.
#'
#' @return an rdata_generator list object that can be passed into \code{rdata} to generate a columns of the random data frame
#'
#' @examples
#' # generates templates for rdata with given specifications
#' ex1 <- gen_num(name = "x", values = 1:20, replace = FALSE, add.na = TRUE)
#' ex2 <- gen_char(3, letters, c("a", "b", "c"))
#' ex3 <- gen_bool(name = "true_or_false", probs = c(.75, .25))
#' gpa <- gen_seq(0,4,.5, name = "gpa")
#' rand_normal <- gen_dist(rnorm)
#' rand_pois <- gen_dist(rpois, lambda = 4)
#' rand_unif <- gen_dist(runif, 0, 5)
#'
#' # shortcut generators
#' state <- gen_state(kind = "abb")
#' state2 <- gen_state(kind = "full")
#' gender <- gen_gender(name = "gender")
#' random <- gen_random(4)
#'
#' @name generate
NULL

############################
# main generation function #
############################
generate <- function(name, values, ncol, replace, probs, add.na, as.factor){
  # exception handling: name argument can only be equal to 1 or the number of columns
  if( length(name) != 1 & ncol != length(name) ) stop("Invalid name arguments")

  # exception handling: length of probability weights must be equal to length of values (including add.na option)
  if(add.na) values <- c(values, NA)
  if( !is.null(probs) & length(probs) != length(values) ) stop("Length of probs not equal to length of values (including add.na option)")

  # conversion to factors
  if(as.factor) values <- factor(values)

  # generates enough names, nval, and replace values for the ncols
  if(ncol > 1 & length(name) == 1) name <- name %p% "_" %p% 1:ncol
  nval <- rep(length(values), ncol)
  replace <- rep(replace, ncol)

  # generate2 rdata arguments
  if( length(values) == 1){
    generator <- list( pryr::partial(rep, x = values) )
  } else{
    generator <- list( pryr::partial(sample, x = values, prob = probs) )
  }
  generator <- rep(generator, ncol)

  # generates list for rdata
  l <- list(name = name, col = generator, nvalues = nval, replace = replace)
  class(l) <- c("list", "rdata_generator")

  # return rdata arguments
  return(l)
}


######################
# general generators #
######################

#' @rdname generate
#' @export
gen_char <- function(ncol = 1, values = LETTERS, name = "X", replace = TRUE, probs = NULL, add.na = FALSE, as.factor = FALSE){
  if(!is.character(values)) stop("values are not of type character")
  generate(name = name, ncol = ncol, values = values, replace = replace, probs = probs, add.na = add.na, as.factor = as.factor)
}

#' @rdname generate
#' @export
gen_num <- function(ncol = 1, values = seq(-10, 10, length.out = 150), name = "X", replace = TRUE, add.na = FALSE, as.factor = FALSE) {
  if(!is.numeric(values)) stop("values are not of type numeric")
  generate(name = name, ncol = ncol, values = values, replace = replace, probs = NULL, add.na = add.na, as.factor = as.factor )
}

#' @rdname generate
#' @export
gen_bool <- function(ncol = 1, name = "X", probs = NULL, add.na = FALSE, as.factor = FALSE) {
  generate(name = name, ncol = ncol, values = c(TRUE, FALSE), replace = TRUE, probs = probs, add.na = add.na, as.factor = as.factor )
}

#' @rdname generate
#' @export
gen_random <- function(ncol = 1, name = "random", add.na = FALSE, as.factor = FALSE){
  types <- c("numeric", "character", "boolean")
  use <- sample(types, 1)
  surprise <- switch(use,
                     numeric = gen_num(name = name, ncol = ncol, add.na = add.na, as.factor = as.factor),
                     character = gen_char(name = name, ncol = ncol, add.na = add.na, as.factor = as.factor),
                     boolean = gen_bool(name = name, ncol = ncol, add.na = add.na, as.factor = as.factor)
                     )
  return(surprise)
}

#' @rdname generate
#' @export
gen_seq <- function(start, end, ..., ncol = 1, name = "X", replace = TRUE, add.na = FALSE, as.factor = FALSE){
  values <- seq(start, end, ...)
  gen_num(name = name, values = values, ncol = ncol, replace = replace, add.na = add.na, as.factor = as.factor)
}

#' @rdname generate
#' @export
gen_dist <- function(dist, ..., ncol = 1, name, replace = FALSE, add.na = FALSE, as.factor = FALSE){

  # obtain info about the name of the random generator
  rand_func_name <- deparse(substitute(dist))
  if( stringr::str_sub(rand_func_name, 1, 1) != "r" ) stop("This is not a random generation distribution function")

  # generates random values
  values <- dist(n = 500, ...)

  # if no name supplied, names it after the distribution
  if(missing(name)) name <- rand_func_name

  # generates random data
  gen_num(name = name, values = values, ncol = ncol, replace = replace, add.na = add.na, as.factor = as.factor)
}
