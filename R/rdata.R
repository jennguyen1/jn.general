
#' Random Data Frame with Specified Column Types
#'
#' Generate a random data frame with a specified number of rows and randomly generated columns.
#' Column specifications are generated via \code{\link{generate}} functions.
#'
#' @param n number of rows for the new data frame
#' @param ... column specifications; use rdata helpers: \link{generate}
#' @param .id logical; if TRUE, generate a unique numeric id for each row; else no id
#'
#' @return A data frame
#'
#' @export
#' @import magrittr
#'
#' @examples
#' rdata(n = 50, gen_random(ncol = 2))
#'
#'# generate lists
#'ex1 <- gen_num(name = "x", values = 1:20, replace = FALSE, add.na = TRUE)
#'ex2 <- gen_char(3, letters, c("a", "b", "c"))
#'ex3 <- gen_bool(name = "true_or_false", probs = c(.75, .25))
#'state1 <- gen_state(kind = "abb")
#'state2 <- gen_state(kind = "full")
#'gender <- gen_gender(name = "sex")
#'
#'# generate random data frame
#'rdata(n = 100, ex1, ex2, ex3, gender, state1, state2, .id = FALSE)
#'

rdata <- function(n, ..., .id = TRUE)
{

  ########################
  # intialize parameters #
  ########################

  # check n
  if( missing(n) ) stop("Missing n argument")
  if( !is.numeric(n) ) stop("Invalid n value")
  if( length(n) != 1 ) stop("Please specify only 1 n value")
  
  # check l args
  l <- list(...)
  if( length(l) == 0 ) stop("Insufficient rdata generator arguments, must supply at least 1 column.")

  # checks that the column arguments were generated using the rdata generators
  # used for loop because lapply doesn't let you break out of loop
  for(i in 1:length(l)){
    if( all(class(l[[i]]) != "rdata_generator") ){
      stop("Invalid column arguments, please use the rdata generators.")
    }
  }

  # extract column specificiations
  # l is an iterative list due to class restrictions - run through internal extraction so it's faster
  extract <- extraction(l, rbind = FALSE, fill = FALSE, recursive = FALSE)

  # extract
  names <- unlist(extract$name)
  cols <- unlist(extract$col)
  nvalues <- unlist(extract$nvalues)
  replace <- unlist(extract$replace)

  # replace fix: if there are too many sample values than the requested size, replace must be TRUE
  # otherwise it can be as requested
  replace[which(nvalues < n)] <- TRUE

  ##########################
  # random data generation #
  ##########################

  # generate the new data
  new_data <- Map(function(f,r, nval) if(nval == 1) f(times = n) else f(size = n, replace = r),
                  cols, replace, nvalues) %>% do.call("data.frame", .)

  colnames(new_data) <- names

  # make an id column
  if(.id) new_data <- data.frame(id = 1:n, new_data)

  # return new data frame
  return(new_data)
}
