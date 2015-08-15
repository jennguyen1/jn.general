
#' Extract Data Accumulated in a List
#'
#' Loops through an iterative list and rearranges data structures. Accumulates structures of the same
#' name into a subsetted list which can be accessed using \code{$}. \cr
#' See \code{\link{is.iterative.list}} to see what qualifies.
#'
#' @param list_data a list
#' @param rbind logical; if TRUE rbinds/stacks all extracted elements into a data.table (if they are data frames);
#' else they are returned as lists. Elements that are not data frames are returned as lists
#' @param fill logical; only used if rbind = TRUE; if TRUE fills missing columns with NA
#' @param recursive logical; if TRUE extraction applied to list components of x
#'
#' @return a list
#'
#' @import magrittr
#' @export
#'
#' @examples
#' # generate data
#' x <-
#' list(
#'   WI = list(
#'     circle = list(a = matrix(1:6, nrow = 2), b = matrix(1:10, nrow = 2)),
#'     triangle = list(c = matrix(1:14, nrow = 2), d = list(four = 1:25)),
#'     square = list(e = data.frame(x = 1:10, y = 1:10), f = data.frame(i = 1:10, j = 1:10))
#'   ),
#'   MN = list(
#'    hot = list(red = data.frame(x = 1:10, y = 1:10), orange = data.frame(i = 1:10, j = 1:10)),
#'     cold = list(yellow = data.frame(x = 1:10, y = 1:10), green = data.frame(i = 1:10, j = 1:10))
#'   ),
#'   CA = matrix(-10:9, nrow = 4),
#'   WA = LETTERS
#' )
#' master <- list()
#' for(i in 1:3) master[[i]] <- x
#'
#' # run extraction
#' extract_list(master, recursive = FALSE)
#' extract_list(master, rbind = FALSE)
#' extract_list(master, rbind = TRUE)
#'
#'
#' # generate data
#' regression <- function(model_descr){
#'   coeffs <- matrix(1:100, nrow = 50)
#'   coeffs[,1] <- model_descr
#'   var <- data.frame(model_descr = model_descr, a = runif(14), b = rnorm(14))
#'   resids <- rdata(56, gen_char(name = "model_descr", values = model_descr), gen_num(rnorm(50), ncol = 3), .id = FALSE)
#'   r2 <- list(a = rdata(20, gen_char(name = "model_descr", values = model_descr), gen_bool(ncol = 3), gen_num(), .id = FALSE),
#'              b = data.frame(model_descr = model_descr, value = 1:5)
#'              )
#'   return(list(coeffs = coeffs, var = var, resids = resids, r2 = r2))
#' }
#'
#' within_r2 <- function(model_descr){
#' 	x = data.frame(model_descr = model_descr, resid_var = rnorm(1), within_r2 = sample(1:10, 1))
#' 	return(x)
#' }
#'
#' # run analysis on different models
#' model_descr <- paste0("Model_", 1:5)
#' output <- lapply(1:5, function(i){
#' 	model <- model_descr[i]
#' 	rg <- regression(model)
#' 	r2 <- within_r2(model)
#' 	if(i%%2==0) test_null <- NULL else test_null <- c(1,2,3)
#' 	return( list(regression = rg, r2 = r2, test_null = test_null) )
#' })
#'
#' # extract data
#' extract <- extract_list(output)
#'
#' # template for extraction: extract using subsetting '$'
#' all_coefs <- extract$regression$coeffs
#' all_r2_a <- extract$regression$r2$a
#'
#' # what happens if one of the models in an iterations fails/is null?
#' # it will only take non-null values!
#' test_null <- extract$test_null
#'
extract_list <- function(list_data, rbind = TRUE, fill = TRUE, recursive = TRUE){

  # confirms that list is an iterative list;
  # recursive calls are guaranteed to be an iterative list so only need to run this once
  if( !jn.general::is.iterative.list(list_data) ) stop("input list not an iterative list")

  # runs main extraction function
  e <- extraction(list_data, rbind = rbind, fill = fill, recursive = recursive)

  # runs output
  return(e)
}