
# internal function for extraction
extraction <- function(list_data, rbind, fill, recursive){

  # removes all na values
  list_data <- jn.general::rm_na_list(list_data)

  # model iteration: since list_data is an iterative list, 1st element is the same as others
  model <- list_data[[1]]

  # determine whether continue recursion: continue if the structure at each iteration is a list
  # note: cannot do !is.list(model) since a data frame is considered a list so it would cause incomplete results
  if( is.atomic(model) | is.matrix(model) | is.data.frame(model) | is.function(model) ){
    if( rbind & is.data.frame(model) ) return( dplyr::bind_rows(list_data) ) else return( list_data )
  }

  # since list_data is an iterative list, then the variable names of the first element is the same for all elements
  varname <- names(model)

  # for every unique variable name, extracts the element named varname from every iterative element in list_data
  # 2 loops:
  ## outer loop: loop over varname
  ## inner loop: extract varname from the various iterated elements of list_data
  out <- lapply( varname, function(var) lapply(1:length(list_data), function(i) list_data[[i]][[var]]) )

  # final result should be a list where each element is the data from the unique variable name
  names(out) <- varname

  # recursively runs function until it encounters a non-list element
  if(recursive) out <- lapply(out, extraction, rbind = rbind, fill = fill, recursive = recursive)

  # returns results
  return(out)
}
