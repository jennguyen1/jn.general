
extraction <- function(list_data, rbind, recursive){
  list_data <- Filter(function(x) !is.null(x), list_data)
  model <- list_data[[1]]

  # determine whether continue recursion: continue if the structure at each iteration is a list
  # note: cannot do !is.list(model) since a data frame is considered a list so it would cause incomplete results
  if( is.atomic(model) | is.matrix(model) | is.data.frame(model) | is.function(model) ){
    if( rbind & is.data.frame(model) ) return( dplyr::bind_rows(list_data) ) else return( list_data )
  }
  varname <- names(model)

  # for every unique variable name, extracts named element from every iterative element in list_data
  # 2 loops:
  ## outer loop: loop over varname
  ## inner loop: extract varname from the various iterated elements of list_data
  out <- lapply( varname, function(var) lapply(1:length(list_data), function(i) list_data[[i]][[var]]) )
  names(out) <- varname

  if(recursive) out <- lapply(out, extraction, rbind = rbind, recursive = recursive) # runs recursively
  return(out)
}
