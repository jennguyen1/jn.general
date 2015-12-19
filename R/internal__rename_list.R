
# internal function for list renaming
aux_rename_list <- function(x, name_entry){
  # if data frame, add column
  if( is.data.frame(x) ){

    new_x_df <- dplyr::mutate(x, file_id = name_entry)
    new_x_df <- dplyr::select(new_x_df, file_id, everything())
    return(new_x_df)

    # if item is a list, recurse through and rename
  } else if( is.list(x)){

    new_x_list <- lapply(x, aux_rename_list, name_entry = name_entry)
    return(new_x_list)

    # if not a list or data frame, do nothing, return
  } else{

    return(x)

  }
}
