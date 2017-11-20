
aux_rename_list <- function(x, name_entry){
  if( is.data.frame(x) ){
    new_x_df <- dplyr::mutate(x, name_id = name_entry)
    new_x_df <- dplyr::select(new_x_df, name_id, dplyr::everything())
    return(new_x_df)

  } else if( is.list(x)){
    new_x_list <- lapply(x, aux_rename_list, name_entry = name_entry)
    return(new_x_list)

  } else{
    return(x)

  }
}
