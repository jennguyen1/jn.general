# todo check col names


# concats the serotypes into 1 logic str
logic_sero <- function(x){
  x %>%
    stringr::str_split(", ") %>%
    purrr::map_chr(~ paste0("'", .x, "'", collapse = ", ")) %>%
    paste0("serotype %in% c(", ., "))")
}

# concats file id into 1 logic str
logic_file_id <- function(x){
  paste0("(processed_data_id == '", x,  "'")
}

subset_to_valid_sero <- function(d, file_map){
  # logic cmd
  logic_expr <- file_map %>%
    dplyr::mutate(
      part1 = logic_file_id(processed_file_name),
      part2 = logic_sero(serotypes),
      part3 = paste(part1, "&", part2)
    ) %>%
    dplyr::pull(part3) %>%
    paste(collapse = " | ")

  # full subset cmd
  subset_expr <- paste0("subset(d, ", logic_expr, ")")

  # evaluate subset & return
  eval(parse(text = subset_expr))
}


