
rm_dataframe <- function(v){
  index <- which(stringr::str_detect(v, "data.frame"))
  add <- as.numeric(stringr::str_replace(stringr::str_extract(v[index], "\\d+ var"), " var", ""))
  rm_index <- unlist(Map(function(x,y) (x+1):(x+y), index, add))
  if(is.null(rm_index)) v else v[-rm_index]
}

check_names <- function(e){
  s <- rm_dataframe(capture.output(str(e)))[-1]
  clean <- stringr::str_sub(s, 1, stringr::str_locate(s, ":")[,"start"])
  stringr::str_subset(clean, "[ .]*\\$")
}
