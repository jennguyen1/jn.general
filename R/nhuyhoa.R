
#' Functions for nhuyhoa jekyll blog
#'
#' Functions to use when running jekyll blog
#'
#' @name nhuyhoa
NULL

#' @rdname nhuyhoa
#' @export
nhuyhoa <- function() servr::jekyll(dir = ".", input = c(".", "_source", "_posts"),
                                    output = c(".", "_posts", "_posts"), script = c("build.R"),
                                    serve = TRUE, command = "jekyll build")

#' @rdname nhuyhoa
#' @export
nhuyhoa_df_print <- function(df, head = 5, data = TRUE, attribute = "class = \"presenttab\"", ...){
  # for printing data
  if(data){
    df %>% head(head) %>% knitr::kable(format = "html", align = "c", ...)

  # for presentating tables
  } else{
    df %>% head(head) %>% knitr::kable(format = "html", table.attr = attribute, ...)
  }
}
