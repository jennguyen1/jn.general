#' Functions for nhuyhoa jekyll blog
#'
#' Functions to use when running jekyll blog
#'
#' @details
#'
#' nhuyhoa() implements changes to the jekyll blog \cr
#'
#' nhuyhoa_df_print() formatting for tables on blog \cr
#'
#' run_recipes() generates RMD files for recipes \cr
#'
#' @name nhuyhoa
NULL


#' @rdname nhuyhoa
#' @export
nhuyhoa <- function(recipes = FALSE){

  # run recipes
  if(recipes) run_recipes()

  # jekyll blog
  servr::jekyll(dir = ".", input = c(".", "_source", "_posts"),
                output = c(".", "_posts", "_posts"), script = c("build.R"),
                serve = TRUE, command = "jekyll build")
}


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


#' @rdname nhuyhoa
#' @export
run_recipes <- function(){

  connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "../_data/recipes.db")
  recipes <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM recipes")
  RSQLite::dbDisconnect(conn = connect)

  # extract recipe pics
  recipe_info <- recipes %>%
    purrrlyr::by_row(function(r){
      pat <- r$recipe %>%
        stringr::str_replace(" \\(.*", "") %>%
        stringr::str_replace_all(" ", "_")
      pics <- list.files("figure/food/", pattern = pat)
      stringr::str_subset(pics, "JPG")
    }, .to = "pictures")


  # generate recipe RMD files for website
  make_script <- function(df){

    name <- df$recipe[1]

    # recipe pictures - format for markdown/html
    pictures <- df$pictures[[1]]
    use_image <- pictures %>%
      paste0("![pic", 1:length(.), ']( {{"/figure/food/', ., '" | absolute_url }})') %>%
      paste(collapse = "\n\n")
    use_image <- ifelse(length(pictures) == 0, "", use_image)

    # recipe youtube - format for markdown/html
    youtube <- df$youtube
    use_video <- ""
    if( !is.na(youtube) ) use_video <- paste0("[![youtube](http://img.youtube.com/vi/", youtube, "/0.jpg)](http://www.youtube.com/watch?v=", youtube, ")")

    # RMD template
    script <- c("---
layout: post
title: \"", name, "\"
date: \"May 15, 2017\"
categories: ['recipes', '", df$meal_type, "']
---

```{r, echo = FALSE, warning = FALSE}
library(jn.general)
lib(data)

connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = '_data/recipes.db')
ingredients <- RSQLite::dbGetQuery(conn = connect, statement = 'SELECT * FROM ingredients')
instructions <- RSQLite::dbGetQuery(conn = connect, statement = 'SELECT * FROM instructions')
RSQLite::dbDisconnect(conn = connect)
recipe_name <- '", name, "'

display_ingredients <- ingredients %>%
  subset(recipe == recipe_name) %>%
  group_by(recipe, type) %>%
  mutate(n = 1:n()) %>%
  spread(type, ingredients) %>%
  dplyr::select(other, meat, veggie, fruit) %>%
  dplyr::rename(Other = other, Meat = meat, Veggie = veggie, Fruit = fruit)

display_instructions <- instructions %>%
  subset(recipe == recipe_name) %>%
  arrange(idx) %>%
  dplyr::select(instructions)
```

", use_image,"

", use_video, "


#### Ingredients

```{r, echo = FALSE}
display_ingredients %>% nhuyhoa_df_print(head = 100, data = FALSE, attribute = \"class = \\\"presenttab\\\"\")
```

<br>

#### Instructions

```{r, echo = FALSE}
display_instructions %>% nhuyhoa_df_print(head = 100, data = FALSE, attribute = \"class = \\\"presenttabnoh\\\"\")
```
") %>% paste(collapse = "")

    # save file
    name_edit <- stringr::str_replace_all(name, " ", "-")
    file_name <- stringr::str_interp("_source/2017-05-15-Recipe-${name_edit}.Rmd")
    write(script, file = file_name)
    return(script)
  }


  # make RMD script for each recipe
  purrrlyr::by_row(recipe_info, make_script)
}
