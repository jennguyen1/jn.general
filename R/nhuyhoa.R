
#' Functions for nhuyhoa Jekyll Blog
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
  "Implements changes to jekyll blog"

  assertthat::assert_that(basename(getwd()) == "nhuyhoa", msg = "Invalid working directory")
  if(recipes) run_recipes()
  servr::jekyll(dir = ".", input = c(".", "_source", "_posts"),
                output = c(".", "_posts", "_posts"), script = c("build.R"),
                serve = TRUE, command = "jekyll build")
}


#' @rdname nhuyhoa
#' @export
nhuyhoa_df_print <- function(df, head = 5, data = TRUE, attribute = "class = \"presenttab\"", ...){
  "Formatting for blog tables"

  if(data){ # printing data
    df %>% head(head) %>% knitr::kable(format = "html", align = "c", ...)
  } else{ # presenting tables
    df %>% head(head) %>% knitr::kable(format = "html", table.attr = attribute, ...)
  }
}


#' @rdname nhuyhoa
#' @export
run_recipes <- function(){
  "Regenerate recipes files"

  connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "_data/recipes.db")
  recipes <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM recipes")
  RSQLite::dbDisconnect(conn = connect)

  # extract recipe pics
  recipe_info <- recipes
  recipe_info$pictures <- apply(recipes, 1, function(r){
    pat <- r['recipe'] %>%
      stringr::str_replace(" \\(.*", "") %>%
      stringr::str_replace_all(" ", "_")
    pics <- list.files("figure/food/", pattern = pat)
    stringr::str_subset(pics, glue::glue("^{pat}\\d*.JPG"))
  })


  # generate recipe RMD files for website
  make_script <- function(df){

    name <- df$recipe

    # recipe pictures - format for markdown/html
    pictures <- df$pictures
    use_image <- pictures %>%
      paste0("![pic", 1:length(.), ']( {{"/figure/food/', ., '" | absolute_url }})') %>%
      paste(collapse = "\n\n")
    use_image <- ifelse(length(pictures) == 0, "", use_image)

    # recipe youtube - format for markdown/html
    youtube <- df$youtube
    use_video <- ""
    if( !is.na(youtube) ) use_video <- paste0("[![youtube](http://img.youtube.com/vi/", youtube, "/0.jpg)](http://www.youtube.com/watch?v=", youtube, ")")

    script <- c("---
layout: post
title: \"", name, "\"
date: \"May 15, 2017\"
categories: Recipes
tags: ", stringr::str_to_title(df$meal_type), "
---

```{r, echo = FALSE, warning = FALSE}
library(jn.general)
lib(data)

recipe_name <- '", name, "'
connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = '../_data/recipes.db')
ingredients_query <- glue::glue('SELECT * FROM ingredients WHERE recipe = \"{recipe_name}\"')
instructions_query <- glue::glue('SELECT * FROM instructions WHERE recipe = \"{recipe_name}\"')
ingredients <- RSQLite::dbGetQuery(conn = connect, statement = ingredients_query)
instructions <- RSQLite::dbGetQuery(conn = connect, statement = instructions_query)
RSQLite::dbDisconnect(conn = connect)

display_ingredients <- ingredients %>%
  dplyr::mutate(idx = ifelse(ingredients == 'any', 1, 0)) %>%
  dplyr::arrange(idx, ingredients) %>%
  dplyr::group_by(recipe, type) %>%
  dplyr::mutate(n = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-idx) %>%
  tidyr::spread(type, ingredients)
add_cols <- purrr::discard(c('other', 'meat', 'veggie', 'fruit'), ~ .x %in% colnames(display_ingredients))
for(c in add_cols) display_ingredients[,c] <- NA
display_ingredients <- display_ingredients %>%
  dplyr::select(other, meat, veggie, fruit) %>%
  dplyr::mutate_all(~ ifelse(is.na(.x), '', .x)) %>%
  dplyr::rename(Other = other, Meat = meat, Veggie = veggie, Fruit = fruit)

display_instructions <- instructions %>%
  dplyr::arrange(idx) %>%
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

    name_edit <- stringr::str_replace_all(name, " ", "-")
    file_name <- glue::glue("_source/2017-05-15-{name_edit}.Rmd")
    write(script, file = file_name)
    return(script)
  }


  apply(recipe_info, 1, make_script)
  invisible()
}
