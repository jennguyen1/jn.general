
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
  if(recipes) run_recipes("_source/data/Recipes.R")

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
run_recipes <- function(db){

  # loads a list called recipes
  source(db)

  # display ingredients as a dataframe; so make each ingredient sublist same length
  recipes <- purrr::map(recipes, function(x){
    m <- max(purrr::map_int(x$ingredients, length))
    x$display_ingredients <- as.data.frame(purrr::map(x$ingredients, function(x) c(x, rep("", m - length(x)))))
    return(x)
  })

  # find the picture(s) for the recipe and add to recipes list
  for(n in names(recipes)){
    pat <- stringr::str_replace(n, " \\(.*", "") %>% stringr::str_replace_all(" ", "_")
    pics <- list.files("figure/food/", pattern = pat)
    recipes[[n]]$pics <- stringr::str_subset(pics, "JPG")
  }

  # save recipes database into a file
  save(recipes, file = "_source/data/recipes.Rdata")
  save(recipes, file = "~/Desktop/recipe_finder/recipes.Rdata")

  # generate recipe RMD files for website
  make_script <- function(i){
    dish <- names(recipes)[i]
    recipe <- recipes[[i]]

    # recipe pictures - format for markdown/html
    pic <- recipe$pics %>%
      paste0("![pic", 1:length(.), "](http://jnguyen92.github.io/nhuyhoa/figure/food/", ., ")") %>%
      paste(collapse = "\n\n")
    pic <- ifelse(length(recipe$pics) == 0, "", pic)

    # recipe youtube - format for markdown/html
    youtube <- ""
    if(recipe$youtube != "") youtube <- paste0("[![youtube](http://img.youtube.com/vi/", recipe$youtube, "/0.jpg)](http://www.youtube.com/watch?v=", recipe$youtube, ")")

    # RMD template
    script <- c("---
layout: post
title: \"", dish, "\"
date: \"May 15, 2017\"
categories: ['recipes', '", recipe$meal, "']
---

```{r, echo = FALSE, warning = FALSE}
library(jn.general)
lib(data)
load('data/recipes.Rdata')
current <- recipes[['", dish, "']]
```

", pic,"

", youtube, "


### Ingredients

```{r, echo = FALSE}
current$display_ingredients %>% nhuyhoa_df_print(head = 100, data = FALSE, attribute = \"class = \\\"presenttab\\\"\")
```

<br>

### Instructions

```{r, echo = FALSE}
current$instructions %>% nhuyhoa_df_print(head = 100, data = FALSE, attribute = \"class = \\\"presenttabnoh\\\"\")
```
") %>% paste(collapse = "")

    # save file
    file_name <- paste0("_source/2017-05-15-Recipe-", stringr::str_replace_all(dish, " ", "-"), ".Rmd")
    write(script, file = file_name)
    return(script)
  }

  # make RMD script for each recipe
  purrr::map(1:length(recipes), make_script)

}
