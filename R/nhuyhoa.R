
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
nhuyhoa <- function(recipes = TRUE){

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

  # find the picture(s) for the recipe
  for(n in names(recipes)){
    pics <- list.files("figure/food/", pattern = stringr::str_replace(n, " \\(.*", ""))
    recipes[[n]]$pics <- stringr::str_subset(pics, "JPG") # can i handle movies?
  }

  # save recipes database into a file
  save(recipes, file = "_source/data/recipes.Rdata")
  save(recipes, file = "~/Desktop/recipe_finder/recipes.Rdata")

  # generate recipe RMD files for website
  make_script <- function(i){
    dish <- names(recipes)[i]

    # recipe pictures
    pic <- recipes[[i]]$pics %>%
      paste0("![pic", 1:length(.), "](http://jnguyen92.github.io/nhuyhoa/figure/food/", ., ")") %>%
      paste(collapse = "\n\n")
    pic <- ifelse(length(recipes[[i]]$pics) == 0, "", pic)

    # RMD template
    script <- c("---
layout: post
title: \"", dish, "\"
date: \"May 15, 2017\"
categories: ['recipes', '", recipes[[i]]$meal, "']
---

* TOC
{:toc}

```{r, echo = FALSE, warning = FALSE}
library(jn.general)
lib(data)
load('data/recipes.Rdata')
current <- recipes[['", dish, "']]
```

", pic,"

**Ingredients**

```{r, echo = FALSE}
current$display_ingredients %>% nhuyhoa_df_print(head = 100, data = FALSE, attribute = \"class = \\\"presenttab\\\"\")
```

<br>

**Instructions**

```{r, echo = FALSE}
current$instructions %>% nhuyhoa_df_print(head = 100, data = FALSE, attribute = \"class = \\\"presenttabnoh\\\"\")
```
") %>% paste(collapse = "")

    # save file
    file_name <- paste0("_source/2017-05-15-", stringr::str_replace_all(dish, " ", "-"), ".Rmd")
    write(script, file = file_name)
    return(script)
  }

  # make RMD script for each recipe
  purrr::map(1:length(recipes), make_script)

}
