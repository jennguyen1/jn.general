
#' Shortcut Functions
#'
#' Things that I commonly use in convenient functions
#'
#' @details
#' blank_slate() clears the environment and console \cr
#'
#' install_jn() installs the jn.general package \cr
#'
#' percent-p-percent is shortcut for paste0 \cr
#'
#' lib() imports libraries depending on args: \cr
#'  data: (data wrangling) magrittr, stirngr, plyr, dplyr, reshape2, data.table \cr
#'  viz: (data visualization) knitr, ggplot2, grid, gridExtra, GGally, gtable \cr
#'  model: (modeling) caret \cr
#'  develop: (development) devtools, microbenchmark \cr
#'
#' nhuyhoa() generates nhuyhoa jekyll blog
#'
#'
#' @name shortcuts
NULL

#' @rdname shortcuts
#' @export
blank_slate <- function(){
  rm(list = ls(pos=".GlobalEnv"), pos=".GlobalEnv")
  cat("\014")
}

#' @rdname shortcuts
#' @export
install_jn <- function() devtools::install_github("jnnguyen2/jn.general")

#' @rdname shortcuts
#' @export
lib <- function(...){
  
  args <- as.character(substitute(list(...))[-1])
  
  lib_opts <- data.table::data.table(data = TRUE, viz = FALSE, model = FALSE, develop = FALSE) %>% .[, args := TRUE, with = FALSE]
  
  suppressPackageStartupMessages( import_lib(lib_opts))
}

import_lib <- function(lib_opts){

  if(lib_opts$data){
    # data wrangling
    library(reshape2)
    library(magrittr)
    library(stringr)
    library(plyr)
    library(dplyr)
    library(data.table)
  }

  if(lib_opts$viz){
    # data visualization
    library(knitr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(GGally)
    library(gtable)
  }

  if(lib_opts$model){
    # model training
    library(caret)
  }

  if(lib_opts$develop){
    # development
    library(devtools)
    library(microbenchmark)
  }

}

#' @rdname shortcuts
#' @export
`%p%` <- function(x,y) paste0(x,y)

