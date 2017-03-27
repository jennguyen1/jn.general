
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
#'  data: (data wrangling) magrittr, stringr, plyr, dplyr, tidyr, reshape2, data.table \cr
#'  viz: (data visualization) knitr, ggplot2, grid, gridExtra, GGally, gtable \cr
#'  model: (modeling) broom, lme4, glmnet, caret \cr
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
install_jn <- function() devtools::install_github("jnguyen92/jn.general")

#' @rdname shortcuts
#' @export
lib <- function(...){

  args <- as.character(substitute(list(...))[-1])

  lib_opts <- data.table::data.table(data = TRUE, viz = FALSE, model = FALSE, develop = FALSE) %>% .[, (args) := TRUE]

  suppressWarnings( suppressPackageStartupMessages( import_lib(lib_opts) ) )
}

import_lib <- function(lib_opts){

  if(lib_opts$data){
    # data wrangling
    library(magrittr)
    library(stringr)
    library(forcats)
    #library(purr)
    library(reshape2)
    library(tidyr)
    library(plyr)
    library(dplyr)
    library(data.table)
    library(dtplyr)
  }

  if(lib_opts$viz){
    # data visualization
    library(knitr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(GGally)
    library(gtable)

    # set ggplot2 theme
    theme_set(theme_bw())
  }

  if(lib_opts$model){
    # model training
    library(caret)
    library(broom)
    library(glmnet)
    library(lme4)
    library(nlme)
    library(lmerTest)
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

