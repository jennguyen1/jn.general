
#' Shortcut Functions
#'
#' Things that I commonly use in convenient functions
#'
#' @details
#' blank_slate() clears the environment and console \cr
#'
#' install_jn() installs the jn.general package \cr
#'
#' lib() imports libraries depending on args: \cr
#'  data: (data wrangling) magrittr, stringr, tidyverse, data.table, etc \cr
#'  viz: (data visualization) ggplot2 and extensions \cr
#'  model: (modeling) broom, lme4, glmnet, caret \cr
#'  develop: (development) optparse, logging \cr
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
lib <- function(...) scriptR::lib(...)
