
#' Shortcut Functions
#'
#' Things that I commonly use in convenient functions
#'
#' @details
#' blank_slate() clears the environment and console \cr
#'
#' install_jn() installs the jn.general package \cr
#'
#' lib() imports libraries, see scriptR::lib() \cr
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
