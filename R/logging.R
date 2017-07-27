
#' Logging Functions
#'
#' Additional functions to be used with the logging package
#'
#' @details
#' start_logging() to start a new logging session \cr
#'
#' get_logfile() to obtain the name of the logfile \cr
#'
#' logdf() to log a data.frame \cr
#'
#' stopQuietly() quits without an error message \cr
#'
#' logerror() logs an error and quits the program \cr
#'
#'
#' @name logging
NULL

#' @rdname logging
#' @export
start_logging <- function(name = paste('R', str_replace(Sys.time(), " ", "_") , sep = "_")){

  logReset()

  # write to console
  getLogger()$addHandler(writeToConsole, level = 'DEBUG')

  # write to file
  logfile <- file.path(getwd(), paste0(name, ".log"))
  getLogger()$addHandler(writeToFile, file = logfile, level = 'DEBUG')

}

#' @rdname logging
#' @export
get_logfile <- function() getLogger()$handlers$writeToFile$file

#' @rdname logging
#' @export
logdf <- function(df){

  if(!is.data.frame(df)) logerror("Not a data.frame")

  # print df to console
  print(df)

  # write df to log file
  logfile <- get_logfile()
  suppressWarnings(write.table(df, file = logfile, append = TRUE, quote = FALSE, row.names = FALSE, sep = "\t"))

}

#' @rdname logging
#' @export
stopQuietly <- function() {

  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "))
  stop(simpleError(blankMsg))

}

#' @rdname logging
#' @export
logerror <- function(msg){

  logging::logerror(msg)
  stopQuietly()

}
