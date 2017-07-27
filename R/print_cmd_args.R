
#' Prints out command line arguments to log file
#'
#' @param opt a list generated from optparse::parse_args
#'
#' @export
#'
#' @examples
#' jn.general::start_logging()
#'
#' option_list <- list(make_option("--test_opt", help = "this is a parameter"))
#' opt <- parse_args(OptionParser(description = "This is a test", option_list = option_list))
#'
#' print_cmd_args(opt)

print_cmd_args <- function(opt){

  cmdargs <- purrr::discard(opt, names(opt) %in% c('log', 'help'))
  args <- paste0(names(cmdargs), ": ", cmdargs) %>% paste(collapse = '\n')
  msg <- paste0("Command line args:\n", args, "\n\n")
  logging::loginfo(msg)

}
