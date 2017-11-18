
#' Diagnostic Plots for lm and glm
#'
#' Generates the diagnostic plots in ggplot2, similar to the ones that are generated via the plot command. Currently available for lm and glm models
#'
#' @param mod a lm or glm model obj
#'
#' @return a list of diagnostic plots
#' @export

diagnostic_plots <- function(mod){
  asserthat::assert_that( any(class(mod) %in% c("lm", "glm")), msg = "Invalid mod argument")
  if( any(class(mod) %in% "glm") ) {
    return( glm_plot(mod) )
  } else if( class(mod) %in% "lm" ) {
    return( lm_plot(mod) )
  }
}
