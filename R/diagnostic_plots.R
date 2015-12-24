# TODO: identify points (put on plot)

#' Diagnostic Plots for lm and glm
#'
#' Generates the diagnostic plots in ggplot2, similar to the ones that are generated via the plot command. Currently available for lm and glm models
#'
#' @param mod a lm model
#'
#' @return a list of diagnostic plots
#' @export

diagnostic_plots <- function(mod){

  # glm diagnostic plot
  if( any(class(mod) %in% "glm") ) {
    return( glm_plot(mod) )

  # lm diagnostic plot
  } else if( class(mod) %in% "lm" ) {
    return( lm_plot(mod) )
  }
}
