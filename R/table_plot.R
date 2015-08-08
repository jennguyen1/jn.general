
#' Plots Data Frames
#'
#' Generates a table grob object for plotting with ggplot2.
#'
#' @param df a data frame to be turned into a plot object
#' @param title title of the plot object
#' @param rownames rownames of the data frame; defaults to original rownames of data frame
#'
#' @return a grob with table that can be plotted
#'
#' @export
#'
#' @examples
#' table_plot(head(mtcars), title = "Cars")

table_plot <- function(df, title = "", rownames = NULL){

  # check data frame & title input is correct
  if( missing(df) ) stop("Missing input data frame")
  if( !is.data.frame(df) ) stop("Invalid df input")
  if( !is.character(title) ) stop("Title should be a string")

  # table grob themes
  axis_theme <- list(bg_params = list(fill = "#BFBFBF"), fg_params = list(fontface = "bold"))
  theme <- gridExtra::ttheme_default(rowhead = axis_theme, colhead = axis_theme)

  # plot table; account for rownames
  if( is.null(rownames) ){
    tab <- gridExtra::tableGrob(df, theme = theme)
  } else {
    if( length(rownames) != nrow(df) ) stop("Specified rownames do not match length of data frame")
    tab <- gridExtra::tableGrob(df, rows = rownames, theme = theme)
  }

  # generate title and padding for table
  tab_title <- grid::textGrob(title, gp = grid::gpar(fontsize = 20))
  padding <- grid::unit(2, "line")

  # add title to the table plot with a table
  tab <- gtable::gtable_add_rows(tab, heights = grid::grobHeight(tab_title) + padding, pos = 0)
  tab <- gtable::gtable_add_grob(tab, tab_title, t = 1, l = 1, r = ncol(tab))

  # return table
  return(tab)
}
