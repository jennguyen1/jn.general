#' One-Way Anova Plotting
#'
#' Generates the anova summary table and the box plot for specified data in an easy to plot form
#'
#' @param data a data frame containing the x & y variables
#' @param x independent categorical variable
#' @param y dependent continuous variable
#'
#' @return a list with a ggplot2 plot object named plot and a anova summary table in a tableGrob
#' object named anova
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' library(gridExtra)
#' x <- anova_plot(iris, x = Species, y = Petal.Length)
#' marrangeGrob(x, ncol = 2, nrow = 2)
#'
anova_plot <- function(data, x, y){

  # plot and labels from the variable names
  p_subs <- substitute( ggplot(data = data, aes(x = x, y = y)) + geom_boxplot())
  labels <- sapply( as.character(substitute(list(x, y))[-1]), stringr::str_to_title )
  names(labels) <- c("x", "y")

  # generate plot with the labels
  p <- eval(p_subs) + xlab(labels[1]) + ylab(labels[2])

  # run anova
  a_subs <- substitute( aov(y ~ x) )
  a <- eval(a_subs, data)

  # convert anova table into a plot object
  a <- a %>%
    summary %>%
    .[[1]] %>%
    as.data.frame %>%
    round(2) %>%
    jn.general::table_plot(title = paste(labels["y"], "by",  labels["x"]), rownames = c(labels["x"], "Residuals"))

  # return results
  return( list(plot = p, anova = a) )
}

