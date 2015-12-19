# TODO: identify points (put on plot)

#' Diagnostic Plots for lm
#' 
#' Generates the diagnostic plots in ggplot2, similar to the ones that are generated via the plot command. Currently available for lm models only
#' 
#' @param mod a lm model
#' 
#' @return a list of diagnostic plots
#' 
#' @export 


diagnostic_plots <- function(mod){
  # Residuals vs Fitted Plot
  resids_v_fitted <- ggplot(mod, aes(.fitted, .resid)) +
    geom_point(alpha = 0.8) +
    stat_smooth(method = "loess", se = FALSE) + 
    geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
    xlab("Fitted Values") + ylab("Residuals") +
    ggtitle("Residuals vs. Fitted")
  
  
  
  # qqline
  probs <- c(0.25, 0.75)
  y <- quantile(resid(mod), probs, names = FALSE, type = 7, na.rm = TRUE)
  x <- qnorm(probs)
  slope <- diff(y) / diff(x)
  int <- y[1L] - slope * x[1L]
  # QQ plot
  qqnorm <- qplot(sample = resid(mod), stat = "qq", main = "QQ-Normal") + 
    geom_abline(intercept = int, slope = slope) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") 

  
  
  # Scale Location
  scale_location <- ggplot(mod, aes(.fitted, sqrt(abs(.stdresid)))) + 
    geom_point(alpha = 0.8, na.rm=TRUE) +
    stat_smooth(method="loess", se = FALSE, na.rm = TRUE) + 
    xlab("Fitted Value") + ylab(expression(sqrt("|Standardized residuals|"))) +
    ggtitle("Scale-Location")
    
  
  
  # Cooks Distance
  cooks_dist <- ggplot(mod, aes(seq_along(.cooksd), .cooksd)) + 
    geom_bar(stat="identity", position="identity") + 
    xlab("Obs. Number") + ylab("Cook's distance") +
    ggtitle("Cook's distance")
    
  
  # Influence plot
  influence_plot <- ggplot(mod, aes(x = .hat, y = .stdresid, size = .cooksd, color = .cooksd)) +
    geom_point(alpha = 0.8, na.rm = TRUE) +
    xlab("Hat-Values") + ylab("Standardized Residuals") +
    ggtitle("Residual vs Leverage Plot") +
    scale_size_continuous("Cook's Distance", range=c(2,15)) +
    scale_color_continuous("Cooks Distance") +
    geom_hline(yintercept = -2, col = "black", linetype = "dashed") +
    geom_hline(yintercept = 2, col = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
    geom_vline(xintercept = 3*mean(influence(mod)$hat), col = "black", linetype = "dashed") +
    theme(legend.position = "bottom")
    
  
  
  # output
  out <- list(
    rvfPlot = resids_v_fitted,
    qqPlot = qqnorm,
    slPlot = scale_location,
    cdPlot = cooks_dist,
    influencePlot = influence_plot
  )
  return(out)
}
