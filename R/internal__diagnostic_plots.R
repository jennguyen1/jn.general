
# internal diagnostic plots for lm
lm_plot <- function(mod){

  # Residuals vs Fitted Plot
  resids_v_fitted <- ggplot(mod, aes(.fitted, .resid)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
    xlab("Fitted Values") + ylab("Residuals") +
    ggtitle("Residuals vs. Fitted")

  # Residuals vs Fitted Plot
  stdresids_v_fitted <- ggplot(mod, aes(.fitted, .stdresid)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
    xlab("Fitted Values") + ylab("Studentized Residuals") +
    ggtitle("Residuals vs. Fitted")

  # qqline
  probs <- c(0.25, 0.75)
  y <- quantile(resid(mod), probs, names = FALSE, type = 7, na.rm = TRUE)
  x <- qnorm(probs)
  slope <- diff(y) / diff(x)
  int <- y[1L] - slope * x[1L]
  # QQ plot
  qqnorm <- ggplot(data = NULL, aes(sample = resid(mod))) +
    stat_qq() +
    geom_abline(intercept = int, slope = slope) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("QQ-Normal")

  # Scale Location
  scale_location <- ggplot(mod, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point(alpha = 0.8, na.rm=TRUE) +
    geom_smooth(method="loess", se = FALSE, na.rm = TRUE) +
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
    srvfPlot = stdresids_v_fitted,
    qqPlot = qqnorm,
    slPlot = scale_location,
    cdPlot = cooks_dist,
    influencePlot = influence_plot
  )
  out
}

# internal diagnostic plots for glm
glm_plot <- function(mod){

  # Residuals vs Fitted Plot
  resids_v_fitted <- ggplot(mod, aes(.fitted, .resid)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
    xlab(expression(hat(eta))) + ylab("Deviance Residuals") +
    ggtitle("Residuals vs. Fitted")

  # Half Normal Plot
  d <- broom::augment(mod)
  # standardized residuals
  d <- dplyr::mutate(d,
    x = sort(abs(.std.resid)),
    ui = qnorm( (length(x) + 1:length(x))/(2 * length(x) + 1) )
  )
  halfnorm.resid <- ggplot(data = d, aes(x = ui, y = x)) +
    geom_point() +
    xlab("Half-Normal Quantiles") + ylab("Sorted Data") +
    ggtitle("Half-Normal Plot for Studentized Residuals")

  # leverages
  d <- dplyr::mutate(d,
    x = sort(abs(.hat)),
    ui = qnorm( (length(x) + 1:length(x))/(2 * length(x) + 1) )
  )
  halfnorm.lev <- ggplot(data = d, aes(x = ui, y = x)) +
    geom_point() +
    xlab("Half-Normal Quantiles") + ylab("Sorted Data") +
    ggtitle("Half-Normal Plot for Leverages")

  # cooks distance
  d <- dplyr::mutate(d,
    x = sort(abs(.cooksd)),
    ui = qnorm( (length(x) + 1:length(x))/(2 * length(x) + 1) )
  )
  halfnorm.cd <- ggplot(data = d, aes(x = ui, y = x)) +
    geom_point() +
    xlab("Half-Normal Quantiles") + ylab("Sorted Data") +
    ggtitle("Half-Normal Plot for Cooks Distances")

  # output
  out <- list(
    rvfPlot = resids_v_fitted,
    hnrPlot = halfnorm.resid,
    hnlPlot = halfnorm.lev,
    hncPlot = halfnorm.cd
  )
  out
}
