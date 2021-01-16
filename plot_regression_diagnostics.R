plot_regression_diagnostics <- function(lm.object, mark.outliers = FALSE, alpha = 0.05) {
  # Function that produces a variety of diagnostic plots for linear regression
  #
  #   lm.object: object produced by lm()
  #   mark.outliers: if TRUE will produce thresholds (quantiles) and mark observations outside
  #
  #   output: list of plot objects, access with diagPlot$<name>, or plot all with
  #           do.call(grid.arrange, c(diagPlts, top = "Diagnostic Plots", ncol = <number>))
  
  ff <- fortify(lm.object)
  ff <- ff %>% mutate(.studresid = .resid/.sigma*sqrt(1-.hat)) # Studentized residuals
  
  # Residuals vs Fitted values
  p1 <- ggplot(ff, aes(.fitted, .resid)) + geom_point()
  p1 <- p1 + stat_smooth(method = "lm") + geom_hline(yintercept = 0, col = "red", linetype = "dashed")
  p1 <- p1 + xlab("Fitted values") + ylab("Residuals")
  p1 <- p1 + ggtitle("Residual vs Fitted") + theme_bw()
  
  # Q-Q plot (check normality of residuals)
  p2 <- ggplot(ff, aes(sample = .stdresid)) + stat_qq(na.rm = TRUE)
  p2 <- p2 + geom_abline()
  p2 <- p2 + xlab("Theoretical Quantiles") + ylab("Standardized Residuals")
  p2 <- p2 + ggtitle("Normal Q-Q") + theme_bw()
  
  # Standardized residuals vs Fitted value
  p3 <- ggplot(ff, aes(.fitted, sqrt(abs(.stdresid)))) + geom_point(na.rm = TRUE)
  p3 <- p3 + stat_smooth(method = "lm", na.rm = TRUE)
  p3 <- p3 + xlab("Fitted Value") + ylab(expression(sqrt("|Standardized residuals|")))
  p3 <- p3 + ggtitle("Scale-Location") + theme_bw()
  
  # Standardized residuals vs Leverage
  p4 <- ggplot(ff, aes(.hat, .stdresid)) + geom_point(aes(size=.cooksd), na.rm = TRUE)
  p4 <- p4 + stat_smooth(method = "lm", na.rm = TRUE)
  p4 <- p4 + xlab("Leverage") + ylab("Standardized Residuals")
  p4 <- p4 + ggtitle("Residual vs Leverage Plot")
  p4 <- p4 + scale_size_continuous("Cook's Distance", range = c(1,5))
  p4 <- p4 + theme_bw() + theme(legend.position="bottom")
  
  # Cook's distance vs Observation (assess actual influence)
  p5 <- ggplot(ff, aes(seq_along(.cooksd), .cooksd)) + geom_bar(stat="identity", position="identity")
  p5 <- p5 + xlab("Observation") + ylab("Cook's distance")
  p5 <- p5 + theme_bw()
  
  # Cooks' distance vs Leverage
  p6 <- ggplot(ff, aes(.hat, .cooksd)) + geom_point(na.rm = TRUE) + stat_smooth(method = "loess", na.rm = TRUE)
  p6 <- p6 + xlab("Leverage hii") + ylab("Cook's Distance")
  p6 <- p6 + ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6 <- p6 + geom_abline(slope = seq(0,3,0.5), color = "gray", linetype = "dashed")
  p6 <- p6 + theme_bw()
  
  # Studentized residuals (determine outliers)
  p7 <- ggplot(ff, aes(.fitted, .studresid)) + geom_point()
  p7 <- p7 + stat_smooth(method = "lm") + geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
  p7 <- p7 + xlab("Fitted values") + ylab("Studentized Residuals")
  p7 <- p7 + ggtitle("Studentized Residual vs Fitted") + theme_bw()
  
  # Studentized residuals vs leverage (and Cook's distance)
  p8 <- ggplot(ff, aes(.hat, .studresid)) + geom_point(aes(size = .cooksd), na.rm = TRUE)
  p8 <- p8 + xlab("Leverage") + ylab("Studentized Residuals")
  p8 <- p8 + labs(size = "Cook's distance") + theme_bw()
  
  if (mark.outliers == TRUE){
    # Cook's distance threshold: 2p/n
    t_cooks <- 2 * length(lm.object$coefficients) / nrow(ff)
    p5 <- p5 + geom_hline(yintercept = t_cooks, colour = "red", linetype = "dashed")
    p5 <- p5 + geom_text(aes(label = ifelse(.cooksd >= t_cooks, as.character(seq_along(.cooksd)), '')), hjust=0, vjust=0)
    
    p6 <- p6 + geom_hline(yintercept = t_cooks, colour = "red", linetype = "dashed")
    p6 <- p6 + geom_text(aes(label = ifelse(.cooksd >= t_cooks, as.character(seq_along(.cooksd)), '')), hjust=0, vjust=0)
    
    # Bonferroni correction, quantile from Student's t-distribution with n-1-p degrees of freedom
    alpha_bonferroni = alpha / nrow(ff)
    t_bonferroni = qt(alpha_bonferroni / 2, nrow(ff) - 1 - length(lm.object$coefficients))
    
    p7 <- p7 + geom_hline(yintercept = t_bonferroni, colour = "red", linetype = "dashed") + 
      geom_hline(yintercept = - t_bonferroni, colour = "red", linetype = "dashed")
  }
  
  list(rvfPlot = p1,
       qqPlot = p2, 
       sclLocPlot = p3, 
       rvlevPlot = p4, 
       cdPlot=p5, 
       cvlPlot=p6, 
       studPlot=p7, 
       svlPlot=p8)
}