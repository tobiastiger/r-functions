make_correlation_plot <- function(dataframe) {
  
  dataframe <- dataframe[, sapply(dataframe, class) != "factor"]
  
  cormat <- round(cor(dataframe), 2)
  
  # Get upper triangle of the correlation matrix
  cormat[lower.tri(cormat)] <- NA
  
  melted_cormat <- melt(cormat, na.rm = TRUE)
  
  p1 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))
  p1 <- p1 + geom_tile(color = "white") + xlab("") + ylab("")
  p1 <- p1 + theme_minimal() + coord_fixed()
  p1 <- p1 + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
  p1 <- p1 + scale_fill_gradient2(low = "blue",
                                  high = "red",
                                  mid = "white", 
                                  midpoint = 0, 
                                  limit = c(-1,1),
                                  space = "Lab", 
                                  name="Pearson\nCorrelation")
  
  p1
}