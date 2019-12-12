get_levels <- function(x) {
  levels.df <- as.data.frame(table(x, useNA = "always"), stringsAsFactors = FALSE) 
  levels.df$x[is.na(levels.df$x)] <- "Missing"
  
  n <- sum(levels.df$Freq)
  
  levels.df$Prop <- paste0(round(100 * levels.df$Freq / n, 0), "%")
  
  levels.df <- data.frame(
    Value = levels.df$x,
    Frequency = paste0(levels.df$Freq, " (", levels.df$Prop, ")"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  return(levels.df)
}
