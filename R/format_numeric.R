format_numeric <- function(numeric_summary.df = output.df) {
  columns_to_round <- c("mean", "sd", "p5", "p25", "p50", "p75", "p95")
  
  min.mean <- min(abs(numeric_summary.df$mean), na.rm = TRUE)
  min.sd <- min(abs(numeric_summary.df$sd), na.rm = TRUE)
  
  min.value <- min(min.mean, min.sd, na.rm = TRUE)
  
  digits <- case_when(
    min.value == 0 ~ 0, # this case is explicitly specified since log10(0) is undefined
    min.value >= 10 ~ 0,
    min.value >= 0.1 ~ 1,
    min.value < 0.1 ~ floor(abs(log10(min.value))) + 1
  )
  
  numeric_summary.df[, columns_to_round] <- round(numeric_summary.df[, columns_to_round], digits)
  
  return(numeric_summary.df)
}


