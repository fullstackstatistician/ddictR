tabulate_extreme_values <- function(data = merged.df, epoch = 1:4, variable) {
  lowest.list <- sapply(epoch, function(x) lowest_values(data[data[["epoch"]] %in% x, variable]))
  highest.list <- sapply(epoch, function(x) highest_values(data[data[["epoch"]] %in% x, variable]))
  
  output.df <- data.frame(
    epoch = paste0("Epoch ", epoch),
    lowest = lowest.list,
    highest = highest.list
  ) %>%
    setNames(c(" ", "Lowest Unique Values", "Highest Unique Values"))
  
  return(output.df)
}
