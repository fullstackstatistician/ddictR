summarize_levels <- function(data = merged.df, variable) {
  if ("epoch" %nin% names(data)) {
    stop(paste0("The data set must contain the variable: epoch.\n\n"))
  }
  
  subset.df <- data[, c("epoch", variable)]
  subset.df <- rVMAP::clear_labels(subset.df)
  
  data.list <- list(
    overall = subset.df,
    epoch1  = subset(subset.df, epoch == 1),
    epoch2  = subset(subset.df, epoch == 2),
    epoch3  = subset(subset.df, epoch == 3),
    epoch4  = subset(subset.df, epoch == 4)
  )
  
  output.df <- purrr::map_dfc(
    data.list,
    function(x) {get_levels(x[[variable]])$Frequency}
  )
  
  names(output.df) <- c("Overall", "Epoch 1", "Epoch 2", "Epoch 3", "Epoch 4")
  
  output.df <- cbind(
    Value = c(levels(subset.df[[variable]]), "Missing"),
    output.df,
    stringsAsFactors = FALSE
  )
  
  return(output.df)
}
