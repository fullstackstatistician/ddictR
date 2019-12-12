summarize_data <- function(data = merged.df, variable) {
  if ("map.id" %nin% names(data) | "epoch" %nin% names(data)) {
    stop(paste0("The data set must contain the variables map.id, epoch, and ", variable, ".\n\n"))
  }
  
  subset.df <- data[, c("map.id", "epoch", variable)]
  subset.df <- rVMAP::clear_labels(subset.df)
  
  data.list <- list(
    overall = subset.df,
    epoch1  = droplevels(subset(subset.df, epoch == 1)),
    epoch2  = droplevels(subset(subset.df, epoch == 2)),
    epoch3  = droplevels(subset(subset.df, epoch == 3)),
    epoch4  = droplevels(subset(subset.df, epoch == 4))
  )
  
  if (is.numeric(subset.df[[variable]])) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.numeric(x[[variable]])}
    )
    
    output.df <- format_numeric(numeric_summary.df = output.df)
    
    colnames(output.df) <- c("n", "nmiss", "distinct", "mean", "sd", "5%", "25%", "50%", "75%", "95%")
  } else if (is.factor(subset.df[[variable]])) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.factor(x[[variable]])}
    )
  } else if (inherits(subset.df[[variable]], "Date")) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.Date(x[[variable]])}
    )
  } else if (is.character(subset.df[[variable]])) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.character(x[[variable]])}
    )
  } else {
    stop("Class for variable ", variable, " is not numeric, factor, Date, or character. Please check data type.\n\n")
  }
  
  output.df <- cbind(
    subset = c("Overall", "Epoch 1", "Epoch 2", "Epoch 3", "Epoch 4"),
    output.df,
    stringsAsFactors = FALSE
  )
  
  return(output.df)
}
