print_missing <- function(data = merged.df, variable, id = "map.id") {
  if (id %nin% names(data)) {
    stop("The specified ID variable must be present in the data.\n\n")
  }
  
  missing.vec <- data[is.na(data[[variable]]), id]
  
  if (length(missing.vec) > 0) {
    output.string <- paste0(missing.vec, collapse = ", ")
  } else {
    output.string <- "None"
  }
  
  return(output.string)
}
