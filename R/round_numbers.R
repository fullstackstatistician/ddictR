round_numbers <- function(numeric.vector, class = "numeric") {
  
  min.value <- min(abs(numeric.vector), na.rm = TRUE)
  
  if (grepl("num|dbl|double", class)) {
    if (min.value <= 0.001) {
      return(signif(numeric.vector, 1))
    } else if (min.value < 1) {
      return(round(numeric.vector, 3))
    } else if (min.value < 10) {
      return(round(numeric.vector, 2))
    } else if (min.value < 100) {
      return(round(numeric.vector, 1))
    } else {
      return(round(numeric.vector, 0))
    }
  } else if (grepl("char|chr", class)) {
    if (min.value <= 0.001) {
      return(as.character(signif(numeric.vector, 1)))
    } else if (min.value < 1) {
      return(as.character(round(numeric.vector, 3)))
    } else if (min.value < 10) {
      return(as.character(round(numeric.vector, 2)))
    } else if (min.value < 100) {
      return(as.character(round(numeric.vector, 1)))
    } else {
      return(as.character(round(numeric.vector, 0)))
    }
  } else {
    stop("Please provide a valid value for `class` ('numeric' or 'character').\n")
  }
}
