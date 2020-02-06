lowest_values <- function(x, n = 5) {
  paste(round(head(sort(unique(x), na.last = NA, decreasing = FALSE), n = n), 3), collapse = ", ")
}

highest_values <- function(x, n = 5) {
  paste(round(head(sort(unique(x), na.last = NA, decreasing = TRUE), n = n), 3), collapse = ", ")
}
