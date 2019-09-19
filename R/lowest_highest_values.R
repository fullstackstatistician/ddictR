lowest_values <- function(x, n = 5) {
  paste(head(sort(unique(x), na.last = NA), n = n), collapse = ", ")
}

highest_values <- function(x, n = 5) {
  paste(tail(sort(unique(x), na.last = NA), n = n), collapse = ", ")
}
