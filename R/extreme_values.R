lowest_values <- function(x, n = 5) {
  paste(round(head(sort(unique(x), na.last = NA), n = n), 4), collapse = ", ")
}

highest_values <- function(x, n = 5) {
  paste(round(tail(sort(unique(x), na.last = NA), n = n), 4), collapse = ", ")
}
