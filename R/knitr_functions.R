write_out <- function(x, ..., out_file = file_connection, sep = "\n") {
  cat(paste0(x, ...), file = out_file, append = TRUE, sep = sep)
}

chunk_wrapper <- function(x, ..., out_file = file_connection, options = c("echo = FALSE", "warning = FALSE", "message = FALSE"), label = NULL, quiet = FALSE) {
  if (isTRUE(quiet)) {
    options <- c("include = FALSE", options)
  }
  
  write_out(
    paste0(
      "```{r", 
      ifelse( 
        is.null(label), 
        ", ", 
        paste0(" ", label, ", ")
      ),
      paste0(
        options, 
        collapse = ", "
      ), 
      "}"
    ),
    out_file = out_file
  )
  
  write_out(x, ..., out_file = out_file)
  write_out("```", out_file = out_file)
  write_out("")
}
