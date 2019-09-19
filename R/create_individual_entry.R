create_individual_entry <- function(data, variable, metadata = metadata.df) {
  write_out(paste0('## ', variable, '\n'))
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "label"])) {
    write_out(paste0('### ', metadata[metadata[["variable"]] == variable, "label"], "\n"))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "derivation"])) {
    write_out(paste0('* Derivation: ', metadata[metadata[["variable"]] == variable, "derivation"]))
    write_out("\n")
  }
  
  chunk_wrapper(paste0('summarize_data(variable = \"', variable, '\")'))
  
  if (length(unique(data[[variable]])) < 8 | is.factor(data[[variable]])) {
    chunk_wrapper(paste0('summarize_levels(variable = \"', variable, '\")'))
  }
  
  if (is.numeric(data[[variable]]) | inherits(data[[variable]], "Date")) {
    write_out(paste0('* Lowest Unique Values: ', lowest_values(data[[variable]])))
    write_out(paste0('* Highest Unique Values: ', highest_values(data[[variable]])))
  }
  
  write_out("\n\n")
}
