create_individual_entry <- function(data = merged.df, metadata = metadata.df, variable) {
  write_out(paste0('## ', variable, '\n'))
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "label"])) {
    write_out(paste0('#### ', metadata[metadata[["variable"]] == variable, "label"], "\n"))
  }
  
  write_out("\n")
  
  chunk_wrapper(
    plot_distribution(variable = variable), 
    options = c("echo = FALSE", "warning = FALSE", "message = FALSE", "fig.height = 2.5", "fig.width = 4.2", "fig.align = 'center'")
  )
  
  write_out("\n")
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "derivation"])) {
    write_out(paste0('* **Derivation:** ', metadata[metadata[["variable"]] == variable, "derivation"]))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "comments"])) {
    write_out(paste0('* **Comments:** ', metadata[metadata[["variable"]] == variable, "comments"]))
  }
  
  #chunk_wrapper(paste0('plot_distribution(x = \"', variable, '\")'))

  chunk_wrapper(paste0('summarize_data(variable = \"', variable, '\")'))
  
  if (length(unique(data[[variable]])) < 8 | is.factor(data[[variable]])) {
    chunk_wrapper(paste0('summarize_levels(variable = \"', variable, '\")'))
  }
  
  if (is.numeric(data[[variable]]) | inherits(data[[variable]], "Date")) {
    write_out(paste0('* **Lowest Unique Values:** ', lowest_values(data[[variable]])))
    write_out(paste0('* **Highest Unique Values:** ', highest_values(data[[variable]])))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "figure_label"])) {
    write_out(paste0('* **Publication Figure Label:** ', metadata[metadata[["variable"]] == variable, "figure_label"]))
  }

  if (!is.na(metadata[metadata[["variable"]] == variable, "covariates"])) {
    write_out(paste0('* **Commonly Included Covariates:** ', metadata[metadata[["variable"]] == variable, "covariates"]))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "related_variables"])) {
    write_out(paste0('* **Related Variables:** ', metadata[metadata[["variable"]] == variable, "related_variables"]))
  }
  
  write_out("\n")
  write_out("\\newpage")
}
