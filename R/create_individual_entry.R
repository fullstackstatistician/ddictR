create_individual_entry <- function(data = merged.df, metadata = metadata.df, variable) {
  write_out(paste0('## Variable: ', variable, '\n'))
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "label"])) {
    write_out(paste0('#### Label: ', metadata[metadata[["variable"]] == variable, "label"], "\n"))
  }
  
  write_out("\n")
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "description"])) {
    write_out(paste0('* **Description:** ', metadata[metadata[["variable"]] == variable, "description"]))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "comments"])) {
    write_out(paste0('* **Comments:** ', metadata[metadata[["variable"]] == variable, "comments"]))
  }
  
  write_out("\n")
  
  write_out(paste0('#### Distribution \n'))
  
  chunk_wrapper(
    plot_distribution(variable = variable), 
    options = c("echo = FALSE", "warning = FALSE", "message = FALSE", "fig.height = 2.5", "fig.width = 4.2", "fig.align = 'center'")
  )

  chunk_wrapper(paste0('summarize_data(variable = \"', variable, '\")'))
  
  if (is.factor(data[[variable]]) | length(unique(data[[variable]])) < 10) {
    chunk_wrapper(paste0('summarize_levels(variable = \"', variable, '\")'))
  }
  
  if (is.numeric(data[[variable]]) | inherits(data[[variable]], "Date")) {
    write_out(paste0('##### Epoch 1 \n'))
    write_out(paste0('* **Lowest Unique Values:** ', lowest_values(data[data[["epoch"]] == 1, variable])))
    write_out(paste0('* **Highest Unique Values:** ', highest_values(data[data[["epoch"]] == 1, variable])))
    
    write_out(paste0('##### Epoch 2 \n'))
    write_out(paste0('* **Lowest Unique Values:** ', lowest_values(data[data[["epoch"]] == 2, variable])))
    write_out(paste0('* **Highest Unique Values:** ', highest_values(data[data[["epoch"]] == 2, variable])))
    
    write_out(paste0('##### Epoch 3 \n'))
    write_out(paste0('* **Lowest Unique Values:** ', lowest_values(data[data[["epoch"]] == 3, variable])))
    write_out(paste0('* **Highest Unique Values:** ', highest_values(data[data[["epoch"]] == 3, variable])))
    
    write_out(paste0('##### Epoch 4 \n'))
    write_out(paste0('* **Lowest Unique Values:** ', lowest_values(data[data[["epoch"]] == 4, variable])))
    write_out(paste0('* **Highest Unique Values:** ', highest_values(data[data[["epoch"]] == 4, variable])))
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
  
  if (sum(is.na(data[[variable]])) > 0) {
    write_out("\n#### IDs of Participants with Missing Values by Epoch: \n")
    write_out(paste0('* **Epoch 1:** ', print_missing(data = data[data[["epoch"]] == 1, ], variable = variable, id = "map.id")))
    write_out(paste0('* **Epoch 2:** ', print_missing(data = data[data[["epoch"]] == 2, ], variable = variable, id = "map.id")))
    write_out(paste0('* **Epoch 3:** ', print_missing(data = data[data[["epoch"]] == 3, ], variable = variable, id = "map.id")))
    write_out(paste0('* **Epoch 4:** ', print_missing(data = data[data[["epoch"]] == 4, ], variable = variable, id = "map.id")))
  }
  
  write_out("\n")
  write_out("\\newpage")
}
