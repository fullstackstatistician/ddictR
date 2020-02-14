create_individual_entry <- function(data = merged.df, metadata = metadata.df, variable) {
  
  write_out(paste0('## Variable: ', variable, '\n'))
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "label"])) {
    write_out(paste0('### Label: ', metadata[metadata[["variable"]] == variable, "label"], "\n"))
  }
  
  write_out("\n")
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "derivation"])) {
    write_out(paste0('**Description:** ', metadata[metadata[["variable"]] == variable, "derivation"], '\n'))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "comments"])) {
    write_out(paste0('**Comments:** ', metadata[metadata[["variable"]] == variable, "comments"], '\n'))
  }
  
  # write_out("\n")
  
  write_out(paste0('### Distribution \n'))
  
  chunk_wrapper(
    plot_distribution(variable = variable), 
    options = c("echo = FALSE", "warning = FALSE", "message = FALSE", "fig.height = 4", "fig.width = 5.5", "fig.align = 'center'")
  )
  
  write_out(paste0('### Summary Statistics \n'))

  chunk_wrapper(
    paste0('summarize_data(variable = \"', variable, '\") %>% kable(format = \"latex\", booktabs = TRUE) %>% kable_styling(position = \"center\")')
    #options = c("echo = FALSE", "warning = FALSE", "message = FALSE", "results = 'asis'")
  )
  
  if (is.factor(data[[variable]]) | length(unique(data[[variable]])) < 10) {
    chunk_wrapper(
      # paste0('knitr::kable(summarize_levels(variable = \"', variable, '\"), format = "latex", booktabs = TRUE, align = c("l", rep("r", 5)))'),
      # paste0('kableExtra::kable_styling(knitr::kable(summarize_levels(variable = \"', variable, '\"), format = "latex", booktabs = TRUE, align = c("l", rep("r", 5))), latex_options = "striped")'),
      # summarize_levels(variable = variable)
      paste0('summarize_levels(variable = \"', variable, '\") %>% kable(format = \"latex\", booktabs = TRUE, col.names = NULL, align = c(\"l\", rep(\"r\", 10))) %>% kable_styling(position = \"center\") %>% add_header_above(c(\" \", \"Overall\" = 2, \"Epoch 1\" = 2, \"Epoch 2\" = 2, \"Epoch 3\" = 2, \"Epoch 4\" = 2))')
      # options = c("echo = FALSE", "warning = FALSE", "message = FALSE", "results = 'asis'")
    )
    # write_out(summarize_levels(variable = variable))
    # write_out(paste0('\n', summarize_levels(variable), '\n'))
  }
  
  if (is.numeric(data[[variable]]) | inherits(data[[variable]], "Date")) {
    write_out(paste0('### Extreme Values \n'))
    chunk_wrapper(paste0('tabulate_extreme_values(variable = \"', variable, '\") %>% kable(format = \"latex\", booktabs = TRUE) %>% kable_styling(position = \"center\")'))
  }
  
  # if (!is.na(metadata[metadata[["variable"]] == variable, "figure_label"])) {
  #   write_out(paste0('**Publication Figure Label:** ', metadata[metadata[["variable"]] == variable, "figure_label"], '\n'))
  # }

  if (!is.na(metadata[metadata[["variable"]] == variable, "covariates"])) {
    write_out(paste0('**Commonly Included Covariates:** ', metadata[metadata[["variable"]] == variable, "covariates"], '\n'))
  }
  
  if (!is.na(metadata[metadata[["variable"]] == variable, "related_variables"])) {
    write_out(paste0('**Related Variables:** ', metadata[metadata[["variable"]] == variable, "related_variables"], '\n'))
  }
  
  if (sum(is.na(data[[variable]])) > 0) {
    write_out("\n### IDs of Participants with Missing Values by Epoch: \n")
    write_out(paste0('* **Epoch 1:** ', print_missing(data = data[data[["epoch"]] == 1, ], variable = variable, id = "map.id")))
    write_out(paste0('* **Epoch 2:** ', print_missing(data = data[data[["epoch"]] == 2, ], variable = variable, id = "map.id")))
    write_out(paste0('* **Epoch 3:** ', print_missing(data = data[data[["epoch"]] == 3, ], variable = variable, id = "map.id")))
    write_out(paste0('* **Epoch 4:** ', print_missing(data = data[data[["epoch"]] == 4, ], variable = variable, id = "map.id")))
  }
  
  write_out("\n")
  write_out("\\newpage")
}
