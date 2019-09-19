create_entry <- function(data = merged.df, variable, epoch) {
  if (variable %nin% names(data)) {
    stop(paste0("Variable ", variable, " does not exist in the data set ", deparse(substitute(data)), ".\n\n"))
  }
  
  if (epoch == 0) {
    subset.df <- data
  } else if (epoch != 0) {
    subset.df <- subset(data, epoch == epoch)
  } else {
    stop("Invalid value for epoch. Please specify 0 for overall statistics or 1, 2, 3, or 4 for epoch-specific statistics.\n\n")
  }
  
  if (is.numeric(subset.df[[variable]])) {
    output.list <- summary_statistics.numeric(subset.df[[variable]])
  } else if (is.factor(subset.df[[variable]])) {
    output.list <- summary_statistics.factor(subset.df[[variable]])
  } else if (inherits(subset.df[[variable]], "Date")) {
    output.list <- summary_statistics.Date(subset.df[[variable]])
  } else if (is.character(subset.df[[variable]])) {
    output.list <- summary_statistics.character(subset.df[[variable]])
  } else {
    stop("Class for variable ", variable, " is not numeric, factor, Date, or character. Please check data type.")
  }
  
  variable.label <- Hmisc::label(subset.df[[variable]])
  
  ###
  
  h1.text <- ifelse(
    nchar(variable.label) > 0, 
    paste0("# ", variable, ": ", variable.label, " \n"),
    paste0("# ", variable, " \n")
  )
  
  h2.text <- ifelse(
    epoch == 0,
    paste0("## Overall Statistics \n")
    paste0("## Epoch ", epoch, "Statistics \n")
  )
  
  group.text <- attr(subset.df[[variable]], "group")
  description.text <- attr(subset.df[[variable]], "description")
  units.text <- attr(subset.df[[variable]], "units")
  derivation.text <- attr(subset.df[[variable]], "derivation")
  figure_label.text <- attr(subset.df[[variable]], "figure_label")
  comments.text <- attr(subset.df[[variable]], "comments")

### 

  if (epoch == 0) {
    pagebreak()
    
    cat(h1.text)
    cat("\n")
    cat(paste0("Variable Group: ", group.text, "\n\n"))
    
    if (!is.null(description.text)) {
      cat(paste0("Description: ", description.text, "\n\n"))
    }
    if (!is.null(units.text)) {
      cat(paste0("Units: ", units.text, "\n\n"))
    }
    if (!is.null(derivation.text)) {
      cat(paste0("Derivation: ", description.text, "\n\n"))
    }
    if (!is.null(figure_label.text)) {
      cat(paste0("Figure Label: ", figure_label.text, "\n\n"))
    }
    if (!is.null(comments.text)) {
      cat(paste0("Comments: ", comments.text, "\n\n"))
    }
  }
  
  cat(h2.text)
  print(output.list$summary.df)
  cat("\n\n")
  if (exists("levels.df", where = output.list)) {
    print(output.list$levels.df)
    cat("\n\n")
  }
}
