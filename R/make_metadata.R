make_metadata <- function(data = merged.df, derivation = NULL, comments = NULL, figure_label = NULL, covariates = NULL, related = NULL, units = NULL, ...) {
  metadata.df <- data.frame(
    variable = names(data),
    label = Hmisc::label(data),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(derivation)) {
    if (!all(derivation$variable %in% metadata.df$variable)) {
      warning(
        paste0(
          "The following variables with derivation information are not in the main data set: ",
          paste0(setdiff(derivation$variable, metadata.df$variable), collapse = ", "),
          ".\n\n"
        )
      )
    }
    metadata.df <- dplyr::full_join(x = metadata.df, y = derivation, by = "variable")
  } else {
    metadata.df$derivation <- NA
  }
  
  if (!is.null(comments)) {
    if (!all(comments$variable %in% metadata.df$variable)) {
      warning(
        paste0(
          "The following variables with comments are not in the main data set: ",
          paste0(setdiff(comments$variable, metadata.df$variable), collapse = ", "),
          ".\n\n"
        )
      )
    }
    metadata.df <- dplyr::full_join(x = metadata.df, y = comments, by = "variable")
  } else {
    metadata.df$comments <- NA
  }
  
  if (!is.null(figure_label)) {
    if (!all(figure_label$variable %in% metadata.df$variable)) {
      warning(
        paste0(
          "The following variables with figure labels are not in the main data set: ",
          paste0(setdiff(figure_label$variable, metadata.df$variable), collapse = ", "),
          ".\n\n"
        )
      )
    }
    metadata.df <- dplyr::full_join(x = metadata.df, y = figure_label, by = "variable")
  } else {
    metadata.df$figure_label <- NA
  }

  if (!is.null(covariates)) {
    if (!all(covariates$variable %in% metadata.df$variable)) {
      warning(
        paste0(
          "The following variables with covariate information are not in the main data set: ",
          paste0(setdiff(covariates$variable, metadata.df$variable), collapse = ", "),
          ".\n\n"
        )
      )
    }
    metadata.df <- dplyr::full_join(x = metadata.df, y = covariates, by = "variable")
  } else {
    metadata.df$covariates <- NA
  }
  
  if (!is.null(related)) {
    if (!all(related$variable %in% metadata.df$variable)) {
      warning(
        paste0(
          "The following variables with related variable information are not in the main data set: ",
          paste0(setdiff(related$variable, metadata.df$variable), collapse = ", "),
          ".\n\n"
        )
      )
    }
    metadata.df <- dplyr::full_join(x = metadata.df, y = related, by = "variable")
  } else {
    metadata.df$related_variables <- NA
  }
  
  if (!is.null(units)) {
    if (!all(units$variable %in% metadata.df$variable)) {
      warning(
        paste0(
          "The following variables with unit data are not in the main data set: ",
          paste0(setdiff(units$variable, metadata.df$variable), collapse = ", "),
          ".\n\n"
        )
      )
    }
    metadata.df <- dplyr::full_join(x = metadata.df, y = units, by = "variable")
  } else {
    metadata.df$units <- NA
  }
  
  return(metadata.df)
}
