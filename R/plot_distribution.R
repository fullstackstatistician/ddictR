plot_distribution <- function(data = merged.df, variable) {
  data.name <- deparse(substitute(data))
  
  if (is.character(data[[variable]])) {
    output.code <- ""
  } else if (is.factor(data[[variable]]) | length(unique(data[[variable]])) < 15) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_bar(aes(factor({variable})), na.rm = FALSE) +
      scale_x_discrete(label = c('Ambiguous At Risk' = 'AAR')) +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal()
    ")
  } else if (inherits(data[[variable]], "Date")) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_histogram(aes({variable}), bins = 50, na.rm = TRUE) +
      scale_x_date(labels = scales::date_format('%Y-%b'), date_breaks = '6 months') +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ")
  } else {
    output.code <- glue::glue('
    ggplot(data = {data.name}) +
      geom_histogram(
        aes(x = {variable}, y = ..count..), 
        binwidth = function(x) {{ (max(x, na.rm = T) - min(x, na.rm = T)) / 30 }}, 
        na.rm = TRUE
      ) +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal() 
    ')
  }
  
  return(output.code)
}
