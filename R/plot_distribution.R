plot_distribution <- function(data = merged.df, variable) {
  data.name <- deparse(substitute(data))
  
  if (is.character(data[[variable]])) {
    output.code <- ""
  } else if (is.factor(data[[variable]]) | length(unique(data[[variable]])) < 15) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_bar(aes(factor({variable})), na.rm = FALSE) +
      scale_x_discrete(name = '{variable}', label = c('Ambiguous At Risk' = 'AAR')) +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal()
    ")
  } else if (inherits(data[[variable]], "Date")) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_histogram(aes({variable}), color = 'black', bins = 50, na.rm = TRUE) +
      scale_x_date(labels = scales::date_format('%Y-%b'), date_breaks = '6 months') +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ")
  } else { # hist curve code from https://stackoverflow.com/a/46876971
    output.code <- glue::glue("
    var <- merged.df[['{variable}']]
    bw <- (max(var, na.rm = T) - min(var, na.rm = T)) / 30
    m <- mean(var, na.rm = T)
    sd <- sd(var, na.rm = T)
    n <- length(!is.na(var))
    
    ggplot(data = {data.name}, aes({variable})) +
      geom_histogram(aes(y = ..density..), binwidth = bw, colour = 'black', na.rm = TRUE)  +
      stat_function(fun = dnorm, args = list(mean = m, sd = sd), color = 'red') +
      scale_y_continuous(
        name = 'Density', 
        sec.axis = sec_axis(trans = ~ . * bw * n, name = 'Count')
      ) +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal()
    ")
  }
  
  return(output.code)
}
