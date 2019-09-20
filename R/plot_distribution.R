plot_distribution <- function(data = merged.df, variable) {
  data.name <- deparse(substitute(data))
  
  if (is.character(data[[variable]])) {
    output.code <- ""
  } else if (is.factor(data[[variable]]) | length(unique(data[[variable]])) < 15) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_bar(aes({variable})) +
      theme_minimal()
    ")
  } else if (inherits(data[[variable]], "Date")) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_histogram(aes({variable}), color = 'black', bins = 50) +
      scale_x_date(labels = scales::date_format('%Y-%b'), date_breaks = '6 months') +
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

    ggplot(data = {data.name}) +
      geom_histogram(aes({variable}), color = 'black', bins = 30) +
      theme_minimal() +
      stat_function(
        fun = function(x, mean, sd, n, bw) {{
          dnorm(x = x, mean = mean, sd = sd) * bw * n
        }},
        args = c(mean = m, sd = sd, n = n, bw = bw), 
        color = 'red'
      )
    ")
  }
  
  return(output.code)
}
