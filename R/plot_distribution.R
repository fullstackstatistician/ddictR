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
      theme_minimal() +
      xlab(NULL)
    ")
  } else if (inherits(data[[variable]], "Date")) {
    output.code <- glue::glue("
    ggplot(data = {data.name}) +
      geom_histogram(aes({variable}), color = 'black', bins = 50, na.rm = TRUE) +
      scale_x_date(labels = scales::date_format('%Y-%b'), date_breaks = '6 months') +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab(NULL)
    ")
  } else { # hist curve code from https://stackoverflow.com/a/46876971
    output.code <- glue::glue('
    density.df <- split({data.name}[, c("epoch", "{variable}")], {data.name}$epoch) %>% 
    purrr::map_df(
      ~ tibble(
        {variable} = seq(0.95*min(.x[["{variable}"]], na.rm = T), 1.05*max(.x[["{variable}"]], na.rm = T), length = 100),
        density = dnorm(x = {variable}, mean = mean(.x[["{variable}"]], na.rm = T), sd = sd(.x[["{variable}"]], na.rm = T)),
        n = nrow(.),
        bw = (max(.x[["{variable}"]], na.rm = T) - min(.x[["{variable}"]], na.rm = T)) / 30
      ),
      .id = "epoch"
    )

    ggplot() +
      geom_histogram(
        data = {data.name}, 
        aes(y = ..count.., x = {variable}), 
        binwidth = function(x) {{ (max(x, na.rm = T) - min(x, na.rm = T)) / 30 }}, 
        colour = "black", 
        na.rm = TRUE
      ) +
      facet_wrap(~ epoch, ncol = 2, labeller = label_epoch) +
      geom_line(data = density.df, aes(y = density*bw*n, x = {variable}), colour = "red") +
      theme_minimal() +
      xlab(NULL)
    ')
  }
  
  return(output.code)
}
