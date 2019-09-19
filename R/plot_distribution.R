plot_distribution <- function(data = merged.df, variable) {
  if (is.null(label)) {
    label <- variable
  }
  
  if (epoch == 0) {
    subset.df <- data
  } else if (epoch != 0) {
    subset.df <- subset(data, epoch == epoch)
  } else {
    stop("Invalid value for epoch. Please specify 0 for overall statistics or 1, 2, 3, or 4 for epoch-specific statistics.\n\n")
  }
  
  if (is.numeric(subset.df[[variable]])) {
    sprintf(
      "var <- subset.df[['%s']]\n
       bw <- (max(var, na.rm = T) - min(var, na.rm = T)) / 30\n 
       m <- mean(var, na.rm = T)\n
       sd <- sd(var, na.rm = T)\n
       n <- length(!is.na(var))\n
                   
        class(df[['%s']]) <- NULL
        ggplot(data = na.omit(df)) +
          geom_histogram(aes(%s), color = 'black', bins = 30) +
          theme_minimal() +
          stat_function(fun = function(x, mean, sd, n, bw) {
            dnorm(x = x, mean = mean, sd = sd) * bw * n},
            args = c(mean = m, sd = sd, n = n, bw = bw), color = 'red')",
      variable, 
      variable, 
      variable
    )
  } else if (is.factor(subset.df[[variable]])) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.factor(x[[variable]])}
    )
  } else if (inherits(subset.df[[variable]], "Date")) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.Date(x[[variable]])}
    )
  } else if (is.character(subset.df[[variable]])) {
    output.df <- purrr::map_dfr(
      data.list,
      function(x) {summary_statistics.character(x[[variable]])}
    )
  } else {
    stop("Class for variable ", variable, " is not numeric, factor, Date, or character. Please check data type.\n\n")
  }
  
}
