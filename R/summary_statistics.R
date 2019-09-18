summary_statistics.numeric <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    p5 = quantile(x, .05, na.rm = TRUE),
    p25 = quantile(x, .25, na.rm = TRUE),
    p50 = quantile(x, .50, na.rm = TRUE),
    p75 = quantile(x, .75, na.rm = TRUE),
    p95 = quantile(x, .95, na.rm = TRUE),
    lowest = paste(head(sort(x, na.last = NA), n = 5), collapse = ", "),
    highest = paste(tail(sort(x, na.last = NA), n = 5), collapse = ", "),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  output.list <- list(summary.df = summary.df)
  
  if (length(unique(na.omit(x))) < 5) {
    levels.df <- as.data.frame(table(x)) %>%
      mutate(Prop = round(Freq/length(x), 2)) %>%
      setNames(c("Value", "Frequency", "Proportion"))
    
    output.list <- list(summary.df = summary.df, levels.df = levels.df)
  }
  
  return(output.list)
}

summary_statistics.factor <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  levels.df <- as.data.frame(table(x)) %>%
    mutate(Prop = round(Freq/length(x), 2)) %>%
    setNames(c("Value", "Frequency", "Proportion"))
  
  output.list <- list(summary.df = summary.df, levels.df = levels.df)
  
  return(output.list)
}

summary_statistics.Date <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    lowest = paste(head(sort(x, na.last = NA), n = 5), collapse = ", "),
    highest = paste(tail(sort(x, na.last = NA), n = 5), collapse = ", "),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  output.list <- list(summary.df = summary.df)
  
  if (length(unique(na.omit(x))) < 5) {
    levels.df <- as.data.frame(table(x)) %>%
      mutate(Prop = round(Freq/length(x), 2)) %>%
      setNames(c("Value", "Frequency", "Proportion"))
    
    output.list <- list(summary.df = summary.df, levels.df = levels.df)
  }
  
  return(output.list)
}

summary_statistics.character <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  output.list <- list(summary.df = summary.df)
  
  if (length(unique(na.omit(x))) < 5) {
    levels.df <- as.data.frame(table(x)) %>%
      mutate(Prop = round(Freq/length(x), 2)) %>%
      setNames(c("Value", "Frequency", "Proportion"))
    
    output.list <- list(summary.df = summary.df, levels.df = levels.df)
  }
  
  return(output.list)
}
