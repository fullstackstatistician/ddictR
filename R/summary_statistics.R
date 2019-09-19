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
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  if (summary.df$n == summary.df$nmiss) {
    summary.df[, -c(1:3)] <- NA
  }
  
  return(summary.df)
}

summary_statistics.factor <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  return(summary.df)
}

summary_statistics.Date <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  if (summary.df$n == summary.df$nmiss) {
    summary.df[, -c(1:3)] <- NA
  }
  
  return(summary.df)
}

summary_statistics.character <- function(x) {
  summary.df <- data.frame(
    n = length(x),
    nmiss = sum(is.na(x)),
    distinct = length(unique(na.omit(x))),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  return(summary.df)
}
