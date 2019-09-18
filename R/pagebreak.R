pagebreak <- function() {
  if (knitr::is_latex_output()) {
    return("\\newpage")
  } else {
    return('<div style="page-break-before: always;" />')
  }
}
