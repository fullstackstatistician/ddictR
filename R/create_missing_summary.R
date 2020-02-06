create_missing_summary <- function(data = merged.df, epoch = 1:4) {
  for (epoch.i in epoch) {
    write_out(paste0("## Epoch ", epoch.i, " \n"))
    
    if (epoch.i %in% 1) {
      write_out("There are no missing participants for this epoch.")
    } else {
      write_out(paste0(print_missing_participants(data, epoch.i), collapse = ", "))
    }
    
    write_out("\n")
  }
}
