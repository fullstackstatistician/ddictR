create_ddict <- function(data = merged.df, 
                         file_name = paste0("vmap_data_dictionary_m", format(Sys.time(),'%Y%m%d_%H%M')), 
                         save_directory = "~/dev", 
                         overwrite = TRUE, 
                         report_title = "VMAP Data Dictionary", 
                         data_download_date = NULL, 
                         data_merge_date = NULL
                         ) {
  
  ### set up file names and paths ###
  
  rmd_file_name = paste0(file_name, ".Rmd")
  pdf_file_name = paste0(file_name, ".pdf")
  
  rmd_file_path <- file.path(save_directory, rmd_file_name)
  pdf_file_path <- file.path(save_directory, pdf_file_name)
  
  # delete existing files, if requested
  if (overwrite & file.exists(rmd_file_path)) {
    file.remove(rmd_file_path)
  }
  
  if (overwrite & file.exists(pdf_file_path)) {
    file.remove(pdf_file_path)
  }
  
  # open file connection
  file_connection <<- file(rmd_file_path, "w")
  
  ### YAML header ###
  
  if (!is.null(data_download_date) & !is.null(data_merge_date)) {
    subtitle <- paste0("Data Download Date: ", data_download_date, " | Data Merge Date: ", data_merge_date)
  } else {
    subtitle <- NULL
  }
  
  write_out("---")
  write_out(paste("title:", report_title))
  if (!is.null(subtitle)) write_out(paste0('subtitle: \"', subtitle, '\"'))
  write_out(paste("date:", format(Sys.time(),'%B %d, %Y %H:%M')))
  write_out("output: pdf_document")
  write_out("---")
  write_out("\n")
  
  ### Hidden Setup Chunk ###
  
  chunk_wrapper(
    x = paste0('library(ddictR) \nlibrary(Hmisc) \nlibrary(dplyr) \nlibrary(ggplot2)'),
    quiet = TRUE
  )
  
  ### Entries ###
  
  write_out("# Variables")
  
  for (variable.i in names(data)) {
    create_individual_entry(data = data, variable = variable.i)
  }
  
  # force flush and close connection
  flush(file_connection)
  close(file_connection)
  
  ### Knit Document ###
  #rmarkdown::render(rmd_file_path, 'pdf_document', pdf_file_name)
}
