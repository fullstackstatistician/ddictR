print_missing_participants <- function(data = merged.df, epoch) {
  if (!as.character(epoch) %in% unique(data[["epoch"]])) {
    stop(paste0("Please specify a valid epoch: ", paste0(unique(data[["epoch"]]), collapse = ", ")))
  }
  
  table(data[, c("map.id", "epoch")]) %>%
    as.data.frame.matrix() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(!!rlang::sym(as.character(epoch)) %in% 0) %>%
    dplyr::pull(rowname)
}
