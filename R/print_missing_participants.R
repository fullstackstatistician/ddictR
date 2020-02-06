print_missing_participants <- function(data = merged.df, epoch) {
  epoch <- as.character(epoch)
  
  table(data[, c("map.id", "epoch")]) %>%
    as.data.frame.matrix() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(!!rlang::sym(epoch) %in% 0) %>%
    dplyr::pull(rowname)
}
