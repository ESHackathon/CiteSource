#' Compare duplicate citations across sources
#' 
#' @export
#' @param unique_data from ASySD, merged unique rows with duplicate IDs
#' @return dataframe with indicators of where a citation appears, with source as column

compare_sources <- function(unique_data){
  
  db_comparison <- unique_data %>%
    dplyr::select(.data$duplicate_id, .data$database, .data$record_id) %>%
    tidyr::separate_rows(.data$database, convert = TRUE) %>%
    unique() %>%
    tidyr::pivot_wider(id_col = .data$duplicate_id, names_from=.data$database, values_from=c(.data$record_id),
                values_fn =  function(x) TRUE,
                values_fill = FALSE)
  
  
  unique_data$author <- gsub(",.*", "", unique_data$author)
  
  #TODO: does this need to be made unique? It seems that there can be multiple records with same author-year?
  pub_id <- unique_data %>%
    dplyr::select(.data$author, .data$year, .data$duplicate_id) %>%
    dplyr::mutate(pub_id = paste0(.data$author, "-", .data$year)) %>%
    dplyr::select(-.data$author, -.data$year, .data$pub_id, dplyr::everything())
  
  db_comparison <-  dplyr::left_join(db_comparison, pub_id, by="duplicate_id")
}
  
