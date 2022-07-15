#' Compare duplicate citations across sources
#' 
#' @export
#' @param unique_data from ASySD, merged unqiue rows with duplicate IDs
#' @return dataframe with indicators of where a citation appears, with source as column

source_comparison <- function(unique_data){
  
  db_comparison <- unique_data %>%
    select(duplicate_id, database, record_id) %>%
    separate_rows(database, convert = TRUE) %>%
    unique() %>%
    pivot_wider(id_col = duplicate_id, names_from=database, values_from=c(record_id),
                values_fn =  function(x) TRUE,
                values_fill = FALSE)
  
  
  unique_data$author <- gsub(",.*", "", unique_data$author)
  
  pub_id <- unique_data %>%
    select(author, year, duplicate_id) %>%
    mutate(pub_id = paste0(author, "-", year)) %>%
    select(-author, -year) %>%
    select(pub_id, everything())
  
  db_comparison <- left_join(db_comparison, pub_id, by="duplicate_id")
}
  
