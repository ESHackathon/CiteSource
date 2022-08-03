#' Compare duplicate citations across sources, labels, and strings
#' 
#' @export
#' @param unique_data from ASySD, merged unique rows with duplicate IDs
#' @param comp_type argument to specify comparison of strings, sources, or labels?
#' @return dataframe with indicators of where a citation appears, with source/label/string as column

compare_sources <- function(unique_data, comp_type=c("sources", "strings", "labels")){
  
  comp_type <- match.arg(comp_type)
  
  if(comp_type == "sources"){
  
  source_comparison <- unique_data %>%
    dplyr::select(.data$duplicate_id, .data$cite_source, .data$record_ids) %>%
    tidyr::separate_rows(.data$cite_source, convert = TRUE) %>%
    unique() %>%
    tidyr::pivot_wider(id_col = .data$duplicate_id, names_prefix="source_", names_from = .data$cite_source, values_from=c(.data$record_ids),
                values_fn =  function(x) TRUE,
                values_fill = FALSE)
  }
  
  else if(comp_type == "strings"){
  
  source_comparison <- unique_data %>%
    dplyr::select(.data$duplicate_id, .data$cite_string, .data$record_ids) %>%
    tidyr::separate_rows(.data$cite_string, convert = TRUE) %>%
    unique() %>%
    tidyr::pivot_wider(id_col = .data$duplicate_id, names_prefix="source_", names_from = .data$cite_string, values_from=c(.data$record_ids),
                       values_fn =  function(x) TRUE,
                       values_fill = FALSE)
  }
  
  else{
    
    source_comparison <- unique_data %>%
      dplyr::select(.data$duplicate_id, .data$cite_label, .data$record_ids) %>%
      tidyr::separate_rows(.data$cite_label, convert = TRUE) %>%
      unique() %>%
      tidyr::pivot_wider(id_col = .data$duplicate_id, names_prefix="source_", names_from = .data$cite_label, values_from=c(.data$record_ids),
                         values_fn =  function(x) TRUE,
                         values_fill = FALSE)
  }
  
  return(source_comparison)
}



#   unique_data$author <- gsub(",.*", "", unique_data$author)
#   
#   #TODO: does this need to be made unique? It seems that there can be multiple records with same author-year?
#   pub_id <- unique_data %>%
#     dplyr::select(.data$author, .data$year, .data$duplicate_id) %>%
#     dplyr::mutate(pub_id = paste0(.data$author, "-", .data$year)) %>%
#     dplyr::select(-.data$author, -.data$year, .data$pub_id, dplyr::everything())
#   
#   db_comparison <-  dplyr::left_join(db_comparison, pub_id, by="duplicate_id")
# }
#   
