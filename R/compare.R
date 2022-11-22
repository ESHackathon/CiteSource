#' Count number of unique and non-unique citations from different sources, labels, and strings
#' @export
#' @param unique_data from ASySD, merged unique rows with duplicate IDs
#' @return dataframe with indicators of where a citation appears, with source/label/string as column

count_unique <- function(unique_data) {
  unique_data %>%
    dplyr::filter(!.data$cite_source == "") %>%
    dplyr::select(.data$duplicate_id, .data$cite_source,  .data$cite_label,  .data$cite_string, .data$record_ids) %>% 
    tidyr::separate_rows(.data$cite_source, convert = TRUE) %>%
    tidyr::separate_rows(.data$cite_label, convert = TRUE) %>%
    tidyr::separate_rows(.data$cite_string, convert = TRUE) %>%
    dplyr::group_by(.data$duplicate_id) %>%
    dplyr::mutate(unique = ifelse(length(unique(.data$cite_source)) == 1, TRUE, FALSE),
                  type = ifelse(.data$unique, "unique", "duplicated") %>% factor(levels = c("unique", "duplicated"))) %>%
    dplyr::ungroup() %>%
    unique()
}

#' Compare duplicate citations across sources, labels, and strings
#' 
#' @export
#' @param unique_data from ASySD, merged unique rows with duplicate IDs
#' @param comp_type Specify which fields are to be included. One or more of "sources", "strings" or "labels" - defaults to all.
#' @return dataframe with indicators of where a citation appears, with sources/labels/strings as columns

compare_sources <- function(unique_data, comp_type = c("sources", "strings", "labels")) {
  
  out <- list()
  
  if ("sources" %in% comp_type) {
  
  source_comparison <- unique_data %>%
    dplyr::select(.data$duplicate_id, .data$cite_source, .data$record_ids) %>%
    dplyr::filter(!cite_source == "") %>%
    tidyr::separate_rows(.data$cite_source, sep = ", ", convert = TRUE) %>%
    unique() %>%
    tidyr::pivot_wider(id_col = .data$duplicate_id, names_prefix="source__", names_from = .data$cite_source, values_from=c(.data$record_ids),
                values_fn =  function(x) TRUE,
                values_fill = FALSE)
  
  out <- c(out, list(source_comparison))
  }
  
  if ("strings" %in% comp_type) {
    
  source_comparison <- unique_data %>%
    dplyr::select(.data$duplicate_id, .data$cite_string, .data$record_ids) %>%
    dplyr::filter(!.data$cite_string == "") %>%
    tidyr::separate_rows(.data$cite_string, sep = ", ", convert = TRUE) %>%
    unique() %>%
    tidyr::pivot_wider(id_col = .data$duplicate_id, names_prefix="string__", names_from = .data$cite_string, values_from=c(.data$record_ids),
                       values_fn =  function(x) TRUE,
                       values_fill = FALSE)
  
  out <- c(out, list(source_comparison))
  }
  
  if ("labels" %in% comp_type) {
    
    source_comparison <- unique_data %>%
      dplyr::select(.data$duplicate_id, .data$cite_label, .data$record_ids) %>%
      dplyr::filter(!cite_label == "") %>%
      tidyr::separate_rows(.data$cite_label, sep = ", ", convert = TRUE) %>%
      unique() %>%
      tidyr::pivot_wider(id_col = .data$duplicate_id, names_prefix="label__", names_from = .data$cite_label, 
                         values_from = c(.data$record_ids),
                         values_fn =  function(x) TRUE,
                         values_fill = FALSE)
    out <- c(out, list(source_comparison))
  }
  
  if (length(out) == 0) stop('comp_type must be one or more of "sources", "strings" or "labels"')
  
  purrr:::reduce(out, dplyr::left_join, by = "duplicate_id")
  
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
