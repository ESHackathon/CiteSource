#' Count number of unique and non-unique citations from different sources, labels, and strings
#' @export
#' @param unique_data from ASySD, merged unique rows with duplicate IDs
#' @param include_references Should bibliographic detail be included in return?
#' @return dataframe with indicators of where a citation appears, with source/label/string as column
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations
#' dedup_results <- dedup_citations(examplecitations)
#'
#' # Count unique and non-unique citations
#' count_unique(dedup_results)

count_unique <- function(unique_data, include_references = FALSE) {
  # Start a pipeline with the input data
  out <- unique_data %>%
    # Filter out rows where 'cite_source' is empty
    dplyr::filter(!.data$cite_source == "") %>%
    # Select specific columns
    dplyr::select(.data$duplicate_id, .data$cite_source, .data$cite_label, .data$cite_string, tidyselect::any_of("record_ids")) %>%
    # Expand metadata columns (replaces three separate_rows calls)
    expand_metadata_columns(columns = c("cite_source", "cite_label", "cite_string")) %>%
    # Group by 'duplicate_id'
    dplyr::group_by(.data$duplicate_id) %>%
    # Add 'unique' and 'type' columns
    dplyr::mutate(
      unique = ifelse(length(unique(.data$cite_source)) == 1, TRUE, FALSE),  # 'unique' is TRUE if 'cite_source' is unique
      type = ifelse(.data$unique, "unique", "duplicated") %>% factor(levels = c("unique", "duplicated"))  # 'type' is 'unique' if 'unique' is TRUE, 'duplicated' otherwise
    ) %>%
    # Ungroup the data
    dplyr::ungroup() %>%
    # Remove duplicate rows
    unique()

  # If 'include_references' is TRUE, join 'out' with 'unique_data' on 'duplicate_id'
  if (include_references == TRUE) {
    out %>% dplyr::left_join(unique_data %>% dplyr::select(-dplyr::all_of(setdiff(intersect(names(.), names(out)), "duplicate_id"))), by = "duplicate_id")
  } else {
    # Otherwise, return 'out' as is
    out
  }
}

#' Compare duplicate citations across sources, labels, and strings
#'
#' @export
#' @param unique_data from ASySD, merged unique rows with duplicate IDs
#' @param comp_type Specify which fields are to be included. One or more of "sources", "strings" or "labels" - defaults to all.
#' @param include_references Should bibliographic detail be included in return?
#' @return dataframe with indicators of where a citation appears, with sources/labels/strings as columns
#' @examples
#' if (interactive()) {
#'   # Load example data from the package
#'   examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#'   examplecitations <- readRDS(examplecitations_path)
#'
#'   # Deduplicate citations and compare sources
#'   dedup_results <- dedup_citations(examplecitations)
#'   compare_sources(unique_citations, comp_type = "sources")
#' }


compare_sources <- function(unique_data, comp_type = c("sources", "strings", "labels"), include_references = FALSE) {
  
  # Map comp_type to column names
  column_map <- c(
    "sources" = "cite_source",
    "strings" = "cite_string", 
    "labels" = "cite_label"
  )
  
  columns_to_expand <- column_map[comp_type]
  columns_to_expand <- columns_to_expand[!is.na(columns_to_expand)]
  
  if (length(columns_to_expand) == 0) {
    stop('comp_type must be one or more of "sources", "strings" or "labels"')
  }
  
  out <- list(unique_data %>% dplyr::select("duplicate_id"))
  
  # Process each column type
  for (i in seq_along(columns_to_expand)) {
    comp_name <- names(columns_to_expand)[i]
    col_name <- columns_to_expand[i]
    prefix <- paste0(stringr::str_sub(comp_name, 1, -2), "__")  # "source__", "label__", etc.
    
    expanded <- unique_data %>%
      dplyr::select(.data$duplicate_id, !!rlang::sym(col_name), tidyselect::any_of("record_ids")) %>%
      dplyr::filter(!is.na(!!rlang::sym(col_name)), !!rlang::sym(col_name) != "") %>%
      expand_single_metadata_column(col_name) %>%
      tidyr::pivot_wider(
        id_cols = .data$duplicate_id,
        names_prefix = prefix,
        names_from = !!rlang::sym(col_name),
        values_from = !!rlang::sym(col_name),
        values_fn = function(x) TRUE,
        values_fill = FALSE
      )
    
    out <- c(out, list(expanded))
    
    # Special warning for labels (keep existing logic)
    if (comp_name == "labels" && any(stringr::str_detect(names(expanded), "[Ss]earch"))) {
      search_stage <- stringr::str_subset(names(expanded), "[Ss]earch")
      if (length(search_stage) == 1) {
        not_in_search <- sum(!expanded[[search_stage]])
        if (not_in_search > 0) {
          warning(
            "Beware: ", not_in_search, " records were not included in ", search_stage, " but in other labels.",
            " *If* this label indicates the full search stage, this might indicate that you ommitted a source, ",
            "or that the deduplication did not go right. Please treat results with caution until you fix this, ",
            "e.g., by using export_csv and then reimport_csv."
          )
        }
      }
    }
  }

  out <- purrr::reduce(out, dplyr::left_join, by = "duplicate_id")

  # Deals with entries missing source or label
  out <- out %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, FALSE)))
    
  if (include_references == TRUE) {
    out %>% dplyr::left_join(unique_data %>% dplyr::select(-dplyr::all_of(setdiff(intersect(names(.), names(out)), "duplicate_id"))), by = "duplicate_id")
  } else {
    out
  }
}

