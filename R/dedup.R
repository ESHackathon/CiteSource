#' Deduplicate citations - ASySD wrapper
#'
#' This function deduplicates citation data. Note that duplicates are assumed to published
#' in the same journal, so pre-prints and similar results will not be identified here.
#'
#' @export
#' @param raw_citations Citation dataframe with relevant columns
#' @param manual logical. If TRUE, manually specify pairs of duplicates to merge. Default is FALSE.
#' @param show_unknown_tags When a label, source, or other merged field is missing, do you want this to show as "unknown"?
#' @return unique citations formatted for CiteSource
#'
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations without manually specifying pairs and without showing unknown tags
#' dedup_results <- dedup_citations(examplecitations)
#'
#' # Deduplicate citations with manual specification of pairs and showing unknown tags
#' dedup_results_manual_unknown <- dedup_citations(
#'   examplecitations, 
#'   manual = TRUE, 
#'   show_unknown_tags = TRUE
#'   )

dedup_citations <- function(raw_citations, manual=FALSE, show_unknown_tags=FALSE){
  
  # rename or coalesce columns
  targets <- c("journal", "number", "pages", "isbn", "record_id")
  sources <- c("source", "issue", "start_page", "issn", "ID")
  raw_citations <- add_cols(raw_citations, sources)
  
  for (i in seq_along(targets)) {
    if (targets[i] %in% names(raw_citations)) {
      raw_citations[[targets[i]]] <- dplyr::coalesce(raw_citations[[targets[i]]], raw_citations[[sources[i]]])
    }  else {
      raw_citations[[targets[i]]] <- raw_citations[[sources[i]]]
    }
  }
  
  raw_citations <- add_cols(raw_citations, c("record_id", "cite_label", "cite_source", "cite_string"))
 
  raw_citations$source <- raw_citations$cite_source
  raw_citations$label <- raw_citations$cite_label
  
  dedup_results <- ASySD::dedup_citations(raw_citations, merge_citations = TRUE, extra_merge_fields = "cite_string", show_unknown_tags = show_unknown_tags)
  
  if(manual == FALSE){
    
  unique_post_dedup <- dedup_results$unique
  unique_post_dedup$cite_source = unique_post_dedup$source
  unique_post_dedup$cite_label = unique_post_dedup$label
  
  # Remove temporary columns
  unique_post_dedup <- unique_post_dedup %>%
    dplyr::select(-source, -label)
  
  return(unique_post_dedup)
  
  } else {
    
    unique_post_dedup <- dedup_results
    unique_post_dedup$unique$cite_source = unique_post_dedup$unique$source
    unique_post_dedup$unique$cite_label = unique_post_dedup$unique$label
    
    # Remove temporary columns
    unique_post_dedup$unique <- unique_post_dedup$unique %>%
      dplyr::select(-source, -label)
    
    return(unique_post_dedup)
  }
  
}

#' Remove pairs with manual dedup - ASySD wrapper
#'
#' This function deduplicates citation data. Note that duplicates are assumed to published
#' in the same journal, so pre-prints and similar results will not be identified here.
#'
#' @export
#' @param unique_citations Unique citations post deduplication
#' @param additional_pairs TRUE duplicate pairs
#' @return unique citations formatted for CiteSource
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations
#' dedup_results <- dedup_citations(examplecitations)
#' 
dedup_citations_add_manual <- function(unique_citations, additional_pairs) {
  
  unique_citations$source = unique_citations$cite_source
  unique_citations$label = unique_citations$cite_label
  
  dedup_results <- ASySD::dedup_citations_add_manual(unique_citations, additional_pairs = additional_pairs,
                                                     extra_merge_fields = "cite_string")

  dedup_results$cite_source <- dedup_results$source
  dedup_results$cite_label <- dedup_results$label
  
  # Remove temporary columns
  dedup_results <- dedup_results %>%
    dplyr::select(-source, -label)
  
  return(dedup_results)
  
}


#' ####------ Add columns ------ ####

#' This function adds citesource columns to citation data if missing
#' @param raw_citations Citation dataframe with relevant columns
#' @param cname column names which are required in dataframe
#' @return Dataframe of citations with id
#' @noRd
add_cols <- function(raw_citations, cname) {
  add <- cname[!cname %in% names(raw_citations)]
  
  if (length(add) != 0) raw_citations[add] <- NA
  raw_citations
}


  

