#' Deduplicate citations - ASySD wrapper
#'
#' This function deduplicates citation data. Note that duplicates are assumed to published
#' in the same journal, so pre-prints and similar results will not be identified here.
#'
#' @export
#' @param raw_citations Citation dataframe with relevant columns
#' @return unique citations formatted for CiteSource
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations
#' dedup_results <- dedup_citations(examplecitations)
#' 

dedup_citations <- function(raw_citations, manual=FALSE, shiny_progress=FALSE){
  
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
  
  dedup_results <- ASySD::dedup_citations(raw_citations, merge_citations = TRUE, extra_merge_fields = "cite_string", shiny_progress=shiny_progress, show_unknown_tags = FALSE)
  
  if(manual == FALSE){
    
  unique_post_dedup <- dedup_results$unique
  unique_post_dedup$cite_source = unique_post_dedup$source
  unique_post_dedup$cite_label = unique_post_dedup$label
  
  return(unique_post_dedup)
  
  } else {
    
    unique_post_dedup <- dedup_results
    unique_post_dedup$unique$cite_source = unique_post_dedup$unique$source
    unique_post_dedup$unique$cite_label = unique_post_dedup$unique$label
    
    return(unique_post_dedup)
  }
  
}


dedup_citations_add_manual <- function(raw_citations, additional_pairs) {
  
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
  
  dedup_results <- ASySD::dedup_citations_add_manual(raw_citations, merge_citations = TRUE, extra_merge_fields = "cite_string",additional_pairs = additional_pairs, show_unknown_tags = FALSE)

    unique_post_dedup <- dedup_results
    unique_post_dedup$cite_source = unique_post_dedup$source
    unique_post_dedup$cite_label = unique_post_dedup$label
    
    return(unique_post_dedup)
  
}





  

