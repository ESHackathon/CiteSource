#' Deduplicate citations - ASySD wrapper
#'
#' This function deduplicates citation data. Note that duplicates are assumed to published
#' in the same journal, so pre-prints and similar results will not be identified here.
#'
#' @export
#' @param raw_citations Citation dataframe with relevant columns
#' @return unique citations formatted for CiteSource

dedup_citations <- function(raw_citations) {
  
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
  
  raw_citations <- add_cols(raw_citations, c("record_id", "cite_label", "cite_source", "cite_string"))
  raw_citations$record_id <- raw_citations$record_id <- ""
  raw_citations$journal <- raw_citations$source
  raw_citations$number <- raw_citations$issue
  raw_citations$pages <- raw_citations$start_page
  raw_citations$isbn <- raw_citations$issn
  raw_citations$source <- raw_citations$cite_source
  raw_citations$label <- raw_citations$cite_label
  
  dedup_results <- ASySD::dedup_citations(raw_citations, merge_citations = TRUE)
  
  unique_post_dedup <- dedup_results$unique
  unique_post_dedup$cite_source = unique_post_dedup$source
  unique_post_dedup$cite_label = unique_post_dedup$label
  
  return(unique_post_dedup)
}