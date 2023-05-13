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

dedup_citations <- function(raw_citations) {
  
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
  
  dedup_results <- ASySD::dedup_citations(raw_citations, merge_citations = TRUE, extra_merge_fields = "cite_string")
  
  unique_post_dedup <- dedup_results$unique
  unique_post_dedup$cite_source = unique_post_dedup$source
  unique_post_dedup$cite_label = unique_post_dedup$label
  
  return(unique_post_dedup)
}
