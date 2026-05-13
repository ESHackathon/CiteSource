#' Deduplicate citations
#'
#' Deduplicates citation data. Duplicates are assumed to be published in the
#' same journal, so pre-prints vs. their published versions will not be merged.
#'
#' @export
#' @importFrom RecordLinkage compare.dedup jarowinkler
#' @importFrom igraph graph_from_data_frame components
#' @importFrom parallelly availableCores
#' @importFrom utf8 utf8_encode
#' @importFrom stats na.omit
#' @importFrom utils menu
#' @param raw_citations Citation dataframe with relevant columns
#' @param manual logical. If TRUE, return the full result list including
#'   potential pairs for manual review. Default is FALSE.
#' @param show_unknown_tags When a label, source, or other merged field is
#'   missing, show it as "unknown"? Default FALSE.
#' @return When `manual = FALSE`: a dataframe of unique citations. When
#'   `manual = TRUE`: a list with `$unique` (unique citations) and
#'   `$manual_dedup` (potential pairs for review).
#'
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds",
#'                                       package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations
#' dedup_results <- dedup_citations(examplecitations)
#'
#' # Return potential pairs for manual review
#' dedup_results_manual <- dedup_citations(examplecitations, manual = TRUE)
dedup_citations <- function(raw_citations, manual = FALSE, show_unknown_tags = FALSE) {

  # Map or coalesce alternate column names into the names ASySD expects
  targets <- c("journal", "number", "pages", "isbn", "record_id")
  sources <- c("source",  "issue",  "start_page", "issn", "ID")
  raw_citations <- add_cols(raw_citations, sources)

  for (i in seq_along(targets)) {
    if (targets[i] %in% names(raw_citations)) {
      raw_citations[[targets[i]]] <- dplyr::coalesce(raw_citations[[targets[i]]], raw_citations[[sources[i]]])
    } else {
      raw_citations[[targets[i]]] <- raw_citations[[sources[i]]]
    }
  }

  raw_citations <- add_cols(raw_citations, c("record_id", "cite_label", "cite_source", "cite_string"))
  raw_citations$source <- raw_citations$cite_source
  raw_citations$label  <- raw_citations$cite_label

  dedup_results <- asys_dedup_citations(
    raw_citations,
    merge_citations    = TRUE,
    extra_merge_fields = "cite_string",
    show_unknown_tags  = show_unknown_tags
  )

  if (!manual) {
    unique_post_dedup <- dedup_results$unique
    unique_post_dedup$cite_source <- unique_post_dedup$source
    unique_post_dedup$cite_label  <- unique_post_dedup$label
    unique_post_dedup <- dplyr::select(unique_post_dedup, -source, -label)
    return(unique_post_dedup)
  } else {
    dedup_results$unique$cite_source <- dedup_results$unique$source
    dedup_results$unique$cite_label  <- dedup_results$unique$label
    dedup_results$unique <- dplyr::select(dedup_results$unique, -source, -label)
    return(dedup_results)
  }
}


#' Add manually identified duplicate pairs to a deduplicated dataset
#'
#' @export
#' @param unique_citations Unique citations returned by [dedup_citations()]
#' @param additional_pairs Dataframe of manually confirmed duplicate pairs
#'   (a subset of the `$manual_dedup` output). If a `result` column is present,
#'   only rows where `result == "match"` are merged.
#' @return Updated unique citations dataframe with manual duplicates merged.
#'
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds",
#'                                       package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate and retrieve manual pairs
#' dedup_results <- dedup_citations(examplecitations, manual = TRUE)
#' # (user reviews dedup_results$manual_dedup and sets result == "match" for true dups)
#' # final <- dedup_citations_add_manual(dedup_results$unique, dedup_results$manual_dedup)
dedup_citations_add_manual <- function(unique_citations, additional_pairs) {

  unique_citations$source <- unique_citations$cite_source
  unique_citations$label  <- unique_citations$cite_label

  dedup_results <- asys_dedup_citations_add_manual(
    unique_citations,
    additional_pairs   = additional_pairs,
    extra_merge_fields = "cite_string"
  )

  dedup_results$cite_source <- dedup_results$source
  dedup_results$cite_label  <- dedup_results$label
  dplyr::select(dedup_results, -source, -label)
}


#' Add missing columns to a citations dataframe
#'
#' @param raw_citations Citation dataframe
#' @param cname Character vector of column names required in the dataframe
#' @return Dataframe with missing columns added as NA
#' @noRd
add_cols <- function(raw_citations, cname) {
  add <- cname[!cname %in% names(raw_citations)]
  if (length(add) != 0) raw_citations[add] <- NA
  raw_citations
}
