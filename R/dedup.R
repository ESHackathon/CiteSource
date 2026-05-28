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
#'   `manual = TRUE`: a list with `$unique` (unique citations),
#'   `$manual_dedup` (potential pairs for review), and `$auto_pairs`
#'   (pairs that were merged automatically — feed to [dedup_log()] together
#'   with confirmed manual pairs to build a full provenance log).
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


#' Build a provenance log of all merged duplicate pairs
#'
#' Combines automatically merged pairs and user-confirmed manual pairs into a
#' single tibble with a `method` column (`"auto"` / `"manual"`). Useful for
#' reporting and auditing — e.g. as supplementary material for a systematic
#' review.
#'
#' @export
#' @param dedup_result List returned by `dedup_citations(manual = TRUE)`
#'   (must contain `$auto_pairs`; optionally `$manual_dedup`).
#' @param confirmed_manual_pairs Optional dataframe of manual pairs the user
#'   confirmed as duplicates. Typically a subset of `dedup_result$manual_dedup`.
#'   If a `result` column is present, only rows where `result == "match"` are
#'   included.
#' @return Tibble with columns `method`, `record_id1`, `record_id2`, and the
#'   common bibliographic fields (`title1/2`, `author1/2`, `year1/2`,
#'   `journal1/2`, `doi1/2`) when available.
#'
#' @examples
#' examplecitations_path <- system.file("extdata", "examplecitations.rds",
#'                                       package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#' dedup_results <- dedup_citations(examplecitations, manual = TRUE)
#' # Log of just the auto-merged pairs
#' dedup_log(dedup_results)
#' # Or include user-confirmed manual pairs
#' # dedup_log(dedup_results, confirmed_manual_pairs = my_confirmed_pairs)
dedup_log <- function(dedup_result, confirmed_manual_pairs = NULL) {

  log_cols <- c("record_id1", "record_id2",
                "title1", "title2", "author1", "author2",
                "year1", "year2", "journal1", "journal2",
                "doi1", "doi2")

  pad_cols <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      out <- stats::setNames(
        as.data.frame(matrix(NA_character_, nrow = 0, ncol = length(log_cols)),
                      stringsAsFactors = FALSE),
        log_cols
      )
      return(out)
    }
    missing <- setdiff(log_cols, names(df))
    if (length(missing) > 0) df[missing] <- NA
    df <- df[, log_cols, drop = FALSE]
    # Coerce to character so auto-pairs (numeric year, etc.) and manual-pairs
    # (character) can be bound together without type clashes.
    for (col in log_cols) df[[col]] <- as.character(df[[col]])
    df
  }

  auto <- pad_cols(dedup_result$auto_pairs)
  if (nrow(auto) > 0) auto$method <- "auto"

  manual <- confirmed_manual_pairs
  if (!is.null(manual) && "result" %in% names(manual)) {
    manual <- manual[manual$result == "match", , drop = FALSE]
  }
  if (!is.null(manual) && "record_id1" %in% names(manual)) {
    manual <- pad_cols(manual)
  } else {
    manual <- pad_cols(NULL)
  }
  if (nrow(manual) > 0) manual$method <- "manual"

  out <- dplyr::bind_rows(auto, manual)
  if (nrow(out) > 0) out <- out[, c("method", log_cols), drop = FALSE]
  tibble::as_tibble(out)
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
