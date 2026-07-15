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
#'   (pairs that were merged automatically - feed to [dedup_log()] together
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
    unique_post_dedup <- resolve_type_column(unique_post_dedup)
    return(unique_post_dedup)
  } else {
    dedup_results$unique$cite_source <- dedup_results$unique$source
    dedup_results$unique$cite_label  <- dedup_results$unique$label
    dedup_results$unique <- dplyr::select(dedup_results$unique, -source, -label)
    dedup_results$unique <- resolve_type_column(dedup_results$unique)
    return(dedup_results)
  }
}


#' Resolve merged document types to a single value per record
#'
#' Applies the auto-deduplication rule: if every merged record in a cluster
#' shares the same document type (compared case-insensitively and trimmed) that
#' type is kept; otherwise the generic RIS type `"GEN"` is used. `merge_metadata()`
#' joins the individual types with ", ", so this operates on that joined string.
#' @noRd
resolve_type <- function(x) {
  vapply(x, function(t) {
    if (is.na(t)) return(NA_character_)
    parts <- trimws(unlist(strsplit(as.character(t), ",")))
    parts <- parts[!is.na(parts) & parts != "" & toupper(parts) != "NA"]
    if (length(parts) == 0) return(NA_character_)
    if (length(unique(toupper(parts))) == 1) return(parts[[1]])
    "GEN"
  }, character(1), USE.NAMES = FALSE)
}

#' Apply `resolve_type()` to the `type` column of a citations dataframe (if any)
#' @noRd
resolve_type_column <- function(citations) {
  if ("type" %in% names(citations)) citations$type <- resolve_type(citations$type)
  citations
}


#' Build a provenance log of all merged duplicate pairs
#'
#' Combines automatically merged pairs and user-confirmed manual pairs into a
#' single tibble with a `method` column (`"auto"` / `"manual"`). Useful for
#' reporting and auditing - e.g. as supplementary material for a systematic
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

  # Honor a user-provided `type_keep` on confirmed pairs: set the document type
  # of the involved records so that merging yields the chosen value. Pairs
  # without a `type_keep` fall back to the "shared type, else GEN" rule applied
  # by resolve_type_column() below.
  if ("type_keep" %in% names(additional_pairs) && "type" %in% names(unique_citations) &&
      all(c("duplicate_id.x", "duplicate_id.y") %in% names(additional_pairs))) {
    ap <- additional_pairs
    if ("result" %in% names(ap)) ap <- ap[ap$result == "match", , drop = FALSE]
    dup_ids <- as.character(unique_citations$duplicate_id)
    for (i in seq_len(nrow(ap))) {
      tk <- ap$type_keep[i]
      if (!is.na(tk) && nzchar(tk)) {
        ids <- c(as.character(ap$duplicate_id.x[i]), as.character(ap$duplicate_id.y[i]))
        unique_citations$type[dup_ids %in% ids] <- tk
      }
    }
  }

  dedup_results <- asys_dedup_citations_add_manual(
    unique_citations,
    additional_pairs   = additional_pairs,
    extra_merge_fields = "cite_string"
  )

  dedup_results$cite_source <- dedup_results$source
  dedup_results$cite_label  <- dedup_results$label
  dedup_results <- dplyr::select(dedup_results, -source, -label)
  resolve_type_column(dedup_results)
}


#' Add new citations to a previously deduplicated set and re-deduplicate
#'
#' Adds further citations (e.g. an additional database search) to a set that was
#' already deduplicated, and deduplicates the new records against both the
#' existing set and each other - without discarding the work already done. Each
#' existing unique record enters as a single row, so prior automatic and manual
#' merge decisions are preserved; the new records are integrated and full
#' provenance (the original `record_ids` behind every merged record) is carried
#' through.
#'
#' This is the incremental counterpart to running [dedup_citations()] on all
#' sources from scratch and, for the same data, produces the same unique set.
#'
#' @export
#' @param existing_citations A previously deduplicated set (from
#'   [dedup_citations()], [reimport_csv()] or [reimport_ris()]) - must contain a
#'   `duplicate_id` column.
#' @param new_citations New raw citations to add, as returned by
#'   [read_citations()] (with `cite_source` / `cite_label` / `cite_string`).
#' @param manual logical. If TRUE, return the full result list including
#'   `$manual_dedup` candidate pairs for review (see [dedup_citations()]).
#'   Default FALSE.
#' @param show_unknown_tags When a label, source, or other merged field is
#'   missing, show it as "unknown"? Default FALSE.
#' @return When `manual = FALSE`: a dataframe of unique citations across both
#'   sets. When `manual = TRUE`: a list with `$unique`, `$manual_dedup` and
#'   `$auto_pairs` (as in [dedup_citations()]). In both cases `record_ids`
#'   retains the original record IDs behind every merged record.
#' @seealso [dedup_citations()], [dedup_citations_add_manual()]
#'
#' @examples
#' if (interactive()) {
#'   existing <- dedup_citations(read_citations(old_files, cite_sources = old_srcs))
#'   new_raw  <- read_citations(new_files, cite_sources = new_srcs)
#'   combined <- dedup_citations_add_sources(existing, new_raw)
#' }
dedup_citations_add_sources <- function(existing_citations, new_citations,
                                        manual = FALSE, show_unknown_tags = FALSE) {

  if (!"duplicate_id" %in% names(existing_citations)) {
    stop("existing_citations must contain a `duplicate_id` column - pass a set ",
         "returned by dedup_citations(), reimport_csv() or reimport_ris().")
  }

  # Work in character throughout (the dedup engine's canonical type) so the two
  # frames bind without column-type clashes.
  ex <- dplyr::mutate(existing_citations, dplyr::across(dplyr::everything(), as.character))
  if (!"record_ids" %in% names(ex)) ex$record_ids <- ex$duplicate_id

  # Provenance lookup: existing duplicate_id -> its underlying original record_ids
  prov <- stats::setNames(as.character(ex$record_ids), as.character(ex$duplicate_id))

  # Each existing unique record enters as one input keyed by its duplicate_id
  ex$record_id <- as.character(ex$duplicate_id)

  # New records get fresh ids that cannot collide with any existing id. Base the
  # offset on the max of ALL underlying record_ids (duplicate_id is the cluster
  # minimum, so a new id keyed off it could otherwise reuse an existing id).
  existing_ids <- c(
    as.character(ex$duplicate_id),
    unlist(strsplit(as.character(ex$record_ids), ",\\s*"))
  )
  max_id <- suppressWarnings(max(as.numeric(existing_ids), na.rm = TRUE))

  nw <- dplyr::mutate(new_citations, dplyr::across(dplyr::everything(), as.character))
  nw <- dplyr::select(nw, -dplyr::any_of(c("duplicate_id", "record_ids", "record_id")))
  nw$record_id <- if (is.finite(max_id)) {
    as.character(max_id + seq_len(nrow(nw)))
  } else {
    paste0("new_", seq_len(nrow(nw)))
  }

  # Drop the merged-set metadata that would otherwise trigger a record_id clash
  # (format_rerun renames duplicate_id -> record_id) or be stale after re-dedup.
  ex <- dplyr::select(ex, -dplyr::any_of(c("duplicate_id", "record_ids", "manual_dedup_complete")))

  combined <- dplyr::bind_rows(ex, nw)

  result <- dedup_citations(combined, manual = manual, show_unknown_tags = show_unknown_tags)

  # Restore original provenance: expand existing-duplicate-id tokens in the
  # rebuilt record_ids back to their underlying original record IDs.
  unique_out <- if (manual) result$unique else result
  unique_out$record_ids <- vapply(unique_out$record_ids, function(rids) {
    toks <- trimws(strsplit(rids, ",\\s*")[[1]])
    toks <- ifelse(toks %in% names(prov), prov[toks], toks)
    toks <- unlist(strsplit(paste(toks, collapse = ", "), ",\\s*"))
    paste(unique(trimws(toks[toks != ""])), collapse = ", ")
  }, character(1), USE.NAMES = FALSE)

  if (manual) {
    result$unique <- unique_out
    result
  } else {
    unique_out
  }
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
