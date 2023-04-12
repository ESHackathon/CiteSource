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
#' dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#' unique_citations <- dedup_results$unique
#'
#' # Count unique and non-unique citations
#' count_unique(unique_citations)

count_unique <- function(unique_data, include_references = FALSE) {
  out <- unique_data %>%
    dplyr::filter(!.data$cite_source == "") %>%
    dplyr::select(.data$duplicate_id, .data$cite_source, .data$cite_label, .data$cite_string, tidyselect::any_of("record_ids")) %>%
    tidyr::separate_rows(.data$cite_source, convert = TRUE) %>%
    tidyr::separate_rows(.data$cite_label, convert = TRUE) %>%
    tidyr::separate_rows(.data$cite_string, convert = TRUE) %>%
    dplyr::group_by(.data$duplicate_id) %>%
    dplyr::mutate(
      unique = ifelse(length(unique(.data$cite_source)) == 1, TRUE, FALSE),
      type = ifelse(.data$unique, "unique", "duplicated") %>% factor(levels = c("unique", "duplicated"))
    ) %>%
    dplyr::ungroup() %>%
    unique()

  if (include_references == TRUE) {
    out %>% dplyr::left_join(unique_data %>% dplyr::select(-dplyr::all_of(setdiff(intersect(names(.), names(out)), "duplicate_id"))), by = "duplicate_id")
  } else {
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
#'   dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#'   unique_citations <- dedup_results$unique
#'   compare_sources(unique_citations, comp_type = "sources")
#' }


compare_sources <- function(unique_data, comp_type = c("sources", "strings", "labels"), include_references = FALSE) {
  out <- list()

  if ("sources" %in% comp_type) {
    source_comparison <- unique_data %>%
      dplyr::select(.data$duplicate_id, .data$cite_source, tidyselect::any_of("record_ids")) %>%
      dplyr::filter(!cite_source == "") %>%
      tidyr::separate_rows(.data$cite_source, sep = ", ", convert = TRUE) %>%
      unique() %>%
      tidyr::pivot_wider(
        id_cols = .data$duplicate_id, names_prefix = "source__", names_from = .data$cite_source, values_from = .data$cite_source,
        values_fn = function(x) TRUE,
        values_fill = FALSE
      )

    out <- c(out, list(source_comparison))
  }

  if ("strings" %in% comp_type) {
    source_comparison <- unique_data %>%
      dplyr::select(.data$duplicate_id, .data$cite_string, tidyselect::any_of("record_ids")) %>%
      dplyr::filter(!.data$cite_string == "") %>%
      tidyr::separate_rows(.data$cite_string, sep = ", ", convert = TRUE) %>%
      unique() %>%
      tidyr::pivot_wider(
        id_cols = .data$duplicate_id, names_prefix = "string__", names_from = .data$cite_string, values_from = .data$cite_string,
        values_fn = function(x) TRUE,
        values_fill = FALSE
      )

    out <- c(out, list(source_comparison))
  }

  if ("labels" %in% comp_type) {
    source_comparison <- unique_data %>%
      dplyr::select(.data$duplicate_id, .data$cite_label, tidyselect::any_of("record_ids")) %>%
      dplyr::filter(!cite_label == "") %>%
      tidyr::separate_rows(.data$cite_label, sep = ", ", convert = TRUE) %>%
      unique() %>%
      tidyr::pivot_wider(
        id_cols = .data$duplicate_id, names_prefix = "label__", names_from = .data$cite_label,
        values_from = .data$cite_label,
        values_fn = function(x) TRUE,
        values_fill = FALSE
      )
    out <- c(out, list(source_comparison))


    if (any(stringr::str_detect(names(source_comparison), "[Ss]earch"))) {
      search_stage <- stringr::str_subset(names(source_comparison), "[Ss]earch")
      if (length(search_stage) == 1) {
        not_in_search <- sum(!source_comparison[[search_stage]])
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

  if (length(out) == 0) stop('comp_type must be one or more of "sources", "strings" or "labels"')

  out <- purrr::reduce(out, dplyr::left_join, by = "duplicate_id")

  if (include_references == TRUE) {
    out %>% dplyr::left_join(unique_data %>% dplyr::select(-dplyr::all_of(setdiff(intersect(names(.), names(out)), "duplicate_id"))), by = "duplicate_id")
  } else {
    out
  }
}

