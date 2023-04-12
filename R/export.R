#' Export deduplicated citations with source data as csv file
#'
#' This function saves deduplicated citations as a csv file for further analysis and/or reporting.
#' Metadata can be separated into one column per source, label or string, which facilitates analysis.
#' Note that *existing files are overwritten without warning.*
#'
#' @param citations Dataframe with unique citations, resulting from `dedup_citations()`
#' @param filename Name (and path) of file, should end in .csv
#' @param separate Character vector indicating which (if any) of cite_source, cite_string and cite_label should be split into separate columns to facilitate further analysis.
#' @param trim_abstracts Some databases may return full-text that is misidentified as an abstract. This inflates file size and may lead to issues with Excel, 
#' which cannot deal with more than 32,000 characters per field. Therefore, the default is to trim very long abstracts to 32,000 characters. Set a lower number to reduce file size, or
#' NULL to retain abstracts as they are.
#' @export
#' @examples
#' if (interactive()) {
#'   # Load example data from the package
#'   examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#'   examplecitations <- readRDS(examplecitations_path)
#'   dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#'   export_csv(dedup_results$unique, "cite_sources.csv", separate = "cite_source")
#' }



export_csv <- function(citations, filename = "citations.csv", separate = NULL, trim_abstracts = 32000) {
  if (tolower(tools::file_ext(filename)) != "csv") warning("Function saves a CSV file, so filename should (usually) end in .csv. For now, name is used as provided.")

  if (!is.null(separate)) {
    separate <- match.arg(separate, choices = c("cite_source", "cite_label", "cite_string"), several.ok = TRUE)

    separated <- purrr::map_dfc(separate, function(x) {
      citations %>%
        dplyr::select(tidyselect::all_of(x), .data$duplicate_id, .data$record_ids) %>%
        tidyr::separate_rows(1, sep = ", ", convert = TRUE) %>%
        unique() %>%
        tidyr::pivot_wider(
          id_cols = .data$duplicate_id, names_prefix = paste0(stringr::str_remove(x, "cite_"), "_"),
          names_from = 1, values_from = c(.data$record_ids),
          values_fn = function(x) TRUE,
          values_fill = FALSE
        ) %>%
        dplyr::select(tidyselect::starts_with(paste0(stringr::str_remove(x, "cite_"))))
    })
    
    if (!is.null(trim_abstracts)) {
      citations <- citations %>% dplyr::mutate(abstract = stringr::str_sub(.data$abstract, 1, trim_abstracts))
    }

    citations <- citations %>%
      dplyr::select(-tidyselect::all_of(separate)) %>%
      dplyr::bind_cols(separated)
  }
  utils::write.csv(citations, filename, row.names = FALSE)
}


#' Export deduplicated citations to RIS file
#'
#' This function saves deduplicated citations as a RIS file with sources, labels and strings
#' included as custom fields (if they were initially provided for any of the citations). Note that
#' *existing files are overwritten without warning.*
#'
#' @param citations Dataframe with unique citations, resulting from `dedup_citations()`
#' @param filename Name (and path) of file, should end in .ris
#' @param source_field Character. Which RIS field should cite_sources be saved to? NULL to exclude.
#' @param label_field Character. Which RIS field should cite_labels be saved to? NULL to exclude.
#' @param string_field Character. Which RIS field should cite_strings be saved to? NULL to exclude.
#' @export
#' @examples
#' if (interactive()) {
#'   # Load example data from the package
#'   examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#'   examplecitations <- readRDS(examplecitations_path)
#'   dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#'   export_ris(dedup_results$unique, "cite_sources.ris", string_field = NULL)
#' }

export_ris <- function(citations, filename = "citations.ris", source_field = "DB", label_field = "C7", string_field = "C8") {
  if (tolower(tools::file_ext(filename)) != "ris") warning("Function saves a RIS file, so filename should (usually) end in .ris. For now, name is used as provided.")

  if (!is.null(source_field)) {
    citations <- citations %>% dplyr::rename(cite_source_include = .data$cite_source)
  }

  if (!is.null(label_field)) {
    citations <- citations %>% dplyr::rename(cite_label_include = .data$cite_label)
  }

  if (!is.null(string_field)) {
    citations <- citations %>% dplyr::rename(cite_string_include = .data$cite_string)
  }

  # Move source_type to the front - should be there unless there was an import issue
  # but these are common - see https://github.com/mjwestgate/synthesisr/issues/24
  citations <- citations %>%
    dplyr::select("source_type", "duplicate_id", dplyr::everything(), -tidyselect::any_of(c("cite_source", "cite_string", "cite_label", "record_id")))

  synthesisr_codes <- dplyr::bind_rows(
    tibble::tribble(
      ~code, ~field, ~ris_synthesisr,
      source_field, "cite_source_include", TRUE,
      string_field, "cite_string_include", TRUE,
      label_field, "cite_label_include", TRUE,
      "C1", "duplicate_id", TRUE,
      "C2", "record_ids", TRUE
    ),
    synthesisr_code_lookup %>% dplyr::filter(.data$ris_synthesisr)
  ) %>% dplyr::distinct(.data$code, .keep_all = TRUE) # Remove fields from synthesisr specification used for CiteSource metadata

  # Currently, write_refs does not accept tibbles, thus converted
  write_refs(as.data.frame(citations), file = filename, tag_naming = synthesisr_codes)
}


#' Export deduplicated citations to .bib file
#'
#' This function saves deduplicated citations as a BibTex file with sources, labels and strings
#' included in the `note` field (if they were initially provided for any of the citations). Therefore,
#' beware that **any `note` field that might be included in `citations` will be overwritten**. Also note that
#' *existing files are overwritten without warning.*
#'
#' @param citations Dataframe with unique citations, resulting from `dedup_citations()`
#' @param filename Name (and path) of file, should end in .ris
#' @param include Character. One or more of sources, labels or strings
#' @export
#' @examples
#' if (interactive()) {
#'   # Load example data from the package
#'   examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#'   examplecitations <- readRDS(examplecitations_path)
#'   dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#'   export_bib(dedup_results$unique, "cite_sources.bib", include = "sources")
#' }

export_bib <- function(citations, filename = "citations.bib", include = c("sources", "labels", "strings")) {
  if (tolower(tools::file_ext(filename)) != "bib") warning("Function saves a BibTex file, so filename should (usually) end in .bib. For now, name is used as provided.")

  include <- stringr::str_remove(include, "s$") %>% paste0("cite_", .)

  notes <- citations %>% dplyr::select(tidyselect::all_of(include))

  for (i in seq_along(include)) {
    notes[include[i]] <- paste(include[i], notes[[include[i]]], sep = ": ")
  }

  notes <- notes %>%
    tidyr::unite("note", dplyr::everything(), sep = "; ") %>%
    dplyr::pull(.data$note)

  citations["note"] <- notes

  citations <- citations %>%
    dplyr::select(-dplyr::starts_with("cite_"), -tidyselect::any_of(c("duplicate_id", "record_ids", "record_id")))

  write_refs(as.data.frame(citations), format = "bib", file = filename)
}
