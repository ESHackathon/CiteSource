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
#'   export_csv(dedup_results, "cite_sources.csv", separate = "cite_source")
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


#' Export data frame to RIS file
#'
#' This function saves a data frame as a RIS file with specified columns mapped to RIS fields. Note that
#' *existing files are overwritten without warning.*
#'
#' @param citations Dataframe to be exported to RIS file
#' @param filename Name (and path) of file, should end in .ris
#' @param user_mapping List. Custom mapping of RIS fields to data frame columns. If NULL, a default mapping is used.
#' @export
#' @examples
#' if (interactive()) {
#'   # Load example data from the package
#'   examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#'   examplecitations <- readRDS(examplecitations_path)
#'   dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#'   export_ris(dedup_results$unique, "cite_sources.ris", user_mapping = list("DB" = "cite_source_include", "C7" = "cite_label_include"))
#' }

# Define the export_ris function
export_ris <- function(citations, filename = "output.ris", user_mapping = NULL) {
  
  # Default mapping of RIS fields to citations columns
  default_mapping <- list(
    "DB" = "cite_source_include",
    "C7" = "cite_label_include",
    "C8" = "cite_string_include",
    "C1" = "duplicate_id",
    "C2" = "record_ids"
  )
  
  # If user_mapping is provided, override the default mapping
  if (!is.null(user_mapping)) {
    for (field in names(user_mapping)) {
      default_mapping[[field]] <- user_mapping[[field]]
    }
  }
  
  # Rename the citations columns according to the final mapping
  for (field in names(default_mapping)) {
    if (default_mapping[[field]] %in% names(citations)) {
      citations <- citations %>% dplyr::rename(!!field := .data[[default_mapping[[field]]]])
    }
  }
  
  # Write the RIS file
  write_refs(as.data.frame(citations), file = filename)
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
