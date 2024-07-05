#' Import citations from file
#'
#' This function imports RIS and Bibtex files with citations and merges them
#' into one long tibble with one record per line.
#'
#' @param files One or multiple RIS or Bibtex files with citations.
#' Should be .bib or .ris files
#' @param cite_sources The origin of the citation files (e.g. "Scopus", "WOS", "Medline") - vector with one value per file, defaults to file names.
#' @param cite_strings Optional. The search string used (or another grouping to analyse) - vector with one value per file
#' @param cite_labels Optional. An additional label per file, for instance the stage of search - vector with one value per file
#' @param metadata A tibble with file names and metadata for each file. Can be specified as an *alternative* to files, cite_sources, cite_strings and cite_labels. 
#' @param verbose Should number of reference and allocation of labels be reported?
#' @param only_key_fields Should only key fields (e.g., those used by CiteCourse) be imported? If FALSE, all RIS data is retained. Can also be a character vector of field names to retain (after they have been renamed by the import function) in addition to the essential ones.
#' @inheritParams synthesisr_read_refs
#' @return A tibble with one row per citation
#' @examples
#' if (interactive()) {
#'   # Import only key fields from the RIS files
#'   read_citations(c("res.ris", "res.bib"),
#'     cite_sources = c("CINAHL", "MEDLINE"),
#'     cite_strings = c("Search1", "Search2"),
#'     cite_labels = c("raw", "screened"),
#'     only_key_fields = TRUE
#'   )
#'
#'   # or equivalently
#'   metadata_tbl_key_fields <- tibble::tribble(
#'    ~files,     ~cite_sources, ~cite_strings, ~cite_labels, ~only_key_fields,
#'   "res.ris",  "CINAHL",      "Search1",     "raw",        TRUE,
#'   "res.bib",  "MEDLINE",     "Search2",     "screened",   TRUE
#'   )
#'
#'   read_citations(metadata = metadata_tbl_key_fields)
#' }
#' @export

read_citations <- function(files = NULL,
                           cite_sources = NULL,
                           cite_strings = NULL,
                           cite_labels = NULL,
                           metadata = NULL,
                           verbose = TRUE,
                           tag_naming = "best_guess",
                           only_key_fields = TRUE) {
  
  if (is.character(only_key_fields)) {
    only_key_fields <- union(key_fields, only_key_fields)
  } else if (only_key_fields == TRUE) {
    only_key_fields <- key_fields
  } else if (!only_key_fields == FALSE) {
    stop("only_key_fields must be TRUE, FALSE or a character vector")
  } else {
    only_key_fields <- NULL
  }

  if (is.null(files) && is.null(metadata)) stop("Either files or metadata must be specified.")
  if (!is.null(files) && !is.null(metadata)) stop("files and metadata cannot both be specified.")
  
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata)) stop("metadata must be a tibble/dataframe.")
    if (!("files" %in% colnames(metadata))) stop("metadata must contain at least a `files` column")
    files <- metadata[["files"]]
    cite_sources <- metadata[["cite_sources"]]
    cite_strings <- metadata[["cite_strings"]]
    cite_labels <- metadata[["cite_labels"]]
  }
  
  
  if (is.null(cite_sources)) {
    cite_sources <- purrr::map_chr(files, ~ tools::file_path_sans_ext(basename(.x)))

    if (any(duplicated(cite_sources))) {
      cite_sources <- make.unique(cite_sources, sep = "_")
      message("Some file names were duplicated. Therefore, their cite_source values are distinguished by suffixes (_1 etc). For greater clarity, specify cite_sources explicitly or rename files.")
    }
  }

  if (length(files) != length(cite_sources)) {
    stop("Files and origins cite_sources be of equal length")
  }
  if (!is.null(cite_strings)) {
    if (length(cite_sources) != length(cite_strings)) {
      stop("Cite_sources and cite_strings must be of equal length")
    }
  }
  if (!is.null(cite_labels)) {
    if (length(cite_sources) != length(cite_labels)) {
      stop("Cite_sources and cite_labels must be of equal length")
    }
  }

  contains_commas <- any(stringr::str_detect(c(cite_sources, cite_labels, cite_strings), ","))

  if (!is.na(contains_commas) && contains_commas) {
    stop("',' must not be used in cite_source, cite_labels or cite_strings (or filenames if these are not specified)")
  }

  # Need to import files separately to add origin, platform, and searches
  ref_list <- purrr::map(files,
                         \(x) synthesisr_read_refs(x,  tag_naming = tag_naming, select_fields = only_key_fields),
                         .progress = list(  total = 100, 
                                            format = "Importing files {cli::pb_bar} {cli::pb_percent}")
  )

  # Drop empty citations
  ref_list <- lapply(
    ref_list,
    function(data) data[rowSums(is.na(data)) != (ncol(data) - 1), ]
  )

  ref_counts <- numeric(length(files))

  for (i in seq_along(files)) {
    ref_counts[i] <- nrow(ref_list[[i]])
  }

  for (index in seq_len(length(files))) {
    ref_list[[index]]$cite_source <- cite_sources[[index]]
    if (!is.null(cite_strings)) {
      ref_list[[index]]$cite_string <- cite_strings[[index]]
    }
    if (!is.null(cite_labels)) {
      ref_list[[index]]$cite_label <- cite_labels[[index]]
    }
  }

  if (verbose) {
    report <- data.frame(
      file = basename(files),
      cite_source = cite_sources,
      cite_string = if (is.null(cite_strings)) NA_character_ else cite_strings,
      cite_label = if (is.null(cite_labels)) NA_character_ else cite_labels,
      citations = ref_counts
    )

    message("Import completed - with the following details:")
    message(paste0(utils::capture.output(report), collapse = "\n"))
  }

  refs <- ref_list %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows)
  
  missing_fields <- setdiff(key_fields, colnames(refs))
  refs[missing_fields] <- NA

  refs
  
}


