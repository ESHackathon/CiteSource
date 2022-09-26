#' Check whether each file contains results from unique database
#'
#' If filenames are to be used to name databases, users should be aware if
#' one file contains results from more than one database.
#'
#' @param files List of file names
#' @param ref_list List of references
#'
#' @keywords internal
#' 

# TODO: decide whether to use or remove this function

check_unique_search_meta <- function(files, ref_list) {
  #Iterate over ref list, check whether contents in a field are unique
  #If not, warn the user
  if (FALSE) {
    warning(paste("Beware: ", files[i], "contains multiple values in field",
            ref_field, ". However, they will all be labeled as coming from the
            same database"))
  }
}



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
#' @inheritParams synthesisr::read_refs
#' @return A tibble with one row per citation
#' @examples
#' if (interactive()) {
#'  read_citations(c("res.ris", "res.bib"),
#'  cite_sources = c("CINAHL", "MEDLINE"),
#'  cite_strings = c("Search1", "Search2"),
#'  cite_labels = c("raw", "screened"))
#'  }
#' @export
#' 

# TODO: allow user to specify citation fields to serve as cite_sources, cite_strings or cite_labels

# TODO_LATER - revisit idea of search detail JSONs - could be
# @param search_json Optional JSON containing search history
# information in line with the search history standard.
# If a vector is provided, it must be the same length as the list of files.
# @param search_json_field Optional field code / name that contains the
# JSON metadata in line with the search history standard. If specified
# search_json is ignored.
 
read_citations <- function(files,
                           cite_sources = NULL,
                           cite_strings = NULL,
                           cite_labels = NULL,
                           tag_naming = "best_guess"
                           ) {
  if (!is.null(utils::packageDescription("synthesisr")$Repository) && utils::packageDescription("synthesisr")$Repository == "CRAN" && !utils::packageVersion("synthesisr") > "0.3.0") {
    rlang::warn("NB: There is a bug in synthesisr 0.3.0 on CRAN that can lead to issues here. Best update to Github dev version or a newer version.",   .frequency = "once", .frequency_id = "synthesisr-version")
  }
  
 
  if (is.null(cite_sources)) {
    cite_sources <- purrr::map_chr(files, ~tools::file_path_sans_ext(basename(.x)))
    
    if(any(duplicated(cite_sources))) {
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
  
  if (any(stringr::str_detect(c(cite_sources, cite_labels, cite_strings), ","))) {
    stop("',' must not be used in cite_source, cite_labels or cite_strings (or filenames if these are not specified)")
  }
  
  # Need to import files separately to add origin, platform, and searches
  ref_list <- lapply(files,
                     synthesisr::read_refs,
                     tag_naming = tag_naming)
  
  
  #Drop empty citations
  ref_list <- lapply(ref_list,
                     function(data) {data[rowSums(is.na(data)) != (ncol(data)-1),]})
  
  for (i in seq_along(files)) {
    message(basename(files[i]), ": read ", nrow(ref_list[[i]]), " citations.")
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
 ref_list <- ref_list %>%
   purrr::map(tibble::as_tibble) %>%
   purrr::reduce(dplyr::bind_rows)
  return(ref_list)
}
