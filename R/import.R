# Use magrittr rather than base R pipe to be compatible with older R versions?
#' @importFrom magrittr `%>%`
NULL
#' Check whether each file contains results from unique database
#'
#' If filenames are to be used to name databases, users should be aware if
#' one file contains results from more than one database.
#'
#' @param files List of file names
#' @param ref_list List of references
#'
#' @keywords internal
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
#' @param search_json Optional JSON containing search history
#' information in line with the search history standard.
#' If a vector is provided, it must be the same length as the list of files.
#' @param search_json_field Optional field code / name that contains the
#' JSON metadata in line with the search history standard. If specified
#' search_json is ignored.
#' @param tag_naming passes this directly to synthesisr::read_refs
#' Passed to and documented in \code{\link[synthesisr]{read_ref}}
#' @return A tibble with one row per citation
#' @examples
#' \dontrun{
#'  read_citations(c("DB1.ris", "DB1.bib"))
#'  read_citations(c("res.ris", "res.bib"),
#'  database = c("CINAHL", "MEDLINE"),
#'  plaform = c("WOS", "EBSCO"),
#'  search_ids = c("Search1", "Search2"))
#'  }
#' @export
read_citations <- function(files,
                            database,
                            platform = NA,
                            search_ids = NA,
                            tag_naming = "best_guess") {
  if (length(files) != length(database)) {
    stop("Files and databases must be of equal length")
  }
  if (!is.na(platform)) {
    if (length(database) != length(platform)) {
      stop("databases and platforms must be of equal length")
    }
  }
  if (!is.na(search_ids)) {
    if (length(database) != length(search_ids)) {
      stop("databases and search_ids must be of equal length")
    }
  }
  # Need to import files separately to add database, platform, and searches
  ref_list <- lapply(files,
                     synthesisr::read_refs,
                     tag_naming = tag_naming)
  for (index in seq_len(length(files))) {
    ref_list[[index]]$database <- database[[index]]
    if (!is.na(platform)) {
      ref_list[[index]]$platform <- platform[[index]]
    }
    if (!is.na(search_ids)) {
      ref_list[[index]]$search_id <- search_ids[[index]]
    }
  }
  dpylr::bind_rows(ref_list)
  return(ref_list)
}