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
#' @param tag_naming Specifies how tags should be renamed.
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
                            search_json = NA,
                            tag_naming = "best_guess") {
  # Need to import files separately to
  # check whether they contain unique databases
  ref_list <- lapply(files,
                     synthesisr::read_refs,
                     tag_naming = tag_naming)
  if (length(search_json) == 1) {
    search_json <- rep(search_json, length(files))
  } else if (length(search_json) != length(files)) {
    stop(paste0("Length of search_json must be 1 or equal to length of files"))
  }
  ref_list <- mapply(function(ref, file, json) {
    ref$cs__filename <- basename(file)
    return(ref)
  }, ref_list, files)
 if (databases == files) {
    check_unique_search_meta(files, ref_list)
  }
  ref_tibble <- do.call(dplyr::bind_rows, ref_list) %>% tibble::as_tibble()
  return(ref_tibble)
}