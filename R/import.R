# Use magrittr rather than base R pipe to be compatible with older R versions?
#' @importFrom magrittr `%>%` 

#' Import citations from file
#' 
#' This function imports RIS and Bibtex files with citations and merges them 
#' into one long tibble with one record per line.
#'
#' @param files One or multiple RIS or Bibtex files with citations. Should be .bib or .ris files
#' @param databases An optional vector of database names, in order of `files`.
#' Defaults to filenames..
#' @param search_ids An optional vector of search IDs, in order of `files`.
#' Defaults to empty.
#' @param tag_naming Specifies how tags should be renamed. 
#' Passed to and documented in \code{\link[synthesisr]{read_ref}}
#' @return A tibble with one row per citation
#' @examples
#' \dontrun{
#'  read_citations(c("DB1.ris", "DB1.bib"))
#'  read_citations(c("res.ris", "res.bib"), c("EBSCO", "WOS"), c("Search1", "Search2"))
#'  }   
#' @export

read_citations <- function(files, databases = files, search_ids = "", tag_naming = "best_guess") {
  #?? Should default databases be filenames without paths, or filenames as provided?
  
  #Need to import files separately to check whether they contain unique databases
  ref_list <- lapply(files,
                     synthesisr::read_refs,
                     filename = files, tag_naming = tag_naming)
  
  if (databases = files) {
    check_unique_dbs(files, ref_list)
  }
  
  ref_list <- mapply(function(ref, database, search_id) {
    ref$cs__db <- database
    ref$cs__search_id <- search_id
  }, ref_list, databases, search_ids)
  
  ref_tibble <- do.call(rbind, ref_list) %>% tibble::as_tibble()
  
}

#' Check whether each file contains results from unique database
#' 
#' If filenames are to be used to name databases, users should be aware if
#' one file contains results from more than one database. 
#' 
#' @param files List of file names
#' @param 
#' 
#' @internal

check_unique_dbs <- function(files, ref_list) {
  #Iterate over ref list, check whether contains a database field 
  #and whether value is unique
  #If so, warn 
  if (FALSE) {
    warning(paste("Beware: ", files[i], "contains multiple values in field",
            ref_field, ". However, they will all be labeled as coming from the
            same database")
            }
}