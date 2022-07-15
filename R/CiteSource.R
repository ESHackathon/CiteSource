#' CiteSource: A package to compare sources of citation records
#'
#' The CiteSource package supports evidence aggregation by helping with the
#' processing of results of various searches in different sources. It allows to 
#' deduplicate results while retaining meta-data on where those results were 
#' found and then enables users to compare the contribution of different sources
#' 
#' @docType package
#' @name CiteSource
NULL

# Use magrittr rather than base R pipe to be compatible with older R versions
# And data pronoun .data to avoid issues/warnings due to dplyr non-standard evaluation
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
NULL