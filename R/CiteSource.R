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
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
#' @importFrom rlang .data :=
NULL

# Declare . as global variable to remove warnings
utils::globalVariables(".")

# Set Shiny upload size depending on interactive use or deployment on shinyapps.io
.onLoad <- function(libname, pkgname) {
    if (interactive() ) { 
        options(shiny.maxRequestSize=2000*1024^2, timeout = 40000000) 
    } else {
        options(shiny.maxRequestSize=250*1024^2, timeout = 40000000) 
        }
}
