% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{export_csv}
\alias{export_csv}
\title{Export deduplicated citations with source data as CSV file}
\usage{
export_csv(
  unique_citations,
  filename = "citesource_exported_citations.csv",
  separate = NULL,
  trim_abstracts = 32000
)
}
\arguments{
\item{unique_citations}{Dataframe with unique citations, resulting from \code{dedup_citations()}}

\item{filename}{Name (and path) of file, should end in .csv}

\item{separate}{Character vector indicating which (if any) of cite_source, cite_string and cite_label should be split into separate columns to facilitate further analysis.}

\item{trim_abstracts}{Some databases may return full-text that is misidentified as an abstract. This inflates file size and may lead to issues with Excel,
which cannot deal with more than 32,000 characters per field. Therefore, the default is to trim very long abstracts to 32,000 characters. Set a lower number to reduce file size, or
NULL to retain abstracts as they are.}
}
\value{
The function saves the deduplicated citations as a CSV file to the specified location.
}
\description{
This function saves deduplicated citations as a CSV file for further analysis and/or reporting.
Metadata can be separated into one column per source, label or string, which facilitates analysis.
Note that \emph{existing files are overwritten without warning.}
}
\examples{
if (interactive()) {
  # Load example data from the package
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)
  dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
  export_csv(dedup_results, "cite_sources.csv", separate = "cite_source")
}
}
