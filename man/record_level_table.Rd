% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{record_level_table}
\alias{record_level_table}
\title{Record-level table}
\usage{
record_level_table(
  citations,
  include = "sources",
  include_empty = TRUE,
  return = c("tibble", "DT"),
  indicator_presence = NULL,
  indicator_absence = NULL
)
}
\arguments{
\item{citations}{A deduplicated tibble as returned by \code{dedup_citations()}.}

\item{include}{Which metadata should be included in the table? Defaults to 'sources', can be replaced or expanded with 'labels' and/or 'strings'}

\item{include_empty}{Should records with empty metadata (e.g., no information on 'sources') be included in the table? Defaults to FALSE.}

\item{return}{Either a \code{tibble} that can be exported, e.g. as a csv, or a DataTable (\code{DT}) that allows for interactive exploration. Note that the DataTable allows
users to download a .csv file; in that file, presence and absence is always indicated as TRUE and FALSE to prevent issues with character encodings.}

\item{indicator_presence}{How should it be indicated that a value is present in a source/label/string? Defaults to TRUE in tibbles and a tickmark in DT tables}

\item{indicator_absence}{How should it be indicated that a value is \emph{not} present in a source/label/string? Defaults to FALSE in tibbles and a cross in DT tables}
}
\value{
A tibble or DataTable containing the per-record table that shows which sources (and/or labels/strings) each item was found in.
}
\description{
Creates a per-record table that shows which sources (and/or labels/strings) each item was found in.
}
\examples{
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)

# Deduplicate citations and compare sources
unique_citations <- dedup_citations(examplecitations)

unique_citations |> 
dplyr::filter(stringr::str_detect(cite_label, "final"))  |> 
record_level_table(return = "DT")
}
