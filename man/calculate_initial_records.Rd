% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_count_and_table.R
\name{calculate_initial_records}
\alias{calculate_initial_records}
\title{Calculate Initial Records Unique Citations}
\usage{
calculate_initial_records(unique_citations, labels_to_include = NULL)
}
\arguments{
\item{unique_citations}{A data frame containing the unique citations.
It must contain the columns \code{cite_source}, \code{cite_label}, and \code{duplicate_id}.}

\item{labels_to_include}{An optional character vector of labels to filter the citations.
If provided, only citations matching these labels will be included in the counts.
Default is NULL, meaning no filtering will be applied.}
}
\value{
A data frame containing the counts of \verb{Records Imported} and \verb{Distinct Records}
for each citation source. The data frame also includes a "Total" row summing
the counts across all sources.
}
\description{
This function processes a dataset of unique citations, expands the \code{cite_source} column,
filters based on user-specified labels (if provided), and then calculates the number
of records imported and distinct records for each citation source. It also adds a
total row summarizing these counts.
}
\details{
The function first checks if the required columns are present in the input data frame.
It then expands the \code{cite_source} column to handle multiple sources listed in a
single row and filters the dataset based on the provided labels (if any).
The function calculates the number of records imported (total rows) and the number
of distinct records (unique \code{duplicate_id} values) for each citation source.
Finally, a total row is added to summarize the counts across all sources.
}
\examples{
# Example usage with a sample dataset
unique_citations <- data.frame(
  cite_source = c("Source1", "Source2", "Source3"),
  cite_label = c("Label1", "Label2", "Label3"),
  duplicate_id = c(1, 2, 3)
)
calculate_initial_records(unique_citations)
}
