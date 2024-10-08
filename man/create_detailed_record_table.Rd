% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_count_and_table.R
\name{create_detailed_record_table}
\alias{create_detailed_record_table}
\title{Create a Detailed Record Table}
\usage{
create_detailed_record_table(data)
}
\arguments{
\item{data}{A data frame containing the detailed counts for each citation source.
The data frame must include the following columns:
\itemize{
\item \code{Source}: The name of the citation source.
\item \verb{Records Imported}: The total number of records imported from the source.
\item \verb{Distinct Records}: The number of distinct records after deduplication within the source.
\item \verb{Unique Records}: The number of records unique to that source.
\item \verb{Non-unique Records}: The number of records found in at least one other source.
\item \verb{Source Contribution \%}: The percentage contribution of each source to the total distinct records.
\item \verb{Source Unique Contribution \%}: The percentage contribution of each source to the total unique records.
\item \verb{Source Unique \%}: The percentage of records from each source that were unique.
}}
}
\value{
A \code{gt} table object summarizing the detailed record counts for each citation source.
}
\description{
This function generates a formatted summary table using the \code{gt} package,
which displays detailed counts for each citation source. The table includes
columns for the number of records imported, distinct records, unique records,
non-unique records, and various contribution percentages. Data from the
function calculate_detailed_records is pre-formatted for this table.
}
\details{
The function checks for the presence of all required columns in the input data frame.
If any required columns are missing, the function stops and returns an error message
specifying the missing columns. This ensures that the input data is correctly formatted
before attempting to generate the table.

The generated table includes a header and footnotes that provide additional context
for each column, explaining the meaning of the data presented.
}
\examples{
# Example usage with a sample dataset
sample_data <- data.frame(
  Source = c("Source1", "Source2", "Source3", "Total"),
  `Records Imported` = c(100, 150, 250, 500),
  `Distinct Records` = c(90, 140, 230, 460),
  `Unique Records` = c(50, 70, 120, 240),
  `Non-unique Records` = c(40, 70, 110, 220),
  `Source Contribution \%` = c("39.1\%", "60.9\%", "100\%", "100\%"),
  `Source Unique Contribution \%` = c("41.7\%", "58.3\%", "100\%", "100\%"),
  `Source Unique \%` = c("55.6\%", "50\%", "52.2\%", "52.2\%"),
  check.names = FALSE
)

# Create the detailed record table
create_detailed_record_table(sample_data)
}
