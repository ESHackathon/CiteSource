# Calculate Detailed Record Counts

This function processes a dataset and expands the 'cite_source' column,
filters on user-specified labels (if provided), and calculates detailed
counts such as the records imported, distinct records, unique records,
non-unique records, and several percentage contributions for each
citation source/method it also adds a total row summarizing these
counts.

## Usage

``` r
calculate_detailed_records(
  unique_citations,
  n_unique,
  labels_to_include = NULL
)
```

## Arguments

- unique_citations:

  A data frame containing unique citations. The data frame must include
  the columns `cite_source`, `cite_label`, and `duplicate_id`.

- n_unique:

  A data frame containing counts of unique records, typically filtered
  by specific criteria (e.g., `cite_label == "search"`).

- labels_to_include:

  An optional character vector of labels to filter the citations. If
  provided, only citations matching these labels will be included in the
  counts. if 'NULL' all labels are included. Default is 'NULL'.

## Value

A data frame with detailed counts for each citation source, including:

- `Records Imported`: Total number of records imported.

- `Distinct Records`: Number of distinct records after deduplication.

- `Unique Records`: Number of unique records specific to a source.

- `Non-unique Records`: Number of records found in other sources.

- `Source Contribution %`: Percentage contribution of each source to the
  total distinct records.

- `Source Unique Contribution %`: Percentage contribution of each source
  to the total unique records.

- `Source Unique %`: Percentage of unique records within the distinct
  records for each source.

## Details

The function first checks if the required columns are present in the
input data frames. It then expands the `cite_source` column, filters the
data based on the provided labels (if any), and calculates various
counts and percentages for each citation source. The function also adds
a total row summarizing these counts across all sources.

## Examples

``` r
# Example usage with a sample dataset
unique_citations <- data.frame(
  cite_source = c("Source1, Source2", "Source2", "Source3"),
  cite_label = c("Label1", "Label2", "Label1"),
  duplicate_id = c(1, 2, 3)
)
n_unique <- data.frame(
  cite_source = c("Source1", "Source2", "Source3"),
  cite_label = c("search", "search", "search"),
  unique = c(10, 20, 30)
)
calculate_detailed_records(unique_citations, n_unique, labels_to_include = "search")
#> [1] Source             Records.Imported   Distinct.Records   Unique.Records    
#> [5] Non.unique.Records
#> <0 rows> (or 0-length row.names)
```
