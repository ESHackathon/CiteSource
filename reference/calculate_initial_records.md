# Calculate Initial Records Unique Citations

This function processes a dataset of unique citations, expands the
`cite_source` column, filters based on user-specified labels (if
provided), and then calculates the number of records imported and
distinct records for each citation source. It also adds a total row
summarizing these counts.

## Usage

``` r
calculate_initial_records(unique_citations, labels_to_include = NULL)
```

## Arguments

- unique_citations:

  A data frame containing the unique citations. It must contain the
  columns `cite_source`, `cite_label`, and `duplicate_id`.

- labels_to_include:

  An optional character vector of labels to filter the citations. If
  provided, only citations matching these labels will be included in the
  counts. Default is NULL, meaning no filtering will be applied.

## Value

A data frame containing the counts of `Records Imported` and
`Distinct Records` for each citation source. The data frame also
includes a "Total" row summing the counts across all sources.

## Details

The function first checks if the required columns are present in the
input data frame. It then expands the `cite_source` column to handle
multiple sources listed in a single row and filters the dataset based on
the provided labels (if any). The function calculates the number of
records imported (total rows) and the number of distinct records (unique
`duplicate_id` values) for each citation source. Finally, a total row is
added to summarize the counts across all sources.

## Examples

``` r
# Example usage with a sample dataset
unique_citations <- data.frame(
  cite_source = c("Source1", "Source2", "Source3"),
  cite_label = c("Label1", "Label2", "Label3"),
  duplicate_id = c(1, 2, 3)
)
calculate_initial_records(unique_citations)
#> # A tibble: 4 × 3
#>   Source  Records_Imported Distinct_Records
#>   <chr>              <int>            <int>
#> 1 Source1                1                1
#> 2 Source2                1                1
#> 3 Source3                1                1
#> 4 Total                  3                3
```
