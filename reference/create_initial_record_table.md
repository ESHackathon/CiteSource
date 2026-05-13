# Initial Record Table

This function generates a formatted table displaying the record counts
for each citation source, including the number of records imported and
the distinct records after deduplication.

## Usage

``` r
create_initial_record_table(data)
```

## Arguments

- data:

  A data frame containing the record counts for each citation source. It
  must include columns `Source`, `Records_Imported`, and
  `Distinct_Records`.

## Value

A `gt` table object summarizing the record counts for each citation
source.

## Details

The function checks if the input data frame is empty and returns an
empty `gt` table if no data is present. Otherwise, it generates a
formatted table with labeled columns and adds footnotes explaining the
meaning of each column.

## Examples

``` r
sample_data <- data.frame(
  Source = c("Source1", "Source2", "Source3"),
  Records_Imported = c(100, 150, 250),
  Distinct_Records = c(90, 140, 230)
)
create_initial_record_table(sample_data)


  


Record Counts
```
