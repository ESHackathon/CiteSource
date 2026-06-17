# Record counts function Calculate and combine counts of distinct records and imported records for each database

This function calculates the counts of distinct records and records
imported for each database source. It combines these counts into one
dataframe and calculates the total for each count type.

## Usage

``` r
record_counts(unique_citations, citations, db_colname)
```

## Arguments

- unique_citations:

  Dataframe. The dataframe for calculating distinct records count.

- citations:

  Dataframe. The dataframe for calculating records imported count.

- db_colname:

  Character. The name of the column containing the database source
  information.

## Value

A dataframe with counts of distinct records and imported records for
each source, including total counts.

## Examples

``` r
# Create synthetic data for example
unique_citations <- data.frame(
  title = paste("Article", 1:10),
  db_source = sample(c("Database 1", "Database 2", "Database 3"), 10, replace = TRUE),
  stringsAsFactors = FALSE
)

citations <- data.frame(
  title = paste("Article", 1:20),
  db_source = sample(c("Database 1", "Database 2", "Database 3"), 20, replace = TRUE),
  stringsAsFactors = FALSE
)

# Use the synthetic data with the function
result <- record_counts(unique_citations, citations, "db_source")
#> Warning: `record_counts()` is deprecated. Use `calculate_initial_records()` instead.
result
#>       Source Records Imported Distinct Records
#> 1 Database 1                9                4
#> 2 Database 2                8                3
#> 3 Database 3                3                3
#> 4      Total               20               10
```
