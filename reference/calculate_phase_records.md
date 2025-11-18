# Calculate Phase Counts with Precision and Recall

This function calculates the distinct record counts, as well as screened
and final record counts, for each citation source across different
phases (e.g., "screened", "final"). It also calculates precision and
recall metrics for each source.

## Usage

``` r
calculate_phase_records(unique_citations, n_unique, db_colname)
```

## Arguments

- unique_citations:

  A data frame containing unique citations. It must include the columns
  `cite_source`, `cite_label`, and `duplicate_id`.

- n_unique:

  A data frame containing counts of unique records. Typically filtered
  by specific criteria, such as `cite_label == "search"`.

- db_colname:

  The name of the column representing the citation source in the
  `unique_citations` data frame.

## Value

A data frame with phase counts and calculated precision and recall for
each citation source, including:

- `Distinct Records`: The count of distinct records per source.

- `screened`: The count of records in the "screened" phase.

- `final`: The count of records in the "final" phase.

- `Precision`: The precision metric calculated as
  `final / Distinct Records`.

- `Recall`: The recall metric calculated as
  `final / Total final records`.

## Details

The function starts by calculating the total distinct records, as well
as the total "screened" and "final" records across all sources. It then
calculates distinct counts for each source, followed by counts for
"screened" and "final" records. Finally, it calculates precision and
recall metrics and adds a total row summarizing these counts across all
sources.

## Examples

``` r
# Example usage with a sample dataset
unique_citations <- data.frame(
  cite_source = c("Source1", "Source2", "Source3"),
  cite_label = c("screened","screened", "final"),
  duplicate_id = c(1, 2, 3)
)
n_unique <- data.frame(
  cite_source = c("Source1", "Source2", "Source3"),
  unique = c(10, 20, 30)
)
calculate_phase_records(unique_citations, n_unique, "cite_source")
#> # A tibble: 4 × 6
#>   Source  Distinct_Records screened final Precision Recall
#>   <chr>              <int>    <dbl> <dbl>     <dbl>  <dbl>
#> 1 Source1                1        1     0       0        0
#> 2 Source2                1        1     0       0        0
#> 3 Source3                1        0     1     100      100
#> 4 Total                  3        2     1      33.3     NA
```
