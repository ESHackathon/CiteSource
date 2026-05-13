# Calculate phase counts, precision, and recall

This function calculates counts for different phases and calculates
precision and recall for each source based on unique citations and
citations dataframe. The phases should be labeled as 'screened' and
'final' (case-insensitive) in the input dataframes. The function will
give a warning if these labels are not present in the input dataframes.

## Usage

``` r
calculate_phase_count(unique_citations, citations, db_colname)
```

## Arguments

- unique_citations:

  A dataframe containing unique citations with phase information. The
  phase information must be provided in a column named 'cite_label' in
  the dataframe.

- citations:

  A dataframe containing all citations with phase information. The phase
  information must be provided in a column named 'cite_label' in the
  dataframe.

- db_colname:

  The name of the column representing the source database.

## Value

A dataframe containing distinct counts, counts for different phases,
precision, and recall for each source, as well as totals.

## Details

The function will give a warning if 'screened' and 'final' labels are
not present in the 'cite_label' column of the input dataframes.

## Examples

``` r
unique_citations <- data.frame(
db_source = c("Database1", "Database1", "Database2", "Database3", "Database3", "Database3"),
cite_label = c("screened", "final", "screened", "final", "screened", "final"),
duplicate_id = c(102, 102, 103, 103, 104, 104),
other_data = 1:6
)

citations <- data.frame(
db_source = c("Database1", "Database1", "Database1", "Database2", "Database2", "Database3"),
cite_label = c("screened", "final", "screened", "final", "screened", "final"),
other_data = 7:12
)

result <- calculate_phase_count(unique_citations, citations, "db_source")
#> Warning: `calculate_phase_count()` is deprecated. Use `calculate_phase_records()` instead.
result
#>      Source Distinct Records screened final Precision Recall
#> 1 Database1                2        1     1        50  33.33
#> 2 Database2                1        1     0         0      0
#> 3 Database3                3        1     2     66.67  66.67
#> 4     Total                6        3     3        50     NA
```
