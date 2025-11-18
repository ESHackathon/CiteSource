# Calculate record counts function Calculate and combine counts of distinct records, imported records, and unique records for each database

This function calculates the counts of distinct records, records
imported, and unique records for each database source. It combines these
counts into one dataframe and calculates several ratios and percentages
related to the unique and distinct counts. It also calculates the total
for each count type.

## Usage

``` r
calculate_record_counts(unique_citations, citations, n_unique, db_colname)
```

## Arguments

- unique_citations:

  Dataframe. The dataframe for calculating distinct records count.

- citations:

  Dataframe. The dataframe for calculating records imported count.

- n_unique:

  Dataframe. The dataframe for calculating unique records count.

- db_colname:

  Character. The name of the column containing the database source
  information.

## Value

A dataframe with counts of distinct records, imported records, and
unique records for each source, including total counts and several
calculated ratios and percentages.

## Examples

``` r
unique_citations <- data.frame(
  db_source = c("Database1", "Database1", "Database2", "Database3", "Database3", "Database3"),
  other_data = 1:6
)

citations <- data.frame(
  db_source = c("Database1", "Database1", "Database1", "Database2", "Database2", "Database3"),
  other_data = 7:12
)

n_unique <- data.frame(
  cite_source = c("Database1", "Database2", "Database2", "Database3", "Database3", "Database3"),
 cite_label = c("search", "final", "search", "search", "search", "final"),
  unique = c(1, 0, 1, 1, 1, 0)
)

result <- calculate_record_counts(unique_citations, citations, n_unique, "db_source")
print(result)
#>      Source Records Imported Distinct Records Unique records Non-unique Records
#> 1 Database1                3                2              1                  1
#> 2 Database2                2                1              1                  0
#> 3 Database3                1                3              2                  1
#> 4     Total                6                6              4                  2
#>   Source Contribution % Source Unique Contribution % Source Unique %
#> 1                 33.3%                        25.0%           50.0%
#> 2                 16.7%                        25.0%          100.0%
#> 3                 50.0%                        50.0%           66.7%
#> 4                  <NA>                         <NA>            <NA>
```
