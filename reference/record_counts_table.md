# record_counts_table

This function creates a table with footnotes for columns in the table.
It uses the gt package to create the table and adds footnotes to the
"Records Imported" and "Distinct Records" columns.

## Usage

``` r
record_counts_table(data)
```

## Arguments

- data:

  A data frame that must contain the columns "Source", "Records
  Imported", and "Distinct Records". The "Source" column is used as the
  row names of the table.

## Value

A gt object representing the table.
