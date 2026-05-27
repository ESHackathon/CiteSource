# search_summary_table

This function creates a table with footnotes for columns in the table.
It uses the gt package to create the table and adds footnotes to various
columns.

## Usage

``` r
record_summary_table(data)
```

## Arguments

- data:

  A data frame that must contain the columns "Source", "Records
  Imported", "Distinct Records", "Unique records", "Non-unique Records",
  "Source Contribution %", "Source Unique Contribution %", and "Source
  Unique %". The "Source" column is used as the row names of the table.

## Value

A gt object representing the table.
