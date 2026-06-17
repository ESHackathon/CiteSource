# Reimport a CSV-file exported from CiteSource

This function reimports a csv file that was tagged and deduplicated by
CiteSource. It allows to continue with further analyses without
repeating that step, and also allows users to make any manual
corrections to tagging or deduplication. Note that this function only
works on CSV files that were written with
`export_csv(..., separate = NULL)`

## Usage

``` r
reimport_csv(filename)
```

## Arguments

- filename:

  Name (and path) of CSV file to be reimported, should end in .csv

## Value

A data frame containing the imported citation data if all required
columns are present.

## Examples

``` r
if (interactive()) {
  citations <- reimport_csv("path/to/citations.csv")
}
```
