# Export deduplicated citations with source data as CSV file

This function saves deduplicated citations as a CSV file for further
analysis and/or reporting. Metadata can be separated into one column per
source, label or string, which facilitates analysis. Note that *existing
files are overwritten without warning.*

## Usage

``` r
export_csv(
  unique_citations,
  filename = "citesource_exported_citations.csv",
  separate = NULL,
  trim_abstracts = 32000
)
```

## Arguments

- unique_citations:

  Dataframe with unique citations, resulting from
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)

- filename:

  Name (and path) of file, should end in .csv

- separate:

  Character vector indicating which (if any) of cite_source, cite_string
  and cite_label should be split into separate columns to facilitate
  further analysis.

- trim_abstracts:

  Some databases may return full-text that is misidentified as an
  abstract. This inflates file size and may lead to issues with Excel,
  which cannot deal with more than 32,000 characters per field.
  Therefore, the default is to trim very long abstracts to 32,000
  characters. Set a lower number to reduce file size, or NULL to retain
  abstracts as they are.

## Value

The function saves the deduplicated citations as a CSV file to the
specified location.

## Examples

``` r
if (interactive()) {
  # Load example data from the package
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)
  dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
  export_csv(dedup_results, "cite_sources.csv", separate = "cite_source")
}
```
