# Export deduplicated citations with source data as CSV file

This function saves deduplicated citations as a CSV file for further
analysis and/or reporting. Metadata can be separated into one column per
source, label or string, which facilitates analysis. Note that *existing
files are overwritten without warning.*

## Usage

``` r
export_csv(
  unique_citations,
  filename,
  fields = "full",
  separate = NULL,
  trim_abstracts = 32000,
  manual_dedup_complete = FALSE
)
```

## Arguments

- unique_citations:

  Dataframe with unique citations, resulting from
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)

- filename:

  Name (and path) of file, should end in .csv

- fields:

  Controls which columns are included. Use `"full"` (default) to export
  all columns (required for reimport into CiteSource via
  [`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md));
  `"standard"` to export core bibliographic fields plus `cite_source`,
  `cite_label`, and `cite_string` (suitable for import into RELApp or
  other screening tools); or a character vector of column names for a
  custom selection. Note that exports other than `"full"` cannot be
  reimported into CiteSource.

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

- manual_dedup_complete:

  Logical. Records, in a `manual_dedup_complete` column, whether manual
  deduplication has been completed for this set (default `FALSE`). Set
  `TRUE` after confirming manual pairs with
  [`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md).
  This flag is read back by
  [`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md)
  and lets later steps know whether candidate pairs still need review.
  Only written when `fields = "full"`.

## Value

No return value, called for side effects. Saves the deduplicated
citations as a 'CSV' file to the specified location.

## Examples

``` r
if (interactive()) {
  # Load example data from the package
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)
  dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
  export_csv(dedup_results, tempfile(fileext = ".csv"), separate = "cite_source")
  # Standard export for RELApp / screening tools (not reimportable into CiteSource):
  export_csv(dedup_results, tempfile(fileext = ".csv"), fields = "standard")
}
```
