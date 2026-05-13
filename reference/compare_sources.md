# Compare duplicate citations across sources, labels, and strings

Compare duplicate citations across sources, labels, and strings

## Usage

``` r
compare_sources(
  unique_data,
  comp_type = c("sources", "strings", "labels"),
  include_references = FALSE
)
```

## Arguments

- unique_data:

  from ASySD, merged unique rows with duplicate IDs

- comp_type:

  Specify which fields are to be included. One or more of "sources",
  "strings" or "labels" - defaults to all.

- include_references:

  Should bibliographic detail be included in return?

## Value

dataframe with indicators of where a citation appears, with
sources/labels/strings as columns

## Examples

``` r
if (interactive()) {
  # Load example data from the package
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)

  # Deduplicate citations and compare sources
  dedup_results <- dedup_citations(examplecitations)
  compare_sources(dedup_results, comp_type = "sources")
}
```
