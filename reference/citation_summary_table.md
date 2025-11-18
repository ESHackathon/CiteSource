# Contribution summary table

Create a summary table to show the contribution of each source and the
overall performance of the search. For this to work, labels need to be
used that contrast a "search" stage with one or more later stages.

## Usage

``` r
citation_summary_table(
  citations,
  comparison_type = "sources",
  search_label = "search",
  screening_label = "final",
  top_n = NULL
)
```

## Arguments

- citations:

  A deduplicated tibble as returned by
  [`dedup_citations()`](http://www.eshackathon.org/CiteSource/reference/dedup_citations.md).

- comparison_type:

  Either "sources" to summarise and assess sources or "strings" to
  consider strings.

- search_label:

  One or multiple labels that identify initial search results (default:
  "search") - if multiple labels are provided, they are merged.

- screening_label:

  One or multiple label that identify screened records (default:
  "final") - if multiple are provided, each is compared to the search
  stage.

- top_n:

  Number of sources/strings to display, based on the number of total
  records they contributed at the search stage. Note that calculations
  and totals will still be based on all citations. Defaults to NULL,
  then all sources/strings are displayed.

## Value

A tibble containing the contribution summary table, which shows the
contribution of each source and the overall performance of the search

## Examples

``` r
if (interactive()) {
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)

# Deduplicate citations and compare sources
unique_citations <- dedup_citations(examplecitations)

unique_citations |> 
dplyr::filter(stringr::str_detect(cite_label, "final"))  |> 
record_level_table(return = "DT")
citation_summary_table(unique_citations, screening_label = c("screened", "final"))
}
```
