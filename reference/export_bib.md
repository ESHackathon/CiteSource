# Export deduplicated citations to .bib file

This function saves deduplicated citations as a BibTex file with
sources, labels and strings included in the `note` field (if they were
initially provided for any of the citations). Therefore, beware that
**any `note` field that might be included in `citations` will be
overwritten**. Also note that *existing files are overwritten without
warning.*

## Usage

``` r
export_bib(
  citations,
  filename = "citations.bib",
  include = c("sources", "labels", "strings")
)
```

## Arguments

- citations:

  Dataframe with unique citations, resulting from
  [`dedup_citations()`](http://www.eshackathon.org/CiteSource/reference/dedup_citations.md)

- filename:

  Name (and path) of file, should end in .ris

- include:

  Character. One or more of sources, labels or strings

## Examples

``` r
if (interactive()) {
  # Load example data from the package
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)
  dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
  export_bib(dedup_results$unique, "cite_sources.bib", include = "sources")
}
```
