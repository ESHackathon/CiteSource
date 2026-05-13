# Deduplicate citations

Deduplicates citation data. Duplicates are assumed to be published in
the same journal, so pre-prints vs. their published versions will not be
merged.

## Usage

``` r
dedup_citations(raw_citations, manual = FALSE, show_unknown_tags = FALSE)
```

## Arguments

- raw_citations:

  Citation dataframe with relevant columns

- manual:

  logical. If TRUE, return the full result list including potential
  pairs for manual review. Default is FALSE.

- show_unknown_tags:

  When a label, source, or other merged field is missing, show it as
  "unknown"? Default FALSE.

## Value

When `manual = FALSE`: a dataframe of unique citations. When
`manual = TRUE`: a list with `$unique` (unique citations) and
`$manual_dedup` (potential pairs for review).

## Examples

``` r
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds",
                                      package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)

# Deduplicate citations
dedup_results <- dedup_citations(examplecitations)
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!

# Return potential pairs for manual review
dedup_results_manual <- dedup_citations(examplecitations, manual = TRUE)
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!
```
