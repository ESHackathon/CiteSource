# Deduplicate citations - ASySD wrapper

This function deduplicates citation data. Note that duplicates are
assumed to published in the same journal, so pre-prints and similar
results will not be identified here.

## Usage

``` r
dedup_citations(raw_citations, manual = FALSE, show_unknown_tags = FALSE)
```

## Arguments

- raw_citations:

  Citation dataframe with relevant columns

- manual:

  logical. If TRUE, manually specify pairs of duplicates to merge.
  Default is FALSE.

- show_unknown_tags:

  When a label, source, or other merged field is missing, do you want
  this to show as "unknown"?

## Value

unique citations formatted for CiteSource

## Examples

``` r
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)

# Deduplicate citations without manually specifying pairs and without showing unknown tags
dedup_results <- dedup_citations(examplecitations)
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!

# Deduplicate citations with manual specification of pairs and showing unknown tags
dedup_results_manual_unknown <- dedup_citations(
  examplecitations, 
  manual = TRUE, 
  show_unknown_tags = TRUE
  )
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!
```
