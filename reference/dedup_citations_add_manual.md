# Remove pairs with manual dedup - ASySD wrapper

This function deduplicates citation data. Note that duplicates are
assumed to published in the same journal, so pre-prints and similar
results will not be identified here.

## Usage

``` r
dedup_citations_add_manual(unique_citations, additional_pairs)
```

## Arguments

- unique_citations:

  Unique citations post deduplication

- additional_pairs:

  TRUE duplicate pairs

## Value

unique citations formatted for CiteSource

## Examples

``` r
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
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
```
