# Add manually identified duplicate pairs to a deduplicated dataset

Add manually identified duplicate pairs to a deduplicated dataset

## Usage

``` r
dedup_citations_add_manual(unique_citations, additional_pairs)
```

## Arguments

- unique_citations:

  Unique citations returned by
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)

- additional_pairs:

  Dataframe of manually confirmed duplicate pairs (a subset of the
  `$manual_dedup` output). If a `result` column is present, only rows
  where `result == "match"` are merged.

## Value

Updated unique citations dataframe with manual duplicates merged.

## Examples

``` r
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds",
                                      package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)

# Deduplicate and retrieve manual pairs
dedup_results <- dedup_citations(examplecitations, manual = TRUE)
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!
# (user reviews dedup_results$manual_dedup and sets result == "match" for true dups)
# final <- dedup_citations_add_manual(dedup_results$unique, dedup_results$manual_dedup)
```
