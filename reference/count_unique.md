# Count number of unique and non-unique citations from different sources, labels, and strings

Count number of unique and non-unique citations from different sources,
labels, and strings

## Usage

``` r
count_unique(unique_data, include_references = FALSE)
```

## Arguments

- unique_data:

  from ASySD, merged unique rows with duplicate IDs

- include_references:

  Should bibliographic detail be included in return?

## Value

dataframe with indicators of where a citation appears, with
source/label/string as column

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

# Count unique and non-unique citations
count_unique(dedup_results)
#> # A tibble: 166 × 7
#>    duplicate_id cite_source cite_label cite_string record_ids       unique type 
#>    <chr>        <chr>       <chr>      <chr>       <chr>            <lgl>  <fct>
#>  1 1001         DIM         search     NA          1001             TRUE   uniq…
#>  2 1002         WoS         search     NA          1002, 1081       FALSE  dupl…
#>  3 1002         DIM         search     NA          1002, 1081       FALSE  dupl…
#>  4 1003         WoS         search     NA          1003             TRUE   uniq…
#>  5 1004         LENS        search     NA          1004             TRUE   uniq…
#>  6 1005         DIM         search     NA          1005, 1140       TRUE   uniq…
#>  7 1005         DIM         screened   NA          1005, 1140       TRUE   uniq…
#>  8 1006         DIM         search     NA          1006             TRUE   uniq…
#>  9 1007         WoS         search     NA          1007, 1117, 1164 TRUE   uniq…
#> 10 1007         WoS         screened   NA          1007, 1117, 1164 TRUE   uniq…
#> # ℹ 156 more rows
```
