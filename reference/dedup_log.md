# Build a provenance log of all merged duplicate pairs

Combines automatically merged pairs and user-confirmed manual pairs into
a single tibble with a `method` column (`"auto"` / `"manual"`). Useful
for reporting and auditing - e.g. as supplementary material for a
systematic review.

## Usage

``` r
dedup_log(dedup_result, confirmed_manual_pairs = NULL)
```

## Arguments

- dedup_result:

  List returned by `dedup_citations(manual = TRUE)` (must contain
  `$auto_pairs`; optionally `$manual_dedup`).

- confirmed_manual_pairs:

  Optional dataframe of manual pairs the user confirmed as duplicates.
  Typically a subset of `dedup_result$manual_dedup`. If a `result`
  column is present, only rows where `result == "match"` are included.

## Value

Tibble with columns `method`, `record_id1`, `record_id2`, and the common
bibliographic fields (`title1/2`, `author1/2`, `year1/2`, `journal1/2`,
`doi1/2`) when available.

## Examples

``` r
examplecitations_path <- system.file("extdata", "examplecitations.rds",
                                      package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)
dedup_results <- dedup_citations(examplecitations, manual = TRUE)
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!
# Log of just the auto-merged pairs
dedup_log(dedup_results)
#> # A tibble: 83 × 13
#>    method record_id1 record_id2 title1        title2 author1 author2 year1 year2
#>    <chr>  <chr>      <chr>      <chr>         <chr>  <chr>   <chr>   <chr> <chr>
#>  1 auto   1080       1090       ADAPTING TO … ADAPT… GAMARR… GAMARR… 2020  2020 
#>  2 auto   1080       1111       ADAPTING TO … ADAPT… GAMARR… GAMARR… 2020  2020 
#>  3 auto   1012       1142       EVALUATING T… EVALU… SINGH,… SINGH,… 2022  2022 
#>  4 auto   1031       1101       NUMERICAL SI… NUMER… WEIDON… WEIDON… 2012  2012 
#>  5 auto   1005       1140       TRACES OF UR… TRACE… LEE, K… LEE, K… 2021  2021 
#>  6 auto   1032       1125       CHANGES IN W… CHANG… LI, DA… LI, DA… 2016  2016 
#>  7 auto   1057       1113       HEAT ISLAND … HEAT … UNGER,… UNGER,… 1996  1996 
#>  8 auto   1018       1144       DEVELOPMENT … DEVEL… TAHA, … TAHA, … 2021  2021 
#>  9 auto   1021       1149       THRESHOLD TE… THRES… BECKMA… BECKMA… 2021  2021 
#> 10 auto   1021       1163       THRESHOLD TE… THRES… BECKMA… BECKMA… 2021  2021 
#> # ℹ 73 more rows
#> # ℹ 4 more variables: journal1 <chr>, journal2 <chr>, doi1 <chr>, doi2 <chr>
# Or include user-confirmed manual pairs
# dedup_log(dedup_results, confirmed_manual_pairs = my_confirmed_pairs)
```
