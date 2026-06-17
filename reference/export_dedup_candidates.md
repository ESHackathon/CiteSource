# Export manual-review candidate pairs to a CSV file

Saves the candidate duplicate pairs returned as the `$manual_dedup`
element of `dedup_citations(manual = TRUE)` so that manual review can be
completed later. Combine with
[`export_csv()`](https://eshackathon.github.io/CiteSource/reference/export_csv.md)
to defer manual deduplication: export the automatically deduplicated
unique citations *and* these candidate pairs now, then re-import both
later with
[`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md)
and
[`reimport_dedup_candidates()`](https://eshackathon.github.io/CiteSource/reference/reimport_dedup_candidates.md)
to finish the review. Note that *existing files are overwritten without
warning.*

## Usage

``` r
export_dedup_candidates(manual_dedup, filename)
```

## Arguments

- manual_dedup:

  Data frame of candidate pairs, i.e. the `$manual_dedup` element of
  `dedup_citations(manual = TRUE)`.

- filename:

  Name (and path) of file, should end in .csv

## Value

No return value, called for side effects. Saves the candidate pairs as a
'CSV' file to the specified location.

## See also

[`reimport_dedup_candidates()`](https://eshackathon.github.io/CiteSource/reference/reimport_dedup_candidates.md),
[`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md)

## Examples

``` r
if (interactive()) {
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)
  dedup_results <- dedup_citations(examplecitations, manual = TRUE)
  export_dedup_candidates(dedup_results$manual_dedup, tempfile(fileext = ".csv"))
}
```
