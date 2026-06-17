# Reimport manual-review candidate pairs exported from CiteSource

Reads a CSV of candidate duplicate pairs previously written by
[`export_dedup_candidates()`](https://eshackathon.github.io/CiteSource/reference/export_dedup_candidates.md)
(i.e. the `$manual_dedup` element of `dedup_citations(manual = TRUE)`).
This supports a deferred workflow: run automatic deduplication now,
export both the unique citations and the candidate pairs, and complete
the manual review later after re-importing.

## Usage

``` r
reimport_dedup_candidates(filename)
```

## Arguments

- filename:

  Name (and path) of the candidate-pairs CSV, should end in .csv

## Value

A data frame of candidate pairs with `duplicate_id.x` / `duplicate_id.y`
read as character (matching the unique citations from
[`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md)),
ready for review and
[`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md).

## Details

After review, set the `result` column to `"match"` for confirmed
duplicates and pass the result, together with the reimported unique
citations, to
[`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md).

## See also

[`export_dedup_candidates()`](https://eshackathon.github.io/CiteSource/reference/export_dedup_candidates.md),
[`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md)

## Examples

``` r
if (interactive()) {
  candidates <- reimport_dedup_candidates("path/to/candidates.csv")
  # mark confirmed duplicates, then merge into the reimported unique set
  candidates$result <- ifelse(candidates$result == "match", "match", "no_match")
  final <- dedup_citations_add_manual(reimport_csv("unique.csv"), candidates)
}
```
