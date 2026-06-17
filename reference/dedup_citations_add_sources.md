# Add new citations to a previously deduplicated set and re-deduplicate

Adds further citations (e.g. an additional database search) to a set
that was already deduplicated, and deduplicates the new records against
both the existing set and each other - without discarding the work
already done. Each existing unique record enters as a single row, so
prior automatic and manual merge decisions are preserved; the new
records are integrated and full provenance (the original `record_ids`
behind every merged record) is carried through.

## Usage

``` r
dedup_citations_add_sources(
  existing_citations,
  new_citations,
  manual = FALSE,
  show_unknown_tags = FALSE
)
```

## Arguments

- existing_citations:

  A previously deduplicated set (from
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md),
  [`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md)
  or
  [`reimport_ris()`](https://eshackathon.github.io/CiteSource/reference/reimport_ris.md)) -
  must contain a `duplicate_id` column.

- new_citations:

  New raw citations to add, as returned by
  [`read_citations()`](https://eshackathon.github.io/CiteSource/reference/read_citations.md)
  (with `cite_source` / `cite_label` / `cite_string`).

- manual:

  logical. If TRUE, return the full result list including
  `$manual_dedup` candidate pairs for review (see
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)).
  Default FALSE.

- show_unknown_tags:

  When a label, source, or other merged field is missing, show it as
  "unknown"? Default FALSE.

## Value

When `manual = FALSE`: a dataframe of unique citations across both sets.
When `manual = TRUE`: a list with `$unique`, `$manual_dedup` and
`$auto_pairs` (as in
[`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)).
In both cases `record_ids` retains the original record IDs behind every
merged record.

## Details

This is the incremental counterpart to running
[`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)
on all sources from scratch and, for the same data, produces the same
unique set.

## See also

[`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md),
[`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md)

## Examples

``` r
if (interactive()) {
  existing <- dedup_citations(read_citations(old_files, cite_sources = old_srcs))
  new_raw  <- read_citations(new_files, cite_sources = new_srcs)
  combined <- dedup_citations_add_sources(existing, new_raw)
}
```
