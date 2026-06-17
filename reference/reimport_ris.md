# Reimport a RIS-file exported from CiteSource

This function reimports a RIS file that was tagged and deduplicated by
CiteSource. It allows to continue with further analyses without
repeating that step, and also allows users to make any manual
corrections to tagging or deduplication. The function can also be used
to replace the import step (for instance if tags are to be added to
individual citations rather than entire files) - in this case, just call
[`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md)
after the import.

## Usage

``` r
reimport_ris(
  filename = "citations.ris",
  source_field = "DB",
  label_field = "C7",
  string_field = "C8",
  duplicate_id_field = "C1",
  record_id_field = "C2",
  tag_naming = "ris_synthesisr",
  verbose = TRUE
)
```

## Arguments

- filename:

  Name (and path) of RIS file to be reimported, should end in .ris

- source_field:

  Character. Which RIS field should cite_sources be read from? NULL to
  set to missing

- label_field:

  Character. Which RIS field should cite_labels be read from? NULL to
  set to missing

- string_field:

  Character. Which RIS field should cite_strings be read from? NULL to
  set to missing

- duplicate_id_field:

  Character. Which RIS field should duplicate IDs be read from? NULL to
  recreate based on row number (note that neither duplicate nor record
  IDs directly affect CiteSource analyses - they can only allow you to
  connect processed data with raw data)

- record_id_field:

  Character. Which RIS field should record IDs be read from? NULL to
  recreate based on row number

- tag_naming:

  Synthesisr option specifying how RIS tags should be replaced with
  names. This should not be changed when using this function to reimport
  a file exported from CiteSource. If you import your own RIS, check
  `names(CiteSource:::synthesisr_code_lookup)` and select any of the
  options that start with `ris_`

- verbose:

  Should confirmation message be displayed?

## Value

A data frame containing the reimported citation data, with 'CiteSource'
metadata columns (cite_source, cite_label, cite_string, duplicate_id,
record_ids) restored from the 'RIS' fields.

## Details

Note that this functions defaults' are based on those in
[`export_ris()`](https://eshackathon.github.io/CiteSource/reference/export_ris.md)
so that these functions can easily be combined.

## Examples

``` r
if (interactive()) {
  dedup_results <- dedup_citations(citations, merge_citations = TRUE)
  tmp <- tempfile(fileext = ".ris")
  export_ris(dedup_results$unique, tmp)
  unique_citations2 <- reimport_ris(tmp)
}
```
