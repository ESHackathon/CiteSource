# Changelog

## CiteSource 1.0.0

First stable release, following acceptance on CRAN.

### Bug fixes

- [`compare_sources()`](https://eshackathon.github.io/CiteSource/reference/compare_sources.md)
  now honors `include_references`: when `TRUE`, the bibliographic
  columns are joined back onto the source/label/string indicator matrix,
  matching the behavior of
  [`count_unique()`](https://eshackathon.github.io/CiteSource/reference/count_unique.md).
  Previously the argument was silently ignored and only indicator
  columns were returned.
- [`calculate_detailed_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_detailed_records.md):
  the Total row’s `Distinct Records` now respects `labels_to_include`.
  It previously used the unfiltered input, making the Total inconsistent
  with the filtered per-source rows and overstating distinct records
  when labels were restricted.

### Improvements

- [`create_detailed_record_table()`](https://eshackathon.github.io/CiteSource/reference/create_detailed_record_table.md)
  and
  [`create_precision_sensitivity_table()`](https://eshackathon.github.io/CiteSource/reference/create_precision_sensitivity_table.md)
  redesigned for readability: related columns are grouped under spanner
  headers, the contribution/uniqueness percentages have clearer labels,
  Precision and Recall are formatted as percentages, and the Total row
  is visually distinguished as a summary.

## CiteSource 0.2.1

CRAN release: 2026-06-16

### New features

- Incremental deduplication:
  [`dedup_citations_add_sources()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_sources.md)
  adds new citations to a previously deduplicated set and deduplicates
  across both, preserving prior automatic and manual merge decisions and
  the original `record_ids` provenance. For the same data it yields the
  same unique set as deduplicating everything from scratch. Exposed in
  the Shiny app — re-upload a deduplicated set, add new citation files,
  and “Find duplicates” merges them in. Works in `manual = TRUE` mode to
  surface new candidate pairs for review.
- Deferred manual deduplication: run automatic dedup now and complete
  manual review later.
  [`export_dedup_candidates()`](https://eshackathon.github.io/CiteSource/reference/export_dedup_candidates.md)
  /
  [`reimport_dedup_candidates()`](https://eshackathon.github.io/CiteSource/reference/reimport_dedup_candidates.md)
  persist and restore the `$manual_dedup` candidate pairs, and
  [`export_csv()`](https://eshackathon.github.io/CiteSource/reference/export_csv.md)
  gains a `manual_dedup_complete` flag (written as a column, read back
  by
  [`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md))
  so downstream steps know whether review is still pending. Re-import,
  mark `result == "match"`, and merge with
  [`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md).
- Shiny app: re-importing a deduplicated set now shows a read-only
  source overview (records per source, and per label/string) on the
  upload page so you can see what is already in the set before adding
  more; the re-upload input accepts a candidate-pairs CSV and several
  files at once.

### Bug fixes

- [`reimport_csv()`](https://eshackathon.github.io/CiteSource/reference/reimport_csv.md)
  now reads all columns as character, matching the canonical
  (all-character) types produced by
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md).
  This is required so a reimported set can re-enter
  [`dedup_citations_add_manual()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations_add_manual.md)
  (and incremental re-deduplication) without column-type clashes.
- Shiny app: the re-upload (re-import) input no longer errors when more
  than one file is selected; each file is routed by content
  (deduplicated set vs. candidate-pairs CSV vs. RIS).

### Documentation

- In-app User Guide and README updated to document incremental and
  deferred deduplication; the file upload page labels were clarified.

## CiteSource 0.2.0

### Breaking changes

- Requires R \>= 4.1.0 (native pipe and `across()` syntax used
  throughout).

### New features

- [`read_citations()`](https://eshackathon.github.io/CiteSource/reference/read_citations.md)
  now warns when `cite_label` values are outside the standard vocabulary
  (`search`, `screened`, `final`), since phase-analysis functions depend
  on those exact strings.

### Bug fixes

- [`calculate_phase_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_phase_records.md):
  `n_distinct()` called as a terminal pipe no longer throws an error;
  fixed by replacing with `summarise() |> pull()`.
- [`calculate_initial_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_initial_records.md)
  /
  [`calculate_detailed_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_detailed_records.md):
  separator regex in `separate_rows()` changed from `","` to `",\\s*"`
  so sources with spaces after commas are split correctly.
- `generate_apa_reference()`: NA DOI values no longer cause an error in
  `str_detect()`; handled with
  `case_when(is.na(doi) ~ NA_character_, ...)`.
- Shiny app — `summaryPrecTab`: `n_unique` was referenced without the
  `rv$` prefix, causing a scope error; fixed.
- Shiny app — `manual_dedup_dt`: `columnDefs` `targets` now computed as
  0-based integer indices rather than column name strings, matching the
  DT API.
- [`count_unique()`](https://eshackathon.github.io/CiteSource/reference/count_unique.md)
  /
  [`compare_sources()`](https://eshackathon.github.io/CiteSource/reference/compare_sources.md):
  added `!is.na()` guard alongside `!= ""` so NA values in
  `cite_source`, `cite_label`, `cite_string` no longer cause filter or
  pivot errors.
- Shiny app — `rv$pairs_to_check[,1:36]` subsetting removed; full data
  frame passed directly.

### Performance

- `generate_apa_citation()` and `generate_apa_reference()`: all
  `rowwise()` calls replaced with
  [`purrr::map2_chr()`](https://purrr.tidyverse.org/reference/map2.html)
  and
  [`purrr::pmap_chr()`](https://purrr.tidyverse.org/reference/pmap.html),
  avoiding per-row dplyr group overhead.
- Shiny app:
  [`compare_sources()`](https://eshackathon.github.io/CiteSource/reference/compare_sources.md)
  is now computed once in a shared reactive and consumed by both the
  heatmap and upset plot, halving the work on each render.
- Shiny app: multi-value column filtering replaced from per-row
  `sapply` + `str_split` to vectorized `strsplit` + `vapply` via a
  file-scope helper `.filter_multivalue_col`.

### CRAN / dependency changes

- ASySD deduplication functions vendored directly into `R/asys_dedup.R`
  (GPL-3, with attribution to CAMARADES / Kaitlyn Hair); the GitHub-only
  `ASySD` package is no longer a dependency.
- `Remotes:` field removed from DESCRIPTION.
- New imports: `cli`, `igraph`, `parallelly`, `RecordLinkage`, `utf8`.
- Removed from imports: `ASySD`, `plogr`.
- Added to Suggests: `bslib`.
- `.onLoad` side effects removed; `shiny.maxRequestSize` is no longer
  set at package load time.
- `%>%` re-export removed; use the native `|>` pipe.

### Deprecations

- [`record_counts()`](https://eshackathon.github.io/CiteSource/reference/record_counts.md)
  →
  [`calculate_initial_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_initial_records.md)
- [`calculate_record_counts()`](https://eshackathon.github.io/CiteSource/reference/calculate_record_counts.md)
  →
  [`calculate_detailed_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_detailed_records.md)
- [`calculate_phase_count()`](https://eshackathon.github.io/CiteSource/reference/calculate_phase_count.md)
  →
  [`calculate_phase_records()`](https://eshackathon.github.io/CiteSource/reference/calculate_phase_records.md)
- `record_counts_table()` →
  [`create_initial_record_table()`](https://eshackathon.github.io/CiteSource/reference/create_initial_record_table.md)
- `record_summary_table()` → `create_summary_record_table()`
- `precision_sensitivity_table()` → `create_phase_record_table()`

All deprecated functions remain callable with a
[`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) warning
pointing to their replacements.

### Internal

- Google Analytics injection simplified: URL-path reactive detection
  replaced by a file-scope `CITESOURCE_ENV` environment variable check
  evaluated once at app startup.

## CiteSource 0.0.1

- Added a `NEWS.md` file to track changes to the package.

- Added dependency on latest version of the ASySD R package

- Simplified dedup function arguments (now specified within call to
  ASySD)

- Integrated new dedup function into R shiny app

## CiteSource 0.1.1

- Added new functions which allow creation of tables and plots based on
  deduplicated (reimported) data.

- Updated shiny functionality, look and feel, and documentation

- Added new vignettes
