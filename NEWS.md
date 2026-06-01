# CiteSource 0.2.0

## Breaking changes

- Requires R >= 4.1.0 (native pipe and `across()` syntax used throughout).

## New features

- Incremental deduplication: `dedup_citations_add_sources()` adds new raw
  citations to a previously deduplicated set and deduplicates across both,
  preserving prior automatic and manual merge decisions and the original
  `record_ids` provenance. For the same data it yields the same unique set as
  deduplicating everything from scratch. Exposed in the Shiny app — re-upload a
  deduplicated set, add new citation files, and "Find duplicates" merges them
  in. Works in `manual = TRUE` mode to surface new candidate pairs for review.
- Deferred manual deduplication: run automatic dedup now and complete manual
  review later. `export_dedup_candidates()` / `reimport_dedup_candidates()`
  persist and restore the `$manual_dedup` candidate pairs, and `export_csv()`
  gains a `manual_dedup_complete` flag (written as a column, read back by
  `reimport_csv()`) so downstream steps know whether review is still pending.
  Re-import, mark `result == "match"`, and merge with
  `dedup_citations_add_manual()`.
- `reimport_csv()` now reads all columns as character, matching the canonical
  (all-character) types produced by `dedup_citations()`. This is required so a
  reimported set can re-enter `dedup_citations_add_manual()` (and future
  re-deduplication) without column-type clashes.
- `read_citations()` now warns when `cite_label` values are outside the standard
  vocabulary (`search`, `screened`, `final`), since phase-analysis functions
  depend on those exact strings.

## Bug fixes

- `calculate_phase_records()`: `n_distinct()` called as a terminal pipe no
  longer throws an error; fixed by replacing with `summarise() |> pull()`.
- `calculate_initial_records()` / `calculate_detailed_records()`: separator
  regex in `separate_rows()` changed from `","` to `",\\s*"` so sources with
  spaces after commas are split correctly.
- `generate_apa_reference()`: NA DOI values no longer cause an error in
  `str_detect()`; handled with `case_when(is.na(doi) ~ NA_character_, ...)`.
- Shiny app — `summaryPrecTab`: `n_unique` was referenced without the `rv$`
  prefix, causing a scope error; fixed.
- Shiny app — `manual_dedup_dt`: `columnDefs` `targets` now computed as
  0-based integer indices rather than column name strings, matching the DT API.
- `count_unique()` / `compare_sources()`: added `!is.na()` guard alongside
  `!= ""` so NA values in `cite_source`, `cite_label`, `cite_string` no longer
  cause filter or pivot errors.
- Shiny app — `rv$pairs_to_check[,1:36]` subsetting removed; full data frame
  passed directly.

## Performance

- `generate_apa_citation()` and `generate_apa_reference()`: all `rowwise()`
  calls replaced with `purrr::map2_chr()` and `purrr::pmap_chr()`, avoiding
  per-row dplyr group overhead.
- Shiny app: `compare_sources()` is now computed once in a shared reactive and
  consumed by both the heatmap and upset plot, halving the work on each render.
- Shiny app: multi-value column filtering replaced from per-row `sapply` +
  `str_split` to vectorized `strsplit` + `vapply` via a file-scope helper
  `.filter_multivalue_col`.

## CRAN / dependency changes

- ASySD deduplication functions vendored directly into `R/asys_dedup.R`
  (GPL-3, with attribution to CAMARADES / Kaitlyn Hair); the GitHub-only
  `ASySD` package is no longer a dependency.
- `Remotes:` field removed from DESCRIPTION.
- New imports: `cli`, `igraph`, `parallelly`, `RecordLinkage`, `utf8`.
- Removed from imports: `ASySD`, `plogr`.
- Added to Suggests: `bslib`.
- `.onLoad` side effects removed; `shiny.maxRequestSize` is no longer set
  at package load time.
- `%>%` re-export removed; use the native `|>` pipe.

## Deprecations

- `record_counts()` → `calculate_initial_records()`
- `calculate_record_counts()` → `calculate_detailed_records()`
- `calculate_phase_count()` → `calculate_phase_records()`
- `record_counts_table()` → `create_initial_record_table()`
- `record_summary_table()` → `create_summary_record_table()`
- `precision_sensitivity_table()` → `create_phase_record_table()`

All deprecated functions remain callable with a `.Deprecated()` warning
pointing to their replacements.

## Internal

- Google Analytics injection simplified: URL-path reactive detection replaced
  by a file-scope `CITESOURCE_ENV` environment variable check evaluated once at
  app startup.

# CiteSource 0.0.1

* Added a `NEWS.md` file to track changes to the package.

- Added dependency on latest version of the ASySD R package

- Simplified dedup function arguments (now specified within call to ASySD)

- Integrated new dedup function into R shiny app

# CiteSource 0.1.1

- Added new functions which allow creation of tables and plots based on deduplicated (reimported) data. 

- Updated shiny functionality, look and feel, and documentation

- Added new vignettes 