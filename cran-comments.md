## Resubmission

This is a resubmission of a new package (not yet on CRAN). It addresses all
points raised in the previous review:

* **Title / Description formatting.** The `Title` no longer begins with
  "An R Package for", and the `Description` no longer begins with "Tools for".
  The `Description` now opens with an active verb describing what the package
  does. All software/package names in the `Title` and `Description`
  (`'shiny'`, `'RIS'`, `'CSV'`) are written in single quotes.

* **Missing `\value` tags.** `@return` documentation was added to all flagged
  functions (`export_bib`, `export_ris`, `plot_contributions`,
  `plot_source_overlap_upset`, `reimport_ris`) and the `.Rd` files were
  regenerated. Functions called for their side effects use the phrasing
  "No return value, called for side effects." and state what file is written;
  functions that return objects describe the class and meaning of the output.
  All exported functions now carry a `\value` tag.

* **Writing to the home filespace.** Default `filename` values were removed
  from `export_csv()`, `export_ris()`, and `export_bib()`, so the caller must
  supply an explicit path. The internal `write_refs()` fallback in
  `R/import_export_helpers.R` now writes to `tempdir()` instead of `getwd()`.
  All examples write to `tempfile()` / `tempdir()`.

Since the previous review, the package version was incremented from 0.2.0 to
0.2.1 and a few features were added (incremental deduplication via
`dedup_citations_add_sources()`, deferred manual review via
`export_dedup_candidates()` / `reimport_dedup_candidates()`, and a
deduplication provenance log via `dedup_log()`). These are documented in
`NEWS.md`. All newly exported functions include `\value` documentation and
write only to user-supplied paths.

## Test environments

* Local: Windows 11, R 4.5.0

## R CMD check results

0 errors | 0 warnings | 0 notes (local, `R CMD check --as-cran`).

On CRAN's incoming checks we expect the standard "New submission" NOTE, since
the package is not yet published. Any URLs flagged (e.g. the GNU GPL license
pages) are valid and resolve in a browser.

## Reverse dependencies

None — this is a new submission with no downstream dependents.

## Notes

`R/asys_dedup.R` contains code vendored from the ASySD package
(GPL >= 3, CAMARADES Group / Kaitlyn Hair) with attribution in the file
header. CiteSource is also GPL >= 3, so the licenses are compatible.
