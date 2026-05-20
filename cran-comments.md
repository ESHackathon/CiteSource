## Resubmission

This is a resubmission addressing reviewer feedback:

* **Title / description formatting**: Removed "An R Package for" from the
  `Title` field. Added single quotes around `'shiny'` in `Description` to
  conform to the requirement that package names appear in single quotes.

* **Missing `\value` tags**: Added `@return` documentation to all five
  flagged functions (`export_bib`, `export_ris`, `plot_contributions`,
  `plot_source_overlap_upset`, `reimport_ris`) and re-ran
  `devtools::document()` to regenerate the `.Rd` files. Functions called
  for side effects use the phrasing "No return value, called for side
  effects." Functions that return objects describe the class and meaning
  of the output.

* **Writing to home filespace**: Removed default `filename` values from
  `export_csv()`, `export_ris()`, and `export_bib()` so users must supply
  an explicit path. Updated all examples to write to `tempfile()` rather
  than a bare filename. Fixed the `file = TRUE` fallback in the internal
  `write_refs()` helper to write to `tempdir()` instead of `getwd()`.

---

## R CMD check results

0 errors | 0 warnings | 1 note

Tested on:
- Windows 11, R 4.5.0: 0 errors | 0 warnings | 1 note

The single note is "unable to verify current time", a transient network
issue on the checking machine unrelated to the package.

## Reverse dependencies

None — this is a new submission with no downstream dependents.

## Notes

R/asys_dedup.R contains code vendored from the ASySD package
(GPL >= 3, CAMARADES Group / Kaitlyn Hair) with attribution in
the file header. CiteSource is also GPL >= 3, so licenses are compatible.
