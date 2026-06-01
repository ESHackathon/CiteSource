## Submission

This is a minor feature update (0.2.0 -> 0.2.1).

New in this version:

* `dedup_citations_add_sources()` adds new citations to a previously
  deduplicated set and deduplicates across both, preserving prior merge
  decisions and record provenance (incremental deduplication).
* `export_dedup_candidates()` / `reimport_dedup_candidates()`, and a
  `manual_dedup_complete` flag in `export_csv()`, support performing automatic
  deduplication now and completing manual review later.
* `reimport_csv()` now reads all columns as character so a re-imported set can
  re-enter the manual-merge / incremental-deduplication functions without
  column-type clashes.
* Documentation and bundled 'shiny' application updates.

## Test environments

* Local: Windows 11, R 4.5.0
* win-builder: R-release and R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Trevor Riley <tnriley@gmail.com>'

  The "New submission" wording appears only because the previous version is not
  yet published on CRAN. Any flagged URLs (e.g. the GNU GPL license pages) are
  valid and resolve in a browser; the check machine reported transient
  connection resets.

## Reverse dependencies

None - CiteSource has no downstream dependents on CRAN.

## Notes

R/asys_dedup.R contains code vendored from the ASySD package
(GPL >= 3, CAMARADES Group / Kaitlyn Hair) with attribution in the file
header. CiteSource is also GPL >= 3, so the licenses are compatible.
