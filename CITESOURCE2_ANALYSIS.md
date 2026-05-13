# CiteSource 2 — Analysis & Work Log

## Codebase Overview

**Package version:** 0.1.1 (Date: 2023-06-22)  
**R version required:** \>= 3.5.0 (but examples use `|>` which requires
R \>= 4.1.0)  
**Primary dependency:** ASySD (GitHub only — not on CRAN)

### File Map

| File | Purpose |
|----|----|
| `R/CiteSource.R` | Package docs, pipe export, `.onLoad`, `key_fields` vector |
| `R/import.R` | [`read_citations()`](https://www.eshackathon.org/CiteSource/reference/read_citations.md) — main import |
| `R/import_export_helpers.R` | Vendored synthesisr parser: `synthesisr_read_refs`, `parse_ris`, `parse_bibtex`, `write_ris`, `write_bib`, etc. |
| `R/dedup.R` | [`dedup_citations()`](https://www.eshackathon.org/CiteSource/reference/dedup_citations.md), [`dedup_citations_add_manual()`](https://www.eshackathon.org/CiteSource/reference/dedup_citations_add_manual.md), `add_cols()` |
| `R/compare.R` | [`count_unique()`](https://www.eshackathon.org/CiteSource/reference/count_unique.md), [`compare_sources()`](https://www.eshackathon.org/CiteSource/reference/compare_sources.md) |
| `R/count.R` | **Gen 1 (old)**: [`record_counts()`](https://www.eshackathon.org/CiteSource/reference/record_counts.md), [`calculate_record_counts()`](https://www.eshackathon.org/CiteSource/reference/calculate_record_counts.md), [`calculate_phase_count()`](https://www.eshackathon.org/CiteSource/reference/calculate_phase_count.md) |
| `R/new_count_and_table.R` | **Gen 2 (current app)**: [`calculate_initial_records()`](https://www.eshackathon.org/CiteSource/reference/calculate_initial_records.md), [`calculate_detailed_records()`](https://www.eshackathon.org/CiteSource/reference/calculate_detailed_records.md), [`calculate_phase_records()`](https://www.eshackathon.org/CiteSource/reference/calculate_phase_records.md), `create_*_table()` functions |
| `R/tables.R` | [`record_level_table()`](https://www.eshackathon.org/CiteSource/reference/record_level_table.md), [`citation_summary_table()`](https://www.eshackathon.org/CiteSource/reference/citation_summary_table.md), APA citation helpers, plus old `record_counts_table()`, `record_summary_table()`, `precision_sensitivity_table()` |
| `R/plots.R` | [`plot_source_overlap_heatmap()`](https://www.eshackathon.org/CiteSource/reference/plot_source_overlap_heatmap.md), [`plot_source_overlap_upset()`](https://www.eshackathon.org/CiteSource/reference/plot_source_overlap_upset.md), [`plot_contributions()`](https://www.eshackathon.org/CiteSource/reference/plot_contributions.md) |
| `R/export.R` | [`export_csv()`](https://www.eshackathon.org/CiteSource/reference/export_csv.md), [`export_ris()`](https://www.eshackathon.org/CiteSource/reference/export_ris.md), [`export_bib()`](https://www.eshackathon.org/CiteSource/reference/export_bib.md) |
| `R/reimport.R` | [`reimport_csv()`](https://www.eshackathon.org/CiteSource/reference/reimport_csv.md), [`reimport_ris()`](https://www.eshackathon.org/CiteSource/reference/reimport_ris.md) |
| `R/runShiny.R` | [`runShiny()`](https://www.eshackathon.org/CiteSource/reference/runShiny.md) / `run_shiny` alias |
| `R/helpers.R` | `ui_yeah()` (copied from usethis, MIT licensed) |
| `inst/shiny-app/CiteSource/app.R` | ~1890-line Shiny app |

### Three Generations of Count/Table Functions (all currently exported!)

- **Gen 1** (`count.R`): `record_counts`, `calculate_record_counts`,
  `calculate_phase_count` — require separate raw `citations` df; largely
  superseded
- **Gen 2** (`new_count_and_table.R`): `calculate_initial_records`,
  `calculate_detailed_records`, `calculate_phase_records` — work on
  `unique_citations` only; used by the Shiny app
- **Gen 3** (`tables.R`): `citation_summary_table`, `record_level_table`
  — most elegant, paper-aligned; also has old wrappers
  `record_counts_table`, `record_summary_table`,
  `precision_sensitivity_table`

------------------------------------------------------------------------

## Issues Found

### 🔴 CRAN Blockers (Must Fix)

1.  **`Remotes:` field in DESCRIPTION** — Not permitted on CRAN. Removes
    ability to install via
    [`install.packages()`](https://rdrr.io/r/utils/install.packages.html).

2.  **ASySD is not on CRAN** — `CiteSource` Imports `ASySD (>= 0.3.0)`
    from GitHub (`camaradesuk/ASySD`). This is a hard blocker.
    Options: (a) wait for ASySD CRAN submission, (b) vendor ASySD’s core
    dedup logic into CiteSource, (c) make ASySD a Suggests and degrade
    gracefully.

3.  **`plogr` from archived CRAN** —
    `Remotes: plogr=url::https://cran.r-project.org/src/contrib/Archive/plogr_0.2.0.tar.gz`
    — using an archived package. May not be needed at all (appears to be
    a transitive dep from a test setup).

4.  **`bslib` not in DESCRIPTION** — Used in `app.R`
    ([`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)),
    listed neither in Imports nor Suggests.

5.  **`import()` for whole packages in NAMESPACE** — `import(dplyr)`,
    `import(gt)`, `import(rlang)`, `import(scales)`, `import(tidyr)` —
    CRAN policy requires `importFrom()` for specific functions, not
    wholesale package imports (note-worthy at minimum, strict reviewers
    reject this).

6.  **R minimum version mismatch** — `R (>= 3.5.0)` declared but `|>`
    (native pipe) is used in examples in `tables.R` — native pipe
    requires R \>= 4.1.0.

7.  **`.onLoad` sets global options** — Setting `shiny.maxRequestSize`
    and `timeout` in `.onLoad` is a side effect that modifies the user’s
    environment. CRAN policy: “Packages should not modify global options
    or settings” except in clearly scoped ways. Should be moved to
    `app.R` only.

8.  **`export("%>%")` in NAMESPACE** — Exporting a re-exported pipe from
    another package is acceptable but adds clutter; if switching to R
    \>= 4.1.0 requirement, use native `|>` throughout and drop this.

### 🔴 Confirmed Bugs

9.  **[`calculate_phase_records()`](https://www.eshackathon.org/CiteSource/reference/calculate_phase_records.md)
    — wrong use of `n_distinct`** (new_count_and_table.R:319-330):

    ``` r

    total_screened <- unique_citations %>%
      tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
      dplyr::filter(cite_label == "screened") %>%
      dplyr::n_distinct(duplicate_id)  # BUG: n_distinct() expects a vector, not a piped df
    ```

    `dplyr::n_distinct(df, col_name)` where `col_name` is unquoted won’t
    evaluate correctly in this context — `duplicate_id` is not in scope
    outside the pipe. Should be:

    ``` r

    dplyr::summarise(n = dplyr::n_distinct(duplicate_id)) %>% dplyr::pull(n)
    ```

10. **[`calculate_phase_records()`](https://www.eshackathon.org/CiteSource/reference/calculate_phase_records.md)
    — `n_unique` not defined in app server scope** (app.R:1830):

    ``` r

    phase_counts <- calculate_phase_records(unique_citations, n_unique, "cite_source")
    ```

    `n_unique` is not defined as a plain variable here. `rv$n_unique` is
    defined at line 1459 as a reactive expression, and at line 837 as a
    data frame. Either way, bare `n_unique` is undefined — this will
    throw `"object 'n_unique' not found"` when the Precision/Sensitivity
    Table is generated. **This table almost certainly does not work.**

11. **[`calculate_phase_count()`](https://www.eshackathon.org/CiteSource/reference/calculate_phase_count.md)
    — wrong NSE for filter** (count.R:258):

    ``` r

    dplyr::filter(!(!!db_colname == "unknown"))
    ```

    `db_colname` is a character string (e.g. `"cite_source"`), so
    `!!db_colname` splices the string literal, giving
    `!("cite_source" == "unknown")` which is always `TRUE` — the filter
    does nothing. Should be
    `dplyr::filter(!(!!rlang::sym(db_colname) == "unknown"))`.

12. **[`calculate_phase_count()`](https://www.eshackathon.org/CiteSource/reference/calculate_phase_count.md)
    — `rbind` coerces types** (count.R:301):

    ``` r

    totals <- c("Total", nrow(unique_citations), ...)
    combined_counts <- rbind(combined_counts, totals)
    ```

    [`c()`](https://rdrr.io/r/base/c.html) coerces all values to
    character, then `rbind` coerces all numeric columns to character.
    Downstream code receiving this data frame will break on any numeric
    operation.

13. **`generate_apa_reference()` — NA DOI not handled** (tables.R:584):

    ``` r

    doi = dplyr::if_else(stringr::str_detect(doi, "http"), doi, paste0("https://doi.org/", doi))
    ```

    `str_detect(NA, "http")` returns `NA`. `if_else` with an NA
    condition returns NA for the whole link — breaks reference rendering
    for records without DOIs.

14. **`columns2hide` used as DT column targets** (app.R:975):

    ``` r

    list(visible = FALSE, targets = columns2hide)
    ```

    `columns2hide` is defined as a character vector of column names
    (`c("title", "author", ...)`), but DT’s `columnDefs` `targets`
    requires 0-based integer indices, not column names. Silent failure.

15. **`separate_rows(cite_source, sep = ",")` missing whitespace**
    (new_count_and_table.R): Some uses of `separate_rows` use
    `sep = ","` (no whitespace) while elsewhere `sep = ",\\s*"` is used.
    Inconsistent separator handling will leave leading spaces in source
    names, causing phantom duplicate entries (e.g. `"WoS"` vs `" WoS"`).

### 🟠 Code Quality / Logic Issues

16. **Duplicate/redundant exported functions** — Three generations of
    count functions are all exported (`record_counts`,
    `calculate_record_counts`, `calculate_phase_count`,
    `calculate_initial_records`, `calculate_detailed_records`,
    `calculate_phase_records`). Only Gen 2 is used in the app. Gen 1 and
    old table wrappers (`record_counts_table`, `record_summary_table`)
    should be deprecated or removed. Keeping all three confuses API
    users.

17. **[`count_unique()`](https://www.eshackathon.org/CiteSource/reference/count_unique.md)
    — NA handling** (compare.R:21):

    ``` r

    dplyr::filter(!.data$cite_source == "")
    ```

    Doesn’t explicitly handle NAs. Should be
    `filter(!is.na(cite_source) & cite_source != "")`.

18. **[`calculate_detailed_records()`](https://www.eshackathon.org/CiteSource/reference/calculate_detailed_records.md)
    hardcodes `cite_label == "search"`** (new_count_and_table.R:205):

    ``` r
    dplyr::filter(cite_label == "search") %>%
    ```

    If a user labels their initial results anything other than exactly
    `"search"`, this returns 0 unique records. This is a silent failure
    — no warning.

19. **`dedup.R` — `journal` may get filled with database name**: The
    coalesce `journal = coalesce(journal, source)` runs before `source`
    is overwritten with `cite_source`. For records where `journal` is
    missing but `source` (RIS “SO” field, which some DBs set to the
    database name rather than journal) is present, the journal column
    gets the database name. This corrupts journal data for certain
    databases.

20. **`generate_apa_citation` uses
    [`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)**
    — Performance scales very poorly with large citation sets (O(n)
    rowwise operations). Should be vectorized.

21. **`sapply` in visualization/table filter reactives**
    (app.R:1234-1274, 1605-1648) — Per-row string processing with
    `sapply` on `cite_source`/`cite_label`/`cite_string` is very slow
    for large datasets. Vectorized tidyverse approach would be much
    faster.

22. **`compare_sources` called twice per render** — Both `plotHeat` and
    `plotUpset` reactives independently call
    `compare_sources(data_vis, ...)`. Since they always use the same
    `comp_type` and same input data, this computation should be shared
    in a single reactive.

23. **`options(shiny.maxRequestSize=10000*1024^2)` at top of app.R** —
    Allows ~10 GB uploads. The `.onLoad` in CiteSource.R sets 2000 MB
    (interactive) or 250 MB (non-interactive). The app.R hardcoded value
    of 10 GB overrides this and is extreme.

24. **`only_key_fields = FALSE` in app.R upload** — The Shiny app
    imports all RIS fields for every upload. This is slower and produces
    wider data frames. The extra fields are needed so users can see them
    in the manual dedup table, but `key_fields` includes all the
    dedup-critical fields. Could import key fields + a configurable set
    of “display” fields.

25. **`manual_dedup_data()` hardcodes `[,1:36]`** (app.R:917):

    ``` r

    data <- rv$pairs_to_check[,1:36]
    ```

    Assumes ASySD always returns exactly 36+ columns. If the dedup
    output changes, this silently truncates or errors.

26. **`phase_plot` `bar_order` hardcoded** (app.R:1492):

    ``` r

    bar_order = c("search", "screened", "final")
    ```

    If users apply non-standard labels, the phase plot’s bar order
    breaks with no warning.

### 🟡 UX/UI Issues

27. **No workflow state persistence** — If a user navigates away from
    the app or refreshes, all work is lost. The export/reimport flow
    partially addresses this, but there’s no session autosave.

28. **Visualization filters (sources/labels/strings) all visible even
    when `comp_type` is “sources”** — When comparing by sources, the
    label and string filters are not relevant for the heatmap/upset but
    are still shown, causing confusion.

29. **`comp_type` drives comparison but “Phase Analysis” tab always
    shows source × label regardless** — The Phase Analysis plot
    (`plot_contributions`) always uses source as facets and label as
    bars, but the `comp_type` selector doesn’t apply to it. Users may
    not realize the selector doesn’t affect the phase tab.

30. **No inline feedback when dedup produces 0 manual pairs** — The
    success message tells users there are 0 pairs, but the manual dedup
    tab still shows an empty table with the “Remove additional
    duplicates” button (if rows are selected). Could auto-advance to “Go
    to visualizations” state.

31. **Export tab: no filter before export** — Users can only export the
    full deduplicated set. No way to export a filtered subset (e.g. only
    “final” labeled records).

32. **“Review individual records” performance warning but no cap** — The
    app warns about performance for 300+ records but doesn’t prevent
    generation, and the underlying `generate_apa_citation` with
    `rowwise()` is the bottleneck.

33. **Reimport flow bypasses dedup UI entirely** — When reimporting a
    CiteSource CSV/RIS, the dedup tab is skipped entirely (correctly)
    but the UI still shows the dedup step as “Step 3” with no indication
    it’s not needed. A reimport should indicate this clearly.

34. **Google Analytics injection in app.R** — Environment-specific GA
    code based on URL path is deployment infrastructure, not package
    code. It should not be in a CRAN-distributed package.

### 🟡 NAMESPACE / API Cleanliness

35. **`run_shiny` and `runShiny` both exported** — `run_shiny` is just
    an alias. Only one needs to be in the documented public API; the
    other can be internal or mentioned as an alias.

36. **`as.data.frame.bibliography` exported as S3 method** — This is
    correct; it just needs to be checked that the generic is properly
    imported.

37. **`record_counts_table` uses `.data$` in
    [`gt::cells_column_labels`](https://gt.rstudio.com/reference/cells_column_labels.html)**
    (tables.R:636) — `gt` functions don’t use `.data$` pronoun; this
    should be a bare column name or string. Potential bug/NOTE in R CMD
    check.

38. **`@import dplyr`, `@import tidyr` etc. in roxygen** — These add
    whole-package imports. Better to add `@importFrom` for specific
    functions to reduce namespace pollution.

------------------------------------------------------------------------

## Questions for Trevor (Before Proceeding)

See main conversation for questions.

------------------------------------------------------------------------

## Work Log

| Date | Action |
|----|----|
| 2026-05-11 | Cloned repo into CiteSource 2; full read of all R source files and app.R; compiled analysis |
