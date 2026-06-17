# Tests for dedup_citations_add_sources(): adding new citations to a
# previously deduplicated set (Goal 2 — incremental deduplication).

# Build a tiny raw set with controllable duplicates across sources.
make_raw <- function() {
  base <- data.frame(
    title    = c("Alpha study of fish", "Beta review of coral", "Gamma trial of kelp",
                 "Delta survey of crabs", "Epsilon report on eels"),
    author   = c("Smith J", "Jones A", "Lee K", "Brown R", "Davis M"),
    year     = c("2010", "2011", "2012", "2013", "2014"),
    journal  = c("Mar Biol", "Coral J", "Kelp Sci", "Crab Rev", "Eel Rep"),
    abstract = c("aaa fish abundance", "bbb coral cover", "ccc kelp density",
                 "ddd crab counts", "eee eel migration"),
    doi      = c("10.1/a", "10.1/b", "10.1/c", "10.1/d", "10.1/e"),
    pages    = c("1-10", "11-20", "21-30", "31-40", "41-50"),
    volume   = c("1", "2", "3", "4", "5"),
    number   = c("1", "1", "1", "1", "1"),
    isbn     = c("111", "222", "333", "444", "555"),
    stringsAsFactors = FALSE
  )
  base
}

test_that("add_sources errors without a duplicate_id column", {
  raw <- make_raw()
  expect_error(
    dedup_citations_add_sources(raw, raw),
    "duplicate_id"
  )
})

test_that("incremental dedup matches from-scratch and preserves provenance", {
  # Source A = records 1-4, Source B = records 3-5 (records 3,4 overlap A/B).
  a <- make_raw()[1:4, ]; a$cite_source <- "A"; a$cite_label <- ""; a$cite_string <- ""
  b <- make_raw()[3:5, ]; b$cite_source <- "B"; b$cite_label <- ""; b$cite_string <- ""

  existing <- suppressWarnings(suppressMessages(dedup_citations(a)))
  combined <- suppressWarnings(suppressMessages(dedup_citations_add_sources(existing, b)))
  scratch  <- suppressWarnings(suppressMessages(
    dedup_citations(dplyr::bind_rows(a, b))
  ))

  # Same number of unique records as deduping everything at once
  expect_equal(nrow(combined), nrow(scratch))

  # All five distinct titles are represented
  expect_setequal(unique(combined$title), unique(make_raw()$title))

  # Both sources present after the merge
  srcs <- unique(trimws(unlist(strsplit(paste(combined$cite_source, collapse = ", "), ",\\s*"))))
  expect_true(all(c("A", "B") %in% srcs))

  # Output is reimport-shaped
  expect_true(all(c("duplicate_id", "record_ids", "cite_source") %in% names(combined)))
})

test_that("works on a reimported (all-character) existing set and in manual mode", {
  a <- make_raw()[1:4, ]; a$cite_source <- "A"; a$cite_label <- ""; a$cite_string <- ""
  b <- make_raw()[3:5, ]; b$cite_source <- "B"; b$cite_label <- ""; b$cite_string <- ""

  existing <- suppressWarnings(suppressMessages(dedup_citations(a)))
  f <- tempfile(fileext = ".csv")
  export_csv(existing, f)
  existing_re <- reimport_csv(f)

  res <- suppressWarnings(suppressMessages(
    dedup_citations_add_sources(existing_re, b, manual = TRUE)
  ))
  expect_type(res, "list")
  expect_true(all(c("unique", "manual_dedup", "auto_pairs") %in% names(res)))
  expect_true(all(c("duplicate_id", "record_ids") %in% names(res$unique)))

  # Candidate pairs reference duplicate_ids present in the unique output, so the
  # set is ready for dedup_citations_add_manual() / the Shiny manual review tab.
  if (nrow(res$manual_dedup) > 0) {
    ids <- as.character(res$unique$duplicate_id)
    expect_true(all(as.character(res$manual_dedup$duplicate_id.x) %in% ids))
    expect_true(all(as.character(res$manual_dedup$duplicate_id.y) %in% ids))
  }
})
