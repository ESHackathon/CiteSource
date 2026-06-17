# Tests for the deferred manual-deduplication workflow:
# auto-dedup now -> export -> reimport -> complete manual review later.

example_citations <- function() {
  readRDS(system.file("extdata", "examplecitations.rds", package = "CiteSource"))
}

test_that("reimport_csv round-trips all columns as character", {
  auto <- suppressWarnings(suppressMessages(dedup_citations(example_citations())))
  f <- tempfile(fileext = ".csv")
  export_csv(auto, f)
  re <- reimport_csv(f)

  expect_true(all(c("cite_source", "cite_label", "cite_string",
                    "duplicate_id", "record_ids") %in% names(re)))
  # duplicate_id must stay character (read.csv would otherwise infer integer),
  # which is what dedup_citations_add_manual() / re-dedup require.
  expect_type(re$duplicate_id, "character")
  expect_type(re$year, "character")
})

test_that("export_csv writes the manual_dedup_complete flag on full exports", {
  auto <- suppressWarnings(suppressMessages(dedup_citations(example_citations())))

  f1 <- tempfile(fileext = ".csv")
  export_csv(auto, f1)                                   # default FALSE
  expect_equal(unique(reimport_csv(f1)$manual_dedup_complete), "FALSE")

  f2 <- tempfile(fileext = ".csv")
  export_csv(auto, f2, manual_dedup_complete = TRUE)
  expect_equal(unique(reimport_csv(f2)$manual_dedup_complete), "TRUE")

  # Not written for non-reimportable (standard) exports
  f3 <- tempfile(fileext = ".csv")
  suppressWarnings(export_csv(auto, f3, fields = "standard"))
  expect_false("manual_dedup_complete" %in% names(utils::read.csv(f3)))
})

test_that("candidate pairs round-trip and seed a result column", {
  pairs <- data.frame(
    duplicate_id.x = c("1001", "1005"),
    duplicate_id.y = c("1002", "1009"),
    title1 = c("A", "B"), title2 = c("A", "B"),
    stringsAsFactors = FALSE
  )
  f <- tempfile(fileext = ".csv")
  export_dedup_candidates(pairs, f)
  back <- reimport_dedup_candidates(f)

  expect_true(all(c("duplicate_id.x", "duplicate_id.y", "result") %in% names(back)))
  expect_type(back$duplicate_id.x, "character")
  expect_equal(nrow(back), 2)

  expect_error(export_dedup_candidates(data.frame(a = 1), f), "duplicate_id")
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1), bad, row.names = FALSE)
  expect_error(reimport_dedup_candidates(bad), "duplicate_id")
})

test_that("manual pairs can be merged into a reimported (auto-deduped) set", {
  auto <- suppressWarnings(suppressMessages(dedup_citations(example_citations())))
  f <- tempfile(fileext = ".csv")
  export_csv(auto, f)
  re <- reimport_csv(f)

  # Force two real records to be a confirmed manual duplicate
  ids <- as.character(re$duplicate_id)
  pair <- data.frame(duplicate_id.x = ids[1], duplicate_id.y = ids[2],
                     result = "match", stringsAsFactors = FALSE)

  final <- suppressWarnings(suppressMessages(dedup_citations_add_manual(re, pair)))
  expect_equal(nrow(final), nrow(re) - 1)
  expect_true(all(c("cite_source", "record_ids", "duplicate_id") %in% names(final)))
})
