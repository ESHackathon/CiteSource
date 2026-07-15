# Tests for document-type handling across import, deduplication and export.

# Two duplicate clusters, each represented once per source, so ASySD merges the
# two rows in every cluster. Cluster 1 shares a document type; cluster 2 does not.
make_typed_raw <- function(type_a, type_b) {
  one <- function(src, ty) {
    data.frame(
      title    = c("Alpha study of fish", "Beta review of coral"),
      author   = c("Smith J", "Jones A"),
      year     = c("2010", "2011"),
      journal  = c("Mar Biol", "Coral J"),
      abstract = c("aaa fish abundance", "bbb coral cover"),
      doi      = c("10.1/a", "10.1/b"),
      pages    = c("1-10", "11-20"),
      volume   = c("1", "2"),
      number   = c("1", "1"),
      isbn     = c("111", "222"),
      type     = ty,
      cite_source = src,
      cite_label  = "",
      cite_string = "",
      stringsAsFactors = FALSE
    )
  }
  # Source A gives cluster1 = type_a[1], cluster2 = type_a[2]; Source B likewise.
  rbind(
    one("A", c(type_a[1], type_a[2])),
    one("B", c(type_b[1], type_b[2]))
  )
}

test_that("read_citations standardises RIS TY into a `type` column", {
  x <- read_citations(testthat::test_path("data", "1_WoS.ris"))
  expect_true("type" %in% names(x))
  expect_false("source_type" %in% names(x))
  expect_true(any(!is.na(x$type)))
})

test_that("read_citations retains the BibTeX entry type", {
  bib <- "@article{grames2019,
    title={An automated approach},
    author={Grames, Eliza M and Elphick, Chris S},
    journal={Methods in Ecology and Evolution},
    year={2019}
  }"
  tmp <- tempfile(fileext = ".bib")
  writeLines(bib, tmp)
  x <- read_citations(tmp, cite_sources = "A")
  expect_true("type" %in% names(x))
  expect_equal(tolower(x$type[1]), "article")
})

test_that("auto dedup keeps a shared type and uses GEN when types differ", {
  raw <- make_typed_raw(type_a = c("JOUR", "JOUR"),
                        type_b = c("JOUR", "BOOK"))
  dd <- suppressWarnings(suppressMessages(dedup_citations(raw)))

  expect_true("type" %in% names(dd))
  alpha <- dd$type[grepl("Alpha", dd$title, ignore.case = TRUE)]
  beta  <- dd$type[grepl("Beta",  dd$title, ignore.case = TRUE)]

  # Cluster 1: both JOUR -> JOUR retained
  expect_equal(unique(alpha), "JOUR")
  # Cluster 2: JOUR vs BOOK -> GEN
  expect_equal(unique(beta), "GEN")
})

test_that("shared type comparison is case-insensitive", {
  raw <- make_typed_raw(type_a = c("jour", "JOUR"),
                        type_b = c("JOUR", "jour"))
  dd <- suppressWarnings(suppressMessages(dedup_citations(raw)))
  alpha <- dd$type[grepl("Alpha", dd$title, ignore.case = TRUE)]
  beta  <- dd$type[grepl("Beta",  dd$title, ignore.case = TRUE)]
  # All map to the same type ignoring case -> not GEN
  expect_false("GEN" %in% c(alpha, beta))
})

test_that("manual candidate pairs expose type1/type2 and a type_keep column", {
  raw <- make_typed_raw(type_a = c("JOUR", "JOUR"),
                        type_b = c("JOUR", "BOOK"))
  # Loosen matching so a pair is only *maybe* a duplicate: tweak one abstract.
  raw$abstract[raw$cite_source == "B"] <- c("aaa fish abundance slightly changed",
                                            "bbb coral cover slightly changed")
  res <- suppressWarnings(suppressMessages(dedup_citations(raw, manual = TRUE)))
  if (nrow(res$manual_dedup) > 0) {
    expect_true(all(c("type1", "type2", "type_keep") %in% names(res$manual_dedup)))
  } else {
    succeed("No manual candidate pairs produced for this dataset")
  }
})

test_that("dedup_citations_add_manual honors type_keep", {
  raw <- make_typed_raw(type_a = c("JOUR", "JOUR"),
                        type_b = c("JOUR", "BOOK"))
  auto <- suppressWarnings(suppressMessages(dedup_citations(raw)))

  ids <- as.character(auto$duplicate_id)
  pair <- data.frame(
    duplicate_id.x = ids[1], duplicate_id.y = ids[2],
    result = "match", type_keep = "RPRT",
    stringsAsFactors = FALSE
  )
  final <- suppressWarnings(suppressMessages(dedup_citations_add_manual(auto, pair)))
  merged_row <- final$type[grepl(ids[1], final$record_ids) | grepl(ids[2], final$record_ids)]
  expect_true("RPRT" %in% merged_row)
})

test_that("type round-trips through RIS export and reimport", {
  raw <- make_typed_raw(type_a = c("JOUR", "JOUR"),
                        type_b = c("JOUR", "BOOK"))
  dd <- suppressWarnings(suppressMessages(dedup_citations(raw)))

  f <- tempfile(fileext = ".ris")
  export_ris(dd, f)
  re <- suppressWarnings(suppressMessages(reimport_ris(f)))

  expect_true("type" %in% names(re))
  expect_false("source_type" %in% names(re))
  expect_setequal(stats::na.omit(unique(re$type)), stats::na.omit(unique(dd$type)))
})

test_that("type round-trips through CSV export and reimport", {
  raw <- make_typed_raw(type_a = c("JOUR", "JOUR"),
                        type_b = c("JOUR", "BOOK"))
  dd <- suppressWarnings(suppressMessages(dedup_citations(raw)))

  f <- tempfile(fileext = ".csv")
  export_csv(dd, f)
  re <- reimport_csv(f)

  expect_true("type" %in% names(re))
  expect_setequal(unique(re$type), unique(dd$type))
})
