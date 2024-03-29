
test_that("ris import works", {
  x <- read_citations(testthat::test_path("data", "1_WoS.ris"))
  expect_equal(nrow(x), 219)
  expect_equal(x$cite_source[1], "1_WoS")
})

litsearchr <- c(
  "@article{grames2019,
  title={An automated approach to identifying search terms for
  systematic reviews using keyword co-occurrence networks},
  author={Grames, Eliza M and Stillman, Andrew N and Tingley, Morgan W and Elphick, Chris S},
  journal={Methods in Ecology and Evolution},
  volume={10},
  number={10},
  pages={1645--1654},
  year={2019},
  publisher={Wiley Online Library}
}"
)

tmp <- tempfile()

writeLines(litsearchr, tmp)


test_that("bib import works", {
  x <- read_citations(tmp, cite_sources = "A", cite_strings = "B", cite_labels = "C")
  expect_equal(nrow(x), 1)
  expect_equal(x$cite_label, "C")
})
