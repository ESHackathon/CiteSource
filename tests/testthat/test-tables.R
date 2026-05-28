library(dplyr)
library(CiteSource)

authors <- c('Mill, John Stuart and Shelley, Mary and Lovelave, Eda and Hemingway, Ernest and Garcia Marquez, Gabriel',
             'Miller, Arthur and Snow, John',
             'Woolf, Virginia',
             'Miller, Arthur and Snow, John',
             'Mill, John Stuart and Shelley, Mary and Eliot, TS',
             'Woolf, Walter',
             'Mill, Arthur and Shelley, Mary and Eliot, TS',
             'Mill, Arthur and Shelley, Mary and Eliot, TS')

years <- c(rep(1900, 7), 1901)

test_that("disambiguated citations work", {
expect_equal(generate_apa_citation(authors, years),
             c("J. S. Mill, Shelley, Lovelave et al. (1900)", 
               "Miller & Snow (1900a)", "V. Woolf (1900)", 
               "Miller & Snow (1900b)", "J. S. Mill, Shelley & Eliot (1900)", 
               "W. Woolf (1900)", "A. Mill et al. (1900)", "A. Mill et al. (1901)")
  
)
  })



test_that("missing columns do not fail", {
  expect_warning(generate_apa_reference(LETTERS[1:5]))
})

test_that("numeric columns do not fail", {
    expect_warning(generate_apa_reference(paste(LETTERS[1:5], LETTERS[1:5], sep = ", "),  2000:2004))
   })

test_that("record_level_table names indicator columns for sources and labels", {
  cites <- tibble::tibble(
    duplicate_id = c(1L, 2L, 3L),
    cite_source = c("BM", "WoS", "BM, WoS"),
    cite_label = "search",
    author = rep("Smith, John", 3),
    year = 2000L,
    title = rep("A title", 3),
    journal = rep("A journal", 3)
  )
  tbl <- record_level_table(cites, include = c("sources", "labels"), return = "tibble")
  expect_true("BM" %in% names(tbl))
  expect_true("search" %in% names(tbl))
  expect_equal(anyDuplicated(names(tbl)[-(1:3)]), 0L)
  expect_equal(sum(tbl$BM), 2L)
  expect_equal(sum(tbl$search), 3L)
})
    
