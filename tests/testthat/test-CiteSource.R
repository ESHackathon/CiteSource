# Test only included to initialise unit testing - can be deleted later
test_that("pipe works", {
  expect_equal(abs(-2), -2 %>% abs())
})
