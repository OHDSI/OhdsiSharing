library("testthat")


# a test that will fail
test_that("A doomed test", {
  expect_true(FALSE)
})