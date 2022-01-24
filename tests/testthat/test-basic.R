context("basic")

test_that("is_installed works", {
  expect_true(is_installed("base"))
  expect_false(is_installed("base", version = 100))
  expect_false(is_installed("blah"))
})

test_that("ensure_installed works", {
  if (!interactive()) {
    expect_error(ensure_installed("blah"), "must be installed")
  }
  expect_null(ensure_installed("base"))
})
