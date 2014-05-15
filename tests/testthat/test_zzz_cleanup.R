context("cleanup")

test_that("Removing unittest files", {
  expect_true(cleanup())
})
