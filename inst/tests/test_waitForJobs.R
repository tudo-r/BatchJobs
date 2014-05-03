context("waitForJobs")

test_that("waitForJobs", {
  f = function(x) { if (x == 5) stop("x == 5") else x }
  reg = makeTestRegistry()
  batchMap(reg, f, 1:5)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(waitForJobs(reg, 1:5, stop.on.error = FALSE), FALSE)
  expect_warning(waitForJobs(reg, 1:5, stop.on.error = TRUE))
  expect_equal(suppressWarnings(waitForJobs(reg, 1:5, stop.on.error = TRUE)), FALSE)
  expect_equal(waitForJobs(reg, 1:4, stop.on.error=FALSE), TRUE)
  expect_equal(waitForJobs(reg, 1:4, stop.on.error=TRUE), TRUE)
})
