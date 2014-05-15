context("filterResults")

test_that("filterResults", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 1:5)
  submitJobs(reg)
  waitForJobs(reg)
  ids = filterResults(reg, fun=function(job, res) res > 3)
  expect_equal(ids, c(4, 5))
  ids = filterResults(reg, getJobIds(reg)[2:4], fun=function(job, res) res > 3)
  expect_equal(ids, c(4))
})


test_that("filterResults works with empty results", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 1)
  ids = filterResults(reg, fun=function(job, res) TRUE)
  expect_equal(ids, integer(0))
})
