context("reduceResults")

test_that("reduceResults", {
  reg = makeTestRegistry()
  batchMap(reg,  function(x) x^2, 1:3)
  submitJobs(reg)
  y = reduceResults(reg, fun=function(aggr, job, res) aggr+res, init=0L)
  expect_equal(y, sum((1:3)^2))
})


test_that("reduceResults works with empty results", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 1)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1), NULL)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, init=integer(0L)), integer(0L))
  submitJobs(reg)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, ids=integer(0L)), NULL)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, ids=integer(0L), init=integer(0L)), integer(0L))
})

test_that("reduceResults with multiple.result.files", {
  reg = makeTestRegistry(multiple.result.files=TRUE)
  batchMap(reg, function(x) list(foo = x, bar = 1), 1:10)
  submitJobs(reg)

  # quick check for named list as return value of loadResult
  expect_equal(loadResult(reg, 2)$foo, 2)
  expect_equal(loadResult(reg, 2, part="foo"), list(foo=2))
  expect_equal(loadResult(reg, 2, part="bar"), list(bar=1))

  # no part = all parts
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c()), 1:10)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c(), part = c("foo", "bar")), 1:10)

  # wrong part
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c(), part="bar"), NULL)
  # right part
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c(), part="foo"), 1:10)
})
