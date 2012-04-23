context("batchMapReduce")

test_that("batchMapReduce", {
  reg = makeTestRegistry()
  ids = batchReduce(reg, function(aggr, x) aggr+x, 1:20, init=0, block.size=10)
  submitJobs(reg)
  y = reduceResults(reg, fun=function(aggr, job, res) aggr+res, init=0)
  expect_equal(y, sum(1:20))
  expect_equal(ids, 1:2)

  reg = makeTestRegistry()
  expect_equal(batchReduce(reg, function(aggr, x) aggr+x, integer(0L), init=0, block.size=10), integer(0L))
})
