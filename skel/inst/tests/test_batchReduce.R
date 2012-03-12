context("batchMapReduce")

test_that("batchMapReduce", {  
  reg = makeTestRegistry()
  batchReduce(reg, function(aggr, x) aggr+x, 1:20, init=0, block.size=10)
  submitJobs(reg)
  y = reduceResults(reg, fun=function(aggr, job, res) aggr+res, init=0)
  expect_equal(y, sum(1:20))
})