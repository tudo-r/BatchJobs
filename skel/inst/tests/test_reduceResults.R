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
  expect_error(
    reduceResults(reg, fun=function(aggr, job, res) 1),
    "No jobs with corresponding ids finished"  
  )  
})