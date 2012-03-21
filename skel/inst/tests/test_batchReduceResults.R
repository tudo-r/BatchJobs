context("batchReduceResults")

test_that("batchReduceResults", {  
  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:5)
  submitJobs(reg)

  td = tempdir()
  unlink(td, recursive=TRUE)  
  reg2 = makeRegistry(id="foo", file.dir = td)
  batchReduceResults(reg, reg2, , block.size=3, init=c(),
    fun=function(aggr, job, res) c(aggr, res))
  submitJobs(reg2)
  expect_equal(loadResult(reg2, getJobIds(reg)[1]), (1:3)^2, check.names=FALSE)
  expect_equal(loadResult(reg2, getJobIds(reg)[2]), (4:5)^2, check.names=FALSE)
  
  unlink(td, recursive=TRUE)  
  reg2 = makeRegistry(id="foo", file.dir = td)
  ids = getJobIds(reg)
  batchReduceResults(reg, reg2, ids=ids[2:4], block.size=2, init=c(), 
    fun=function(aggr, job, res) c(aggr, res))
  submitJobs(reg2)
  expect_equal(loadResult(reg2, getJobIds(reg)[1]), (2:3)^2, check.names=FALSE)
  expect_equal(loadResult(reg2, getJobIds(reg)[2]), (4)^2, check.names=FALSE)
})
