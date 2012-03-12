context("batchMapResults")

test_that("batchMapResults", {  
  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:3)
  submitJobs(reg)

  td = file.path(tempdir(), "foo")
  unlink(td, recursive=TRUE)  
  reg2 = makeRegistry(file.dir = td)
  batchMapResults(reg, reg2, fun=function(job, res, y, z) (res+y)*z, 4:6, more.args=list(z=2))
  submitJobs(reg2)
  y = sapply(getJobIds(reg2), function(id) loadResult(reg2, id))
  expect_equal(y, ((1:3)^2 + (4:6))*2, check.attributes=FALSE)
  
  unlink(td, recursive=TRUE)  
  reg2 = makeRegistry(file.dir = td)
  ids = getJobIds(reg)
  batchMapResults(reg, reg2, function(job, res, y, z) (res+y)*z, 5:6, ids=ids[2:3], more.args=list(z=2))
  submitJobs(reg2)
  y = sapply(getJobIds(reg2), function(id) loadResult(reg2, id))
  expect_equal(y, ((2:3)^2 + (5:6))*2, check.attributes=FALSE)
})
