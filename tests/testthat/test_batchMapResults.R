context("batchMapResults")

test_that("batchMapResults", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:3)
  submitJobs(reg)
  waitForJobs(reg)

  td = getTempDir()
  reg2 = makeRegistry(id="foo", file.dir = td)
  batchMapResults(reg, reg2, fun=function(job, res, y, z) (res+y)*z, 4:6, more.args=list(z=2))
  submitJobs(reg2)
  waitForJobs(reg2)
  expect_equal(loadResults(reg2, use.names="none", simplify=TRUE), ((1:3)^2 + (4:6))*2)

  td = getTempDir()
  reg2 = makeRegistry(id="foo", file.dir = td)
  ids = getJobIds(reg)
  batchMapResults(reg, reg2, function(job, res, y, z) (res+y)*z, 5:6, ids=ids[2:3], more.args=list(z=2))
  submitJobs(reg2)
  waitForJobs(reg2)
  expect_equal(loadResults(reg2, use.names="none", simplify=TRUE), ((2:3)^2 + (5:6))*2)

  td = getTempDir()
  reg2 = makeRegistry(id="foo", file.dir = td)
  batchMapResults(reg, reg2, function(job, res, ...) res, ids = integer(0L))
})
