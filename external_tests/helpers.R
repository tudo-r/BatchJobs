library(testthat)

doExternalTest = function(whitespace=FALSE, long=FALSE) {
  id = "external_test"
  if (whitespace)
    fd = "foo bär"
  else
    fd = "foo"  
  ok = unlink(fd, recursive=TRUE)
  if (ok != 0)
    stopf("could not delete file dir: %s", fd)
  reg = makeRegistry(id=id, file.dir=fd, sharding=FALSE)
  xs = 1:5
  if (long)
    f = function(x) {Sys.sleep(300);x}
  else 
    f = function(x) x
  batchMap(reg, f, xs)
  submitJobs(reg)
  if (!long) {
    Sys.sleep(5)
    res = reduceResults(reg, fun=function(aggr,job,res) c(aggr, res))
    expect_equal(res, xs)
  }
  return(reg)
}

doKillTest = function() {
  reg = doExternalTest(whitespace=FALSE, long=TRUE)
  ids = getJobIds(reg)
  killJobs(reg, ids)
  expect_equal(findMissingResults(reg), ids)
  expect_equal(findOnSystem(reg), integer(0))
  expect_equal(findRunning(reg), integer(0))
}