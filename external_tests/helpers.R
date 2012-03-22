library(testthat)

doExternalTest = function(whitespace=FALSE) {
  id = "external_test"
  if (whitespace)
    fd = "foo bär"
  else
    fd = "foo"  
  unlink(fd, recursive=TRUE)
  reg = makeRegistry(id=id, file.dir=fd, sharding=FALSE)
  xs = 1:5
  batchMap(reg, identity, xs)
  submitJobs(reg)
  Sys.sleep(5)
  res = reduceResults(reg, fun=function(aggr,job,res) c(aggr, res))
  expect_equal(res, xs)
}