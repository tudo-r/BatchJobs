context("doJob")

if (interactive()) {

test_that("doJob", {  
  reg = makeTestRegistry()
  f = function(x) x
  id = 1L
  job = makeJob(id=id, fun=f, pars=list(x=123), seed=reg$seed)
  addJob(reg, job)  
  df = dbGetJobStatusTable(reg)
  expect_true(is.data.frame(df) && nrow(df) == 1 && ncol(df) == 11)
  ids = findMissingResults(reg)
  expect_equal(ids, id)
  expect_output({ 
    y = doSingleJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)
  }, "BatchJobs job")
  expect_equal(y, 123)
  df = dbGetJobStatusTable(reg)
  expect_true(!is.na(df$started) && !is.na(df$done) && is.na(df$error))
  y = loadResult(reg, id)
  expect_equal(y, 123)
  ids = findMissingResults(reg)
  expect_equal(length(ids), 0)
  
  # test working directory
  reg = makeTestRegistry()
  wd.now = getwd()
  wd.job = tempdir()
  bar = 123
  save(bar=bar, file=file.path(wd.job, "foo.RData"))
  reg$work.dir = wd.job
  f = function(x) {
    load("foo.RData")
    bar+x    
  }
  job = makeJob(id=id, fun=f, pars=list(x=1), seed=reg$seed)
  addJob(reg, job)#
  expect_output({ 
    y = doSingleJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)
  }, "BatchJobs job")
  expect_equal(y, bar + 1)
  expect_equal(getwd(), wd.now)
  
  # test packages
  reg = makeTestRegistry(packages=c("foo"))
  job = makeJob(id=id, fun=f, pars=list(x=1), seed=reg$seed)
  addJob(reg, job)  
  expect_error(suppressAll(doSingleJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)), 
    "Could not load required package 'foo' on node!")  
  expect_equal(findMissingResults(reg), id)
  reg = makeTestRegistry(packages=c("randomForest"))
  f = function() randomForest(Species~., data=iris)
  job = makeJob(id, fun=f, pars=list(), seed=reg$seed)
  addJob(reg, job)  
  expect_output({ 
    y = doSingleJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)
  }, "BatchJobs job")
  expect_equal(length(findMissingResults(reg)), 0)
})

}