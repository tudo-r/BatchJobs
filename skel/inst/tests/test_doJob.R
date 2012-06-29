context("doJob")

if (interactive()) {

test_that("doJob", {  
  reg = makeTestRegistry()
  id = 1L
  batchMap(reg, identity, 123)
  df = dbGetJobStatusTable(reg)
  expect_true(is.data.frame(df) && nrow(df) == 1 && ncol(df) == 12)
  ids = findMissingResults(reg)
  expect_equal(ids, id)
  saveConf(reg)
  expect_output({ 
    y = doJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)
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
  batchMap(reg, f, 1)
  saveConf(reg)
  expect_output({ 
    y = doJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)
  }, "BatchJobs job")
  expect_equal(y, bar + 1)
  expect_equal(getwd(), wd.now)
  
  # test packages
  # be sneaky otherwise we get error here due to pack check
  reg = makeTestRegistry()
  reg$packages = list(foo="foo")
  saveRegistry(reg)
  batchMap(reg, identity, 1)
  saveConf(reg)
  expect_error(suppressAll(doJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)), 
    "Could not load required package 'foo' on node")  
  expect_equal(findMissingResults(reg), id)
  reg = makeTestRegistry(packages=c("randomForest"))
  f = function(i) randomForest(Species~., data=iris)
  batchMap(reg, f, 1)
  saveConf(reg)
  expect_output({ 
    y = doJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id)
  }, "BatchJobs job")
  expect_equal(length(findMissingResults(reg)), 0)
})

}