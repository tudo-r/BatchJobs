context("doJob")
test_that("doJob", {
  reg = makeTestRegistry()
  id = 1L
  batchMap(reg, identity, 123)
  df = BatchJobs:::dbGetJobStatusTable(reg)
  expect_true(is.data.frame(df) && nrow(df) == 1 && ncol(df) == 13)
  ids = findNotDone(reg)
  expect_equal(ids, id)
  BatchJobs:::saveConf(reg)
  expect_output({
    y = BatchJobs:::doJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id, array.id=NA_integer_)
  }, "BatchJobs job")
  waitForJobs(reg)
  expect_equal(y, TRUE)
  expect_equal(loadResult(reg, 1L), 123)
  df = BatchJobs:::dbGetJobStatusTable(reg)
  expect_true(!is.na(df$started) && !is.na(df$done) && is.na(df$error))
  y = loadResult(reg, id)
  expect_equal(y, 123)
  ids = findNotDone(reg)
  expect_equal(length(ids), 0)

  # test working directory
  reg = makeTestRegistry()
  wd.now = getwd()
  wd.job = reg$work.dir
  bar = 123
  save(bar=bar, file=file.path(wd.job, "foo.RData"))
  reg$work.dir = wd.job
  f = function(x) {
    load("foo.RData")
    bar+x
  }
  batchMap(reg, f, 1)
  BatchJobs:::saveConf(reg)
  expect_output({
    y = BatchJobs:::doJob(reg, id, multiple.result.files=FALSE, disable.mail=TRUE, last=id, array.id=NA_integer_)
  }, "BatchJobs job")
  expect_equal(y, TRUE)
  expect_equal(loadResult(reg, 1L), bar + 1)
  expect_equal(getwd(), wd.now)
  unlink(file.path(wd.job, "foo.RData"))

  # test packages
  # be sneaky otherwise we get error here due to pack check
  reg = makeTestRegistry()
  reg$packages = list(foo="foo")
  BatchJobs:::saveRegistry(reg)
  batchMap(reg, identity, 1)
  expect_error(suppressAll(testJob(reg, 1)), "Please install the following packages: foo")
  expect_equal(findNotDone(reg), id)

  if (isExpensiveExampleOk()) {
    reg = makeTestRegistry(packages=c("randomForest"))
    f = function(i) randomForest(Species~., data=iris)
    batchMap(reg, f, 1)
    submitJobs(reg)
    waitForJobs(reg)
    expect_equal(length(findNotDone(reg)), 0)
  }
})
