context("getJobInfo")

test_that("getJobInfo", {
  reg = makeTestRegistry()
  batchMap(reg, function(x, i) x^i, 1:3, i = rep(2, 3))
  mycheck = function(tab) {
    expect_true(is.data.frame(tab))
    expect_equal(tab$id, 1:3)
    expect_true(nrow(tab) == 3)
    expect_true(is(tab$time.submitted, "POSIXt"))
    expect_true(is(tab$time.started, "POSIXt"))
    expect_true(is(tab$time.done, "POSIXt"))
    expect_true(is.numeric(tab$time.running))
    expect_true(is.numeric(tab$memory))
    expect_true(is.numeric(tab$time.queued))
    expect_true(all(is.na(tab$error.msg)))
    expect_true(is.integer(tab$r.pid))
    expect_true(is.integer(tab$seed))
  }
  tab = getJobInfo(reg)
  mycheck(tab)
  submitJobs(reg)
  waitForJobs(reg)
  tab = getJobInfo(reg)
  mycheck(tab)

  tab = getJobInfo(reg, ids = integer(0))
  expect_true(is.data.frame(tab))
  expect_true(nrow(tab) == 0L)

  tab = getJobInfo(reg, pars=TRUE)
  expect_equal(tab$X, 1:3)
  expect_equal(tab$i, rep(2, 3))

  tab = getJobInfo(reg, select = "time.running")
  expect_true(ncol(tab) == 2) # id always selected
  expect_true(names(tab)[1] == "id")
  tab = getJobInfo(reg, select = c("id", "time.running"))
  expect_true(ncol(tab) == 2)

  expect_error(getJobInfo(reg, select = "fooo"), "subset")
})


# FIXME: for some reason this test produced mem = NA, but only in R CMD check

# test_that("getJobInfo: memory correctly reported", {
#   if (!isWindows() && isExpensiveExampleOk()) {
#     reg = makeTestRegistry()
#     conf = getConfig()
#     on.exit(setConfig(conf = conf))
#     setConfig(cluster.functions = makeClusterFunctionsLocal())
#     ids = batchMap(reg, function(n) { x = 1:(10^n); x + rev(x) }, n = c(1, 6))
#     submitJobs(reg, ids)
#     waitForJobs(reg, ids)
#     mem = getJobInfo(reg, select = "memory")$memory
#     expect_true(testNumeric(mem, any.missing = FALSE))
#   }
# })
