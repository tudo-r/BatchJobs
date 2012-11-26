context("getJobInfo")

test_that("getJobInfo", {
  reg = makeTestRegistry()
  batchMap(reg,  function(x, i) x^i, 1:3, i = rep(2, 3))
  submitJobs(reg)
  tab = getJobInfo(reg)
  expect_true(is.data.frame(tab))
  expect_equal(tab$job.id, 1:3)
  expect_true(nrow(tab) == 3)
  expect_true(is(tab$time.submitted, "POSIXt"))
  expect_true(is(tab$time.started, "POSIXt"))
  expect_true(is(tab$time.done, "POSIXt"))
  expect_true(is.character(tab$time.queued))
  expect_true(is.character(tab$time.running))
  expect_true(all(is.na(tab$error.msg)))
  expect_true(is.integer(tab$r.pid))
  expect_true(is.integer(tab$seed))

  tab = getJobInfo(reg, ids = integer(0))
  expect_true(is.data.frame(tab))
  expect_true(nrow(tab) == 0L)

  tab = getJobInfo(reg, parameters=TRUE)
  expect_equal(tab$job.par.X, 1:3)
  expect_equal(tab$job.par.i, rep(2, 3))

  tab = getJobInfo(reg, select = "time.running")
  expect_true(ncol(tab) == 2) # job.id always selected
  tab = getJobInfo(reg, select = c("job.id", "time.running"))
  expect_true(ncol(tab) == 2)

  tab = getJobInfo(reg, select = "fooo")
  expect_true(ncol(tab) == 1 && nrow(tab) == 3)
})

