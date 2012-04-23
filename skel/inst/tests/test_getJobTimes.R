context("getJobTimes")

test_that("getJobTimes", {
  reg = makeTestRegistry()
  f = function(i) {
    if (i == 2)
      stop(123)
  }
  batchMap(reg, f, 1:3)
  tt = getJobTimes(reg)
  expect_equal(names(tt), c("1", "2", "3"))
  expect_equal(as.logical(is.na(tt)), c(TRUE, TRUE, TRUE))

  submitJobs(reg)
  tt = getJobTimes(reg)
  expect_equal(names(tt), c("1", "2", "3"))
  expect_equal(as.logical(is.na(tt)), c(FALSE, TRUE, FALSE))
  expect_true(tt[1] >= 0 && tt[1] <= 3)
  expect_true(tt[3] >= 0 && tt[3] <= 3)

  tt = getJobTimes(reg, use.names=FALSE)
  expect_equal(names(tt), NULL)
  expect_equal(as.logical(is.na(tt)), c(FALSE, TRUE, FALSE))
  expect_true(tt[1] >= 0 && tt[1] <= 3)
  expect_true(tt[3] >= 0 && tt[3] <= 3)

  expect_true(tt >= 0 && tt <= 3)

  tt = getJobTimes(reg, ids = integer(0L), use.names=FALSE)
  expect_equal(tt, integer(0L))
})
