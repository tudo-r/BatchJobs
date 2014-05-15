context("findStatus")

test_that("findStatus", {
  reg = makeTestRegistry()
  f = function(i) {
    if (i == 2)
      stop(123)
  }
  batchMap(reg, f, 1:3)
  ids = getJobIds(reg)
  submitJobs(reg)
  waitForJobs(reg)
  j = findDone(reg)
  expect_equal(j, ids[-2])
  j = findErrors(reg)
  expect_equal(j, ids[2])
  j = findNotDone(reg)
  expect_equal(j, ids[2])
  j = findStarted(reg)
  expect_equal(j, 1:3)
})
