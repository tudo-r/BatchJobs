context("resetJobs")

test_that("resetJobs", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:3)
  ids = getJobIds(reg)
  submitJobs(reg)
  waitForJobs(reg)
  done = findDone(reg)

  # test that nothing happens on empty id vector
  resetJobs(reg)
  resetJobs(reg, ids=integer(0L), force=TRUE)
  expect_equal(done, findDone(reg))

  # now really reset the first job
  resetJobs(reg, done[1], force=TRUE)
  expect_equal(setdiff(done, findDone(reg)), 1)
})
