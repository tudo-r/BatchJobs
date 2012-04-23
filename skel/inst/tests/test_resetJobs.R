context("resetJobs")

test_that("resetJobs", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:3)
  ids = getJobIds(reg)
  submitJobs(reg)
  done = findDone(reg)
  # cf unit tests don't support listing or killing
  # -> expect an error here
  expect_error(resetJobs(reg, done[1]))

  # test that nothing happens on empty id vector
  resetJobs(reg)
  resetJobs(reg, ids=integer(0L), force=TRUE)
  expect_equal(done, findDone(reg))

  # now really reset the first job
  resetJobs(reg, done[1], force=TRUE)
  expect_equal(setdiff(done, findDone(reg)), 1)
})
