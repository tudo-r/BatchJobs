context("setJobFunction")

test_that("setJobFunction", {
  f = function(x) if(x == 2) stop(2) else x
  reg = makeTestRegistry()
  batchMap(reg, f, x = 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(findErrors(reg), 2L)
  expect_equal(findDone(reg), c(1L, 3L))

  f = function(x) x
  setJobFunction(reg, fun=f, ids=2L, reset=TRUE, force=TRUE)
  submitJobs(reg, 2L)
  waitForJobs(reg)
  expect_equal(findDone(reg), 1:3)
})
