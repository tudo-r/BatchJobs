context("batchMap")

test_that("batchMap", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, (1:3)^2, check.attributes = FALSE)

  reg = makeTestRegistry()
  batchMap(reg, function(x,y,z) (x+y)*z, 1:3, 4:6, more.args = list(z = 2))
  submitJobs(reg)
  waitForJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, ((1:3) + (4:6))*2, check.attributes = FALSE)

  reg = makeTestRegistry()
  expect_equal(batchMap(reg, function(...) 1), integer(0L))
  expect_equal(batchMap(reg, function(...) 1, i = integer(0L)), integer(0L))

  reg = makeTestRegistry()
  expect_equal(batchMap(reg, function(...) 1, a = 1:3, b = 1:3, use.names = TRUE), 1:3)

  # factors
  reg = makeTestRegistry()
  ids = batchMap(reg, identity, factor(letters[1:5]))
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(loadResults(reg, simplify = TRUE), setNames(factor(letters[1:5]), 1:5))

  # primitive functions
  reg = makeTestRegistry()
  ids = batchMap(reg, sin, 1:2)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(as.numeric(loadResults(reg, simplify = TRUE)), sin(1:2))
})

