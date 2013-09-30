context("batchMap")

test_that("batchMap", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:3)
  submitJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, (1:3)^2, check.attributes = FALSE)

  reg = makeTestRegistry()
  batchMap(reg, function(x,y,z) (x+y)*z, 1:3, 4:6, more.args=list(z=2))
  submitJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, ((1:3) + (4:6))*2, check.attributes = FALSE)

  reg = makeTestRegistry()
  expect_equal(batchMap(reg, function(...) 1), integer(0L))
  expect_equal(batchMap(reg, function(...) 1, i = integer(0L)), integer(0L))

})

test_that("batchMap non atomic", {
  # factors
  reg = makeTestRegistry()
  ids = batchMap(reg, identity, factor(letters[1:5]))
  submitJobs(reg)
  expect_equal(loadResults(reg, simplify=TRUE), setNames(factor(letters[1:5]), 1:5))
})
