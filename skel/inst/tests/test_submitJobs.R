context("submitJobs")

# FIXME check with chunked jobs
test_that("submitJobs", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 123)
  submitJobs(reg)
  y = loadResult(reg, 1L)
  expect_equal(y, 123)
})

test_that("submitJobs works with empty id vector", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:10)
  submitJobs(reg, ids = integer(0L))
  expect_equal(findSubmitted(reg), integer(0L))
})

test_that("submitJobs works with multiple result files", {
  reg = makeTestRegistry(multiple.result.files=TRUE)
  # no list returned
  batchMap(reg, identity, 1)
  submitJobs(reg)
  expect_equal(findErrors(reg), 1L)
  reg = makeTestRegistry(multiple.result.files=TRUE)
  f = function(x) list(a=x, b=2*x)
  ids = 1:2
  batchMap(reg, f, ids)
  submitJobs(reg)
  expect_equal(findDone(reg), ids)
  ys = loadResults(reg, ids)
  expect_equal(ys, list("1"=list(a=1, b=2), "2"=list(a=2, b=4)))
  ys = loadResults(reg, 2, part="b")
  expect_equal(ys, list("2"=list(b=4)))
})
