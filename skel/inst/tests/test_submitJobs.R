context("submitJobs")

test_that("submitJobs", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 123)
  submitJobs(reg)
  y = loadResult(reg, 1L)
  expect_equal(y, 123)

  # check conversion
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:2)
  submitJobs(reg)
  submitJobs(reg, 1:2)
  submitJobs(reg, c(1,2))
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

test_that("submitJobs works with chunking", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:5)
  ch = chunk(getJobIds(reg), chunk.size=2)
  submitJobs(reg, ids=ch[1:2])
  expect_equal(findDone(reg), 1:4)
  submitJobs(reg, ids=ch)
  expect_equal(findDone(reg), 1:5)
  expect_equal(loadResults(reg, simplify=TRUE, use.names=FALSE), 1:5)
})

