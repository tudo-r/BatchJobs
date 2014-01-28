context("testJob")

test_that("testJob", {
  reg = makeTestRegistry()
  id = 1L
  batchMap(reg, identity, 1)
  res = suppressAll(testJob(reg, id, external=FALSE))
  expect_equal(res, 1)
  ids = findNotDone(reg)
  expect_equal(ids, id)
  expect_output({
    st = showStatus(reg)
  }, "Status for 1 jobs")
  expect_equal(st$submitted, 0)
  expect_equal(st$started, 0)
  expect_equal(st$done, 0)
  expect_equal(st$error, 0)

  reg = makeTestRegistry()
  f = function(i) {library(xxxxx);1}
  batchMap(reg, f, 1)
  expect_error(testJob(reg, id, external=FALSE), "xxxxx")
  ids = findNotDone(reg)
  expect_equal(ids, id)
  expect_output({
    df = showStatus(reg)
  }, "Status for 1 jobs")
  expect_equal(st$submitted, 0)
  expect_equal(st$started, 0)
  expect_equal(st$done, 0)
  expect_equal(st$error, 0)
})
