context("loadResults")

test_that("loadResults", {
  reg = makeTestRegistry()
  ids = 1:2
  batchMap(reg, identity, ids)
  submitJobs(reg)
  waitForJobs(reg)
  ys1 = 1:2
  ys2 = loadResults(reg, simplify=TRUE, use.names="none")
  expect_equal(ys1, ys2)
  ys1 = as.list(ys1)
  ys2 = loadResults(reg, simplify=FALSE, use.names="none")
  expect_equal(ys1, ys2)
  names(ys1) = ids
  ys2 = loadResults(reg, simplify=FALSE, use.names="ids")
  expect_equal(ys1, ys2)
  ys1 = unlist(ys1)
  ys2 = loadResults(reg, simplify=TRUE, use.names="ids")
  expect_equal(ys1, ys2)

  ys2 = loadResults(reg, 2)
  expect_equal(list("2"=2), ys2)

  nl = list()
  names(nl) = character(0L)
  expect_equal(loadResults(reg, ids=integer(0), simplify=TRUE, use.names="ids"), nl)
  expect_equal(loadResults(reg, ids=integer(0), simplify=FALSE, use.names="ids"), nl)
  expect_equal(loadResults(reg, ids=integer(0), simplify=TRUE, use.names="none"), list())
  expect_equal(loadResults(reg, ids=integer(0), simplify=FALSE, use.names="none"), list())

  # test names of loadResults
  reg = makeTestRegistry()
  batchMap(reg, identity, letters, use.names=TRUE)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(names(loadResults(reg, use.names = "ids")), as.character(1:26))
  expect_equal(names(loadResults(reg, use.names = "names")), letters)
})
