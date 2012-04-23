context("submitJobs")


test_that("loadResults", {
  reg = makeTestRegistry()
  ids = 1:2
  batchMap(reg, identity, ids)
  submitJobs(reg)
  ys1 = 1:2
  ys2 = loadResults(reg, simplify=TRUE, use.names=FALSE)
  expect_equal(ys1, ys2)
  ys1 = as.list(ys1)
  ys2 = loadResults(reg, simplify=FALSE, use.names=FALSE)
  expect_equal(ys1, ys2)
  names(ys1) = ids
  ys2 = loadResults(reg, simplify=FALSE, use.names=TRUE)
  expect_equal(ys1, ys2)
  ys1 = unlist(ys1)
  ys2 = loadResults(reg, simplify=TRUE, use.names=TRUE)
  expect_equal(ys1, ys2)

  ys2 = loadResults(reg, 2)
  expect_equal(list("2"=2), ys2)

  nl = list()
  names(nl) = character(0L)
  expect_equal(loadResults(reg, ids=integer(0), simplify=TRUE, use.names=TRUE), nl)
  expect_equal(loadResults(reg, ids=integer(0), simplify=FALSE, use.names=TRUE), nl)
  expect_equal(loadResults(reg, ids=integer(0), simplify=TRUE, use.names=FALSE), list())
  expect_equal(loadResults(reg, ids=integer(0), simplify=FALSE, use.names=FALSE), list())
})

