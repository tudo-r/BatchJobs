context("findJobs")

test_that("findJobs", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 10:13)
  ids = findJobs(reg, pars=.arg1 > 10)
  expect_equal(ids, 2:4)

  reg = makeTestRegistry()
  batchMap(reg, function(x) x, x=10:13)
  ids = findJobs(reg, pars=x > 10)
  expect_equal(ids, 2:4)

  reg = makeTestRegistry()
  batchExpandGrid(reg, function(x,y) x*y, x=1:2, y=10:11)
  ids = findJobs(reg, pars=x == 2)
  expect_true(length(ids) == 2)
  ids = findJobs(reg, pars=x == 1 && y == 11)
  expect_true(length(ids) == 1)

  reg = makeTestRegistry()
  xi = 3
  batchExpandGrid(reg, function(x, y) x+y, x=1:4, y=1:4)
  expect_equal(findJobs(reg, pars = (x == y)), c(1, 6, 11, 16))
  expect_equal(findJobs(reg, pars = (x == xi)), c(3, 7, 11, 15))
})
